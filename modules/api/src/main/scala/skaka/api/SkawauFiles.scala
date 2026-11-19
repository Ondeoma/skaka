package skaka.api

import cats.data.EitherT
import cats.effect.*
import cats.syntax.all.*
import fs2.Stream
import io.circe.Json
import io.circe.syntax.*
import org.http4s.*
import skaka.auth.SecretBox
import skaka.db.*

import java.util.UUID

/**
 * skawau にファイルを置く（録画・議事録で共通）: 利用者の skawau のトークン、自分のルートの下のフォルダへのアップロード、中身の書き換え、
 * 会議に参加したメンバーとの共有. 失敗は利用者に見せる文言.
 */
final class SkawauFiles(
  val client: SkawauClient,
  links: SkawauStore,
  box: SecretBox,
  emails: (UUID, UUID) => IO[List[String]]) {

  type Res[A] = EitherT[IO, String, A]

  private def sk(
    userId: UUID
  )(
    r: IO[Either[(Int, String), Json]]
  ): Res[Json] =
    EitherT(r.flatMap {
      case Left((401, _)) => links.deleteToken(userId).as(Left("skawau との連携が切れています。設定の画面で連携し直してから、もう一度保存してください"))
      case Left((_, m)) => IO.pure(Left(s"skawau: $m"))
      case Right(j) => IO.pure(Right(j))
    })

  /** 利用者の skawau のトークン. */
  def token(
    userId: UUID
  ): Res[String] =
    EitherT.fromOptionF(links.token(userId), "skawau と連携していません。設定の画面で連携してから、もう一度保存してください")
      .flatMap(enc => EitherT.fromOption[IO](box.open(enc._1), "skawau のトークンを読めません（SECRET_KEY が変わりました）。連携し直してください"))

  /** 自分のルートの下の folder（なければ作る）に置く（同じ名前があれば名前を変える）. 置いたファイルの ID. */
  def put(
    token: String,
    userId: UUID,
    folder: String,
    name: String,
    body: Stream[IO, Byte],
    length: Long,
    mime: String
  ): Res[UUID] =
    for {
      root <- sk(userId)(client.call(token, Method.GET, "/api/files/root"))
      rootId <- EitherT.fromOption[IO](root.hcursor.downField("folder").get[UUID]("id").toOption, "skawau の応答を読めません")
      node <- sk(userId)(client.put(token, rootId, folder, name, body, length, mime))
      fileId <- EitherT.fromOption[IO](node.hcursor.get[UUID]("id").toOption, "skawau の応答を読めません")
    } yield fileId

  /** 置いたファイルの中身を書き換える（skawau に新しい版が残る）. ファイルが消されていれば None. */
  def write(
    token: String,
    userId: UUID,
    fileId: UUID,
    data: Array[Byte],
    mime: String
  ): Res[Option[UUID]] =
    EitherT(client.write(token, fileId, data, mime)).map(_ => Some(fileId)).recoverWith {
      case (404, _) => EitherT.rightT(None)
    }.leftSemiflatMap {
      case (401, _) => links.deleteToken(userId).as("skawau との連携が切れています。設定の画面で連携し直してから、もう一度保存してください")
      case (_, m) => IO.pure(s"skawau: $m")
    }

  def url(
    fileId: UUID
  ): String = s"${client.cfg.url}/view/$fileId"

  /** 会議に参加したメンバーと共有する（閲覧のみ. skawau でメールアドレスが一致する人だけ）. 共有できた人数. */
  def share(
    token: String,
    owner: UUID,
    fileId: UUID,
    meetingId: UUID
  ): IO[Int] =
    emails(meetingId, owner).flatMap {
      case Nil => IO.pure(0)
      case es =>
        client.call(token, Method.POST, "/api/users/lookup", Some(Json.obj("emails" -> es.take(100).asJson))).flatMap {
          case Left(_) => IO.pure(0)
          case Right(j) =>
            j.hcursor.downField(
              "users"
            ).as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.get[UUID]("id").toOption).distinct
              .traverse(uid =>
                client.call(
                  token,
                  Method.POST,
                  s"/api/files/$fileId/shares",
                  Some(Json.obj("kind" -> "user".asJson, "userId" -> uid.asJson, "permission" -> "view".asJson))
                ).map(_.isRight)
              )
              .map(_.count(identity))
        }
    }
}
