package skaka.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*

import java.time.Instant
import java.util.UUID

final case class OAuthCode(clientId: String, redirectUri: String, userId: UUID, codeChallenge: String, scope: String)

/** OAuth の認可コードと、認可で発行した API トークン（skuaka など、skaka を使う外部のサービス向け）. */
final class OAuthStore(xa: Transactor[IO]) extends TxStore(xa):

  def createCode(hash: Array[Byte], c: OAuthCode, expires: Instant): IO[Unit] = tx(
    sql"""insert into oauth_code (code_hash, client_id, redirect_uri, user_id, code_challenge, scope, expires_at)
          values ($hash, ${c.clientId}, ${c.redirectUri}, ${c.userId}, ${c.codeChallenge}, ${c.scope}, $expires)""".update.run *>
      sql"delete from oauth_code where expires_at < now() - interval '1 hour'".update.run.void
  )

  /**
   * 認可コードを使う（期限内で、まだ使っていないものだけ）.
   * 使用済みのコードがもう一度使われたら、漏れたものとして、そのコードで発行したトークンを失効させる（RFC 6749 4.1.2）.
   */
  def consumeCode(hash: Array[Byte]): IO[Option[OAuthCode]] = tx(
    sql"""update oauth_code set used_at = now() where code_hash = $hash and used_at is null and expires_at > now()
          returning client_id, redirect_uri, user_id, code_challenge, scope""".query[OAuthCode].option.flatMap {
      case some @ Some(_) => some.pure[ConnectionIO]
      case None =>
        sql"""update api_token t set revoked_at = now() from oauth_code c
              where c.code_hash = $hash and c.used_at is not null and t.user_id = c.user_id and t.oauth_client = c.client_id
                and t.revoked_at is null and t.created_at >= c.used_at - interval '1 minute'""".update.run.as(none[OAuthCode])
    }
  )

  /** そのユーザーにそのサービスへ以前に発行したトークンを失効させ、新しいトークンを登録する. */
  def replaceToken(userId: UUID, clientId: String, name: String, prefix: String, hash: Array[Byte], scope: String): IO[Unit] = tx(
    sql"update api_token set revoked_at = now() where user_id = $userId and oauth_client = $clientId and revoked_at is null".update.run *>
      sql"""insert into api_token (id, user_id, name, prefix, token_hash, scope, oauth_client)
            values (${UUID.randomUUID()}, $userId, $name, $prefix, $hash, $scope, $clientId)""".update.run.void
  )

  /** そのサービスが発行を受けたトークンを失効させる（なければ何もしない）. */
  def revokeToken(hash: Array[Byte], clientId: String): IO[Unit] = tx(
    sql"update api_token set revoked_at = now() where token_hash = $hash and oauth_client = $clientId and revoked_at is null".update.run.void
  )
