package skaka.db

import sbirds.db.*
import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*

import java.time.Instant
import java.util.UUID

/** skawau（ファイル共有）との連携: 暗号化したトークンと、許可の画面から戻るまでの状態. */
final class SkawauStore(xa: Transactor[IO]) extends TxStore(xa) {

  def token(userId: UUID): IO[Option[(Array[Byte], Instant)]] =
    tx(sql"select token_enc, created_at from skawau_link where user_id = $userId".query[(Array[Byte], Instant)].option)

  def saveToken(userId: UUID, enc: Array[Byte]): IO[Unit] = tx(
    sql"""insert into skawau_link (user_id, token_enc) values ($userId, $enc)
          on conflict (user_id) do update set token_enc = excluded.token_enc, created_at = now()""".update.run.void
  )

  def deleteToken(userId: UUID): IO[Unit] = tx(sql"delete from skawau_link where user_id = $userId".update.run.void)

  def saveState(state: String, userId: UUID, verifier: String, returnTo: String, expires: Instant): IO[Unit] = tx(
    sql"delete from oauth_state where expires_at < now()".update.run *>
      sql"""insert into oauth_state (state, user_id, verifier, return_to, expires_at)
            values ($state, $userId, $verifier, $returnTo, $expires)""".update.run.void
  )

  /** 状態を使う（期限内のものを1回だけ）. (ユーザー, verifier, 戻り先). */
  def takeState(state: String): IO[Option[(UUID, String, String)]] =
    tx(sql"delete from oauth_state where state = $state and expires_at > now() returning user_id, verifier, return_to".query[
      (UUID, String, String)
    ].option)
}
