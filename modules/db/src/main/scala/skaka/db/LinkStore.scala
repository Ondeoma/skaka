package skaka.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*

import java.util.UUID

/** API トークンでの連携（table は skuaka_link・skakapo_link. 同じ形の表）. */
final class LinkStore(xa: Transactor[IO], table: String = "skuaka_link") extends TxStore(xa) {
  private val t = Fragment.const(table)

  def get(userId: UUID): IO[Option[TokenLink]] = tx((fr"select token_enc, prefix, account, can_write from" ++ t ++
    fr"where user_id = $userId").query[TokenLink].option)

  def set(userId: UUID, l: TokenLink): IO[Unit] = tx(
    (fr"insert into" ++ t ++
      fr"""(user_id, token_enc, prefix, account, can_write) values ($userId, ${l.tokenEnc}, ${l.prefix}, ${l.account}, ${l.canWrite})
          on conflict (user_id) do update set token_enc = excluded.token_enc, prefix = excluded.prefix, account = excluded.account,
            can_write = excluded.can_write, created_at = now()""").update.run.void
  )

  def delete(userId: UUID): IO[Unit] = tx((fr"delete from" ++ t ++ fr"where user_id = $userId").update.run.void)
}
