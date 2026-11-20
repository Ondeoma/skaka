package skaka.db

import cats.effect.*
import cats.syntax.all.*
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*

import java.util.UUID

/**
 * OAuth の認可コード（sbirds.oauth.OAuthCodeStore）と、認可で発行した API トークン（skuaka など、skaka を使う外部のサービス向け）.
 * 使用済みのコードがもう一度使われたら、漏れたものとして、そのコードで発行したトークンを失効させる（RFC 6749 4.1.2）.
 */
final class OAuthStore(xa: Transactor[IO]) extends sbirds.oauth.OAuthCodeStore(xa, OAuthStore.revokeIssued) {

  /** そのユーザーにそのサービスへ以前に発行したトークンを失効させ、新しいトークンを登録する. */
  def replaceToken(userId: UUID,
                   clientId: String,
                   name: String,
                   prefix: String,
                   hash: Array[Byte],
                   scope: String): IO[Unit] = tx(
    sql"update api_token set revoked_at = now() where user_id = $userId and oauth_client = $clientId and revoked_at is null".update.run *>
      sql"""insert into api_token (id, user_id, name, prefix, token_hash, scope, oauth_client)
            values (${UUID.randomUUID()}, $userId, $name, $prefix, $hash, $scope, $clientId)""".update.run.void
  )

  /** そのサービスが発行を受けたトークンを失効させる（なければ何もしない）. */
  def revokeToken(hash: Array[Byte], clientId: String): IO[Unit] = tx(
    sql"update api_token set revoked_at = now() where token_hash = $hash and oauth_client = $clientId and revoked_at is null".update.run.void
  )
}

object OAuthStore {

  /** 使用済みのコードで発行した API トークンを失効させる（コードを使うのと同じトランザクションで動く）. */
  private def revokeIssued(codeHash: Array[Byte]): ConnectionIO[Unit] =
    sql"""update api_token t set revoked_at = now() from oauth_code c
          where c.code_hash = $codeHash and c.used_at is not null and t.user_id = c.user_id and t.oauth_client = c.client_id
            and t.revoked_at is null and t.created_at >= c.used_at - interval '1 minute'""".update.run.void
}
