package skaka.auth

import java.nio.charset.StandardCharsets
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

/** skaka の API トークン（skaka_ で始まる）. */
object ApiKeys extends sbirds.auth.ApiKeys("skaka_")

/**
 * skaka の秘密の値の暗号化（鍵は設定の秘密の値の SHA-256. salt はなし. 変えると保存済みの値を復号できなくなる）.
 * DB には 12バイトの IV + 暗号文のバイト列で保存する（sbirds-lib の "v1:" + Base64 の文字列と中身は同じ）.
 */
final class SecretBox(secret: String) {
  private val inner = new sbirds.auth.SecretBox(secret, "")

  def seal(plain: String): Array[Byte] = Base64.getDecoder.decode(inner.encrypt(plain).stripPrefix("v1:"))

  /** 鍵が違う・壊れているときは None. */
  def open(box: Array[Byte]): Option[String] = inner.decrypt("v1:" + Base64.getEncoder.encodeToString(box))
}

/** TURN の一時的な認証情報（coturn の use-auth-secret. HMAC-SHA1 の Base64）. */
object TurnCredentials {
  def password(secret: String, user: String): String = {
    val mac = Mac.getInstance("HmacSHA1")
    mac.init(new SecretKeySpec(secret.getBytes(StandardCharsets.UTF_8), "HmacSHA1"))
    Base64.getEncoder.encodeToString(mac.doFinal(user.getBytes(StandardCharsets.UTF_8)))
  }
}
