package skaka.api

/**
 * skakapo（AI エージェント）の設定. requestType は議事録を頼むときの依頼の種類（項目 title・description を持つもの. 既定は「その他の依頼」）.
 */
final case class SkakapoConfig(
  url: String,
  internalUrl: String,
  requestType: String = "other")
