package skaka.shared

import io.circe.derivation.Configuration

/**
 * JSON の形（ConfiguredCodec で使う）: 既定値のある項目は省いてもよい. enum は {"type": "名前", ...}.
 */
private[shared] given Configuration = Configuration.default.withDiscriminator("type").withDefaults
