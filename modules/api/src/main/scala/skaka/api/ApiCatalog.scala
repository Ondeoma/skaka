package skaka.api

/** REST API の一覧（MCP の list_api_endpoints と GET /api/endpoints）. */
object ApiCatalog {
  final case class Endpoint(
    method: String,
    path: String,
    description: String)
  val all: List[Endpoint] = List(
    Endpoint("GET", "/api/me", "自分の情報（名前・メール・タイムゾーン）"),
    Endpoint("PATCH", "/api/me", "自分の名前・タイムゾーンを変える {name?, timezone?}"),
    Endpoint("GET", "/api/users?q=", "ユーザーを名前・メールで探す"),
    Endpoint("GET", "/api/rooms", "自分の会議室と最近入った会議室（live は今入っている人数）"),
    Endpoint("POST", "/api/rooms", "会議室を作る {name, description, guestAccess, lobby}"),
    Endpoint("GET", "/api/rooms/{code}", "会議室の情報"),
    Endpoint("PUT", "/api/rooms/{code}", "会議室を変える（持ち主） {name, description, guestAccess, lobby}"),
    Endpoint("DELETE", "/api/rooms/{code}", "会議室を削除する（持ち主. 自分の会議室は削除できない）"),
    Endpoint("DELETE", "/api/rooms/{code}/recent", "最近の会議室から外す"),
    Endpoint("GET", "/api/rooms/{code}/live", "いま入っている人と待機室の人数"),
    Endpoint("GET", "/api/meetings?room=&before=&limit=", "会議の履歴（自分の会議室の会議と、参加した会議. 新しい順）"),
    Endpoint("GET", "/api/meetings/{meetingId}", "会議の参加者・文字起こし・チャット・録画・議事録"),
    Endpoint("GET", "/api/recordings?room=&meeting=&limit=", "会議の録画（保存先は skawau. fileUrl で再生できる）"),
    Endpoint("GET", "/api/recordings/{recordingId}", "録画の状態"),
    Endpoint("POST", "/api/recordings/{recordingId}/retry", "保存できなかった録画を、もう一度 skawau に置く（録画した人）"),
    Endpoint("DELETE", "/api/recordings/{recordingId}", "録画を消す（録画した人・会議室の持ち主. skawau のファイルは残る）"),
    Endpoint("GET", "/api/skawau", "skawau（ファイル共有. 録画の保存先）との連携の状態"),
    Endpoint("GET", "/api/skuaka", "skuaka（カレンダー）との連携の状態"),
    Endpoint("GET", "/api/skuaka/events?from=yyyy-MM-dd&to=yyyy-MM-dd&tz=", "skuaka の自分の予定（room はその予定の会議室のコード）"),
    Endpoint("POST", "/api/skuaka/events/{eventId}/room", "skuaka の予定に会議室を付ける（場所に会議室の URL を入れる）"),
    Endpoint(
      "POST",
      "/api/skuaka/schedule",
      "会議の予定を立てる {title, start, end（UTC の ISO 8601）, description, attendees: [メール], guestAccess, lobby}"
    )
  )
}
