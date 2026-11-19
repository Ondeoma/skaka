package skaka.api

import cats.syntax.all.*
import io.circe.{ACursor, Json}
import io.circe.syntax.*
import skaka.shared.*

import java.util.UUID

/** 予定（skuaka の EventDto）から、変更の要求（EventReq）を作る. 知らない項目はそのまま渡す. */
object SkuakaEvents {
  def toReq(
    e: Json,
    location: String
  ): Json = {
    val c = e.hcursor
    def f(
      k: String
    ): Json = c.downField(k).focus.getOrElse(Json.Null)
    Json.obj(
      "calendarId" -> f("calendarId"),
      "title" -> f("title"),
      "description" -> f("description"),
      "location" -> location.asJson,
      "allDay" -> f("allDay"),
      "start" -> f("start"),
      "end" -> f("end"),
      "timezone" -> f("timezone"),
      "color" -> f("color"),
      "recurrence" -> f("recurrence"),
      "reminders" -> f("reminders"),
      "version" -> f("version"),
      "attendees" -> c.downField(
        "attendees"
      ).as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.downField("user").get[UUID]("id").toOption).asJson
    )
  }

  def roomEvent(
    e: ACursor
  ): Option[RoomEventDto] =
    (
      e.get[UUID]("id"),
      e.get[String]("title"),
      e.get[String]("start"),
      e.get[String]("end"),
      e.get[Boolean]("allDay")
    ).mapN(RoomEventDto.apply).toOption
}
