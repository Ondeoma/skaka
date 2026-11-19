package skaka.core

import java.nio.ByteBuffer

class WebmSpec extends munit.FunSuite {

  private def bytes(
    xs: Int*
  ): Array[Byte] = xs.map(_.toByte).toArray

  /** 要素（大きさは1バイト）. */
  private def el(
    id: Array[Byte],
    data: Array[Byte]
  ): Array[Byte] = id ++ bytes(0x80 | data.length) ++ data

  private val ebml = el(bytes(0x1a, 0x45, 0xdf, 0xa3), el(bytes(0x42, 0x82), "webm".getBytes))
  private val scale = el(bytes(0x2a, 0xd7, 0xb1), bytes(0x0f, 0x42, 0x40)) // 1,000,000
  private val app = el(bytes(0x4d, 0x80), "Chrome".getBytes)
  private val info = el(bytes(0x15, 0x49, 0xa9, 0x66), scale ++ app)
  private val tracks = el(bytes(0x16, 0x54, 0xae, 0x6b), bytes(0xae, 0x80))
  private val cluster = bytes(0x1f, 0x43, 0xb6, 0x75, 0x01, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xe7, 0x81, 0x00)
  private val unknown = bytes(0x01, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff)

  private def file(
    segSize: Array[Byte],
    children: Array[Byte]
  ) = ebml ++ bytes(0x18, 0x53, 0x80, 0x67) ++ segSize ++ children

  /** Duration の値（8バイトの浮動小数）. */
  private def duration(
    b: Array[Byte]
  ): Option[Double] =
    b.indices.find(i => i + 3 <= b.length && b(i) == 0x44 && b(i + 1) == 0x89.toByte && b(i + 2) == 0x88.toByte)
      .map(i => ByteBuffer.wrap(b, i + 3, 8).getDouble)

  test("ブラウザの録画（Segment の大きさが不明）: Info に Duration を足し、Info の大きさを直す") {
    val f = file(unknown, info ++ tracks ++ cluster)
    val out = Webm.withDuration(f, 83500).get
    assertEquals(out.length, f.length + 11)
    assertEquals(duration(out), Some(83500.0))
    // Info の大きさが 11 増え、続き（Tracks・Cluster）はそのまま
    val infoAt = ebml.length + 4 + 8
    assertEquals(out(infoAt + 4) & 0x7f, (info.length - 5) + 11)
    assert(out.endsWith(tracks ++ cluster))
  }

  test("Segment の大きさが分かっていれば、増えた分を足す") {
    val kids = info ++ tracks
    val f = file(bytes(0x40, kids.length), kids)
    val out = Webm.withDuration(f, 1000).get
    assertEquals(out(ebml.length + 5) & 0xff, kids.length + 11)
  }

  test("既に Duration があれば値だけ書き換える. 時間の単位も考える") {
    val withDur = el(
      bytes(0x15, 0x49, 0xa9, 0x66),
      el(bytes(0x2a, 0xd7, 0xb1), bytes(0x0f, 0x42, 0x40 * 2 / 2)) ++
        bytes(0x44, 0x89, 0x88) ++ ByteBuffer.allocate(8).putDouble(0).array()
    )
    val f = file(unknown, withDur ++ cluster)
    val out = Webm.withDuration(f, 2500).get
    assertEquals(out.length, f.length)
    assertEquals(duration(out), Some(2500.0))
  }

  test("直せないものは触らない（SeekHead がある・WebM でない・途中で切れている）") {
    val seek = el(bytes(0x11, 0x4d, 0x9b, 0x74), bytes(0xec, 0x80))
    assertEquals(Webm.withDuration(file(unknown, seek ++ info ++ cluster), 1000), None)
    assertEquals(Webm.withDuration("not a webm file".getBytes, 1000), None)
    assertEquals(Webm.withDuration(file(unknown, info).dropRight(3), 1000), None)
    assertEquals(Webm.withDuration(file(unknown, info ++ cluster), 0), None)
  }
}
