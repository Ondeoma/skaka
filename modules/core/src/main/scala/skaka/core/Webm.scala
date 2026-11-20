package skaka.core

import java.nio.ByteBuffer

/**
 * WebM（EBML）の先頭を直す. ブラウザの MediaRecorder は録りながら書くため、Segment の Info に長さ（Duration）を書かない.
 * 長さがないと、再生するときにシークバーが使えない（長さが分からない）. 録り終えてから、Info に Duration を足す.
 * Cluster の位置を指す SeekHead・Cues がある WebM は、足すと位置がずれるため直さない（ブラウザの録画には付かない）.
 */
object Webm {
  private val EbmlId = 0x1a45dfa3L
  private val SegmentId = 0x18538067L
  private val InfoId = 0x1549a966L
  private val SeekHeadId = 0x114d9b74L
  private val CuesId = 0x1c53bb6bL
  private val ClusterId = 0x1f43b675L
  private val DurationId = 0x4489L
  private val ScaleId = 0x2ad7b1L

  /** 要素の id（先頭の印のビットを含む）と長さ. */
  private def readId(b: Array[Byte], at: Int): Option[(Long, Int)] =
    if at < 0 || at >= b.length then None
    else {
      val first = b(at) & 0xff
      val len = Integer.numberOfLeadingZeros(first) - 23
      if len < 1 || len > 4 || at + len > b.length then None
      else
        Some((0 until len).foldLeft(0L)((v, i) => (v << 8) | (b(at + i) & 0xff)) -> len)
    }

  /** 要素の大きさ（印のビットを除く. すべて 1 は「不明」で None）と長さ. */
  private def readSize(b: Array[Byte], at: Int): Option[(Option[Long], Int)] =
    if at < 0 || at >= b.length then None
    else {
      val first = b(at) & 0xff
      val len = Integer.numberOfLeadingZeros(first) - 23
      if len < 1 || len > 8 || at + len > b.length then None
      else {
        val v = (1 until len).foldLeft((first & (0xff >> len)).toLong)((v, i) => (v << 8) | (b(at + i) & 0xff))
        val unknown = v == (1L << (7 * len)) - 1
        Some((if unknown then None else Some(v)) -> len)
      }
    }

  /** 大きさを len バイトで書く（入らなければ None）. */
  private def writeSize(v: Long, len: Int): Option[Array[Byte]] =
    if len < 1 || len > 8 || v >= (1L << (7 * len)) - 1 then None
    else
      Some(Array.tabulate(len)(i => ((v >>> (8 * (len - 1 - i))) & 0xff).toByte).updated(
        0,
        (((v >>> (8 * (len - 1))) & 0xff) | (0x80 >> (len - 1))).toByte
      ))

  /** 大きさを、元と同じ長さ（入らなければ最小の長さ）で書く. */
  private def resize(v: Long, len: Int): Array[Byte] =
    writeSize(v, len).orElse((1 to 8).iterator.flatMap(writeSize(v, _)).nextOption()).get

  private def uint(b: Array[Byte], at: Int, len: Int): Long = (0 until len).foldLeft(0L)((v, i) =>
    (v << 8) | (b(at + i) & 0xff)
  )

  /**
   * head（ファイルの先頭. Info を丸ごと含む長さ）に長さ（ミリ秒）を書いたもの. 直せない・直す必要がなければ None.
   * 返したものを head の代わりに使い、続きはそのままつなぐ.
   */
  def withDuration(head: Array[Byte], ms: Double): Option[Array[Byte]] =
    if ms <= 0 then None
    else
      for {
        (id0, l0) <- readId(head, 0) if id0 == EbmlId
        (s0, sl0) <- readSize(head, l0)
        // 大きさは利用者が送った中身のまま. head に収まらないものは読まない（Int に直すと負になり、位置が戻る）
        ebmlLen <- s0 if ebmlLen <= head.length
        segAt = l0 + sl0 + ebmlLen.toInt
        (sid, sl) <- readId(head, segAt) if sid == SegmentId
        segSizeAt = segAt + sl
        (segSize, ssl) <- readSize(head, segSizeAt)
        out <- patchSegment(head, segSizeAt, segSize, ssl, segSizeAt + ssl, ms)
      } yield out

  /** Segment の子を順に見て、Info に Duration を足す. */
  private def patchSegment(b: Array[Byte],
                           segSizeAt: Int,
                           segSize: Option[Long],
                           ssl: Int,
                           start: Int,
                           ms: Double): Option[Array[Byte]] = {
    @annotation.tailrec
    def loop(at: Int): Option[Array[Byte]] =
      readId(b, at) match {
        case None => None
        case Some((id, il)) =>
          readSize(b, at + il) match {
            case Some((Some(size), sl)) if id == InfoId =>
              val dataAt = at + il + sl
              if dataAt + size > b.length then None
              else patchInfo(b, segSizeAt, segSize, ssl, at + il, sl, dataAt, size.toInt, ms)
            case _ if id == SeekHeadId || id == CuesId || id == ClusterId => None
            // 次の要素は必ず先へ進む（大きさを Int に直して負になると、同じ位置を読み続けて終わらない）
            case Some((Some(size), sl)) if size <= b.length - at => loop(at + il + sl + size.toInt)
            case _ => None
          }
      }
    loop(start)
  }

  private def patchInfo(b: Array[Byte],
                        segSizeAt: Int,
                        segSize: Option[Long],
                        ssl: Int,
                        infoSizeAt: Int,
                        isl: Int,
                        dataAt: Int,
                        size: Int,
                        ms: Double): Option[Array[Byte]] = {
    // Info の子: 時間の単位（既定は 1ms = 1,000,000ns）と、既にある Duration
    @annotation.tailrec
    def children(at: Int, acc: List[(Long, Int, Int)]): Option[List[(Long, Int, Int)]] = // (id, データの位置, 大きさ)
      if at == dataAt + size then Some(acc.reverse)
      else
        (
          readId(b, at),
          readId(b, at).flatMap((_, il) => readSize(b, at + il))
        ) match {
          case (Some((id, il)), Some((Some(s), sl))) if at + il + sl + s <= dataAt + size =>
            children(at + il + sl + s.toInt, (id, at + il + sl, s.toInt) :: acc)
          case _ => None
        }
    children(dataAt, Nil).flatMap { cs =>
      val scale =
        cs.collectFirst { case (ScaleId, at, len) if len <= 8 => uint(b, at, len) }.filter(_ > 0).getOrElse(1000000L)
      val value = ms * 1e6 / scale
      cs.find(_._1 == DurationId) match {
        case Some((_, at, 8)) => Some(b.clone().tap(c => ByteBuffer.wrap(c, at, 8).putDouble(value)))
        case Some((_, at, 4)) => Some(b.clone().tap(c => ByteBuffer.wrap(c, at, 4).putFloat(value.toFloat)))
        case Some(_) => None
        case None =>
          val dur = Array[Byte](0x44, 0x89.toByte, 0x88.toByte) ++ ByteBuffer.allocate(8).putDouble(value).array()
          val infoSize = resize(size.toLong + dur.length, isl)
          val delta = dur.length + infoSize.length - isl
          // Segment の大きさが分かっていれば、増えた分を足す（ブラウザの録画では「不明」）
          val segBytes = segSize.fold(b.slice(segSizeAt, segSizeAt + ssl))(s => resize(s + delta, ssl))
          Some(b.slice(0, segSizeAt) ++ segBytes ++ b.slice(segSizeAt + ssl, infoSizeAt) ++ infoSize ++
            b.slice(dataAt, dataAt + size) ++ dur ++
            b.slice(dataAt + size, b.length))
      }
    }
  }

  extension [A](a: A) {
    private def tap(f: A => Unit): A = { f(a); a }
  }
}
