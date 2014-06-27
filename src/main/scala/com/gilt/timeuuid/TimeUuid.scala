package com.gilt.timeuuid

import java.util.{UUID, Random}
import java.lang.ThreadLocal

//Based on http://www.ietf.org/rfc/rfc4122.txt & Datastax/Cassandra/Astyanax timeUUID generation
object TimeUuid {

  def apply(): UUID = new UUID(buildTime(Clock.time()), buildClockSeqAndNode())

  def apply(timeInMillis: Long): UUID = new UUID(buildTime(convertToNanos(timeInMillis)),  buildClockSeqAndNode())

  private def convertToNanos(timeInMillis: Long): Long = (timeInMillis - Clock.StartEpoch) * 10000

  private def buildTime(time: Long): Long = {
    var msb: Long = 0L
    msb |= (0x00000000ffffffffL & time) << 32
    msb |= (0x0000ffff00000000L & time) >>> 16
    msb |= (0x0fff000000000000L & time) >>> 48
    msb |= 0x0000000000001000L //Version 1 Uuid
    msb
  }

  private def buildClockSeqAndNode(): Long = {
    var lsb: Long = 0
    lsb |= (nextClockSeq & 0x000000000000003FL) << 56
    lsb |= (nextRandomLong & 0x00000000000000FFL) << 48
    lsb |= 0x8000000000000000L // variant (2 bits)
    lsb |= Node.id // 6 bytes
    lsb
  }

  private val tRand = new ThreadLocal[Random]
  private def nextRandomLong():Long = {
    var rand = Option(tRand.get) getOrElse {
      val r = new Random()
      tRand.set(r)
      r
    }
    rand.nextLong
  }

  private val tClockSeq = new ThreadLocal[Int]
  private def nextClockSeq():Long = {
    val oldVal = Option(tClockSeq.get) getOrElse 0
    tClockSeq.set(oldVal + 1)
    oldVal
  }
}

