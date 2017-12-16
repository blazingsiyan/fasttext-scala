package com.doubajam.fasttext

import java.io.InputStream

import scala.annotation.strictfp
import scala.collection.mutable.ArrayBuffer

@strictfp
class QMatrix(
    val m: Int,
    val n: Int,
    val codes: ArrayBuffer[UInt8],
    val pq: ProductQuantizer,
    val qnorm: Boolean,
    val normCodes: ArrayBuffer[UInt8] = null,
    val npq: ProductQuantizer = null
) {
  val codesize: Int = codes.size

  def addToVector(x: Vector, t: Int): Unit = {
    var norm = 1.0f
    if (qnorm) {
      norm = npq.getCentroid(0, normCodes(t))
    }
    pq.addCode(x, codes, t, norm)
  }

  def dotRow(vec: Vector, i: Int): Float = {
    require(i >= 0)
    require(i < m)
    require(vec.size == n)
    var norm = 1.0f
    if (qnorm) {
      norm = npq.getCentroid(0, normCodes(i))
    }
    pq.mulCode(vec, codes, i, norm)
  }
}

object QMatrix {
  def load(is: InputStream) = {
    val qnorm    = IOUtil.readByte(is) != 0
    val m        = IOUtil.readLong(is).toInt
    val n        = IOUtil.readLong(is).toInt
    val codesize = IOUtil.readInt(is)

    val codes = new ArrayBuffer[UInt8](codesize)
    for (_ <- 0 until codesize) {
      codes += IOUtil.readByte(is) & 0xFF
    }

    val pq = ProductQuantizer.load(is)
    if (qnorm) {
      val normCodes = new ArrayBuffer[UInt8](m)
      for (_ <- 0 until m) {
        normCodes += IOUtil.readByte(is) & 0xFF
      }
      val npq = ProductQuantizer.load(is)
      new QMatrix(m, n, codes, pq, qnorm, normCodes, npq)
    } else {
      new QMatrix(m, n, codes, pq, qnorm)
    }
  }
}
