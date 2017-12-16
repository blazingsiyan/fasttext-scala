package com.doubajam.fasttext

import java.io.InputStream
import java.util.Random

import scala.annotation.strictfp
import scala.collection.mutable.ArrayBuffer

@strictfp
class ProductQuantizer(
    val dim: Int,
    var nsubq: Int,
    var dsub: Int,
    var lastdsub: Int,
    val centroids: ArrayBuffer[Real]
) {
  import ProductQuantizer._

  val rng = new Random(seed.toLong)

  def getCentroid(m: Int, i: Int): Real = {
    if (m == nsubq - 1)
      centroids(m * ksub * dsub + i * lastdsub)
    else
      centroids((m * ksub + i) * dsub)
  }

  def getCentroidOffset(m: Int, i: Int): Int = {
    if (m == nsubq - 1)
      m * ksub * dsub + i * lastdsub
    else
      (m * ksub + i) * dsub
  }

  def getCentroids(m: Int, i: Int): ArrayBuffer[Real] = {
    if (m == nsubq - 1)
      centroids.slice(m * ksub * dsub + i * lastdsub, centroids.length)
    else
      centroids.slice((m * ksub + i) * dsub, centroids.length)
  }


  def mulCode(x: Vector, codes: ArrayBuffer[UInt8], t: Int, alpha: Float): Float = {
    var res = 0.0f
    var d   = dsub
    for (m <- 0 until nsubq) {
      val offset = getCentroidOffset(m, codes(m + nsubq * t))
      if (m == nsubq - 1) {
        d = lastdsub
      }
      for (n <- 0 until d) {
        res += x(m * dsub + n) * centroids(offset + n)
      }
    }
    res * alpha
  }

  def addCode(x: Vector, codes: ArrayBuffer[UInt8], t: Int, alpha: Float): Unit = {
    var d = dsub
    for (m <- 0 until nsubq) {
      val offset = getCentroidOffset(m, codes(m + nsubq * t))
      if (m == nsubq - 1) {
        d = lastdsub
      }
      for (n <- 0 until d) {
        x(m * dsub + n) += alpha * centroids(offset + n)
      }
    }
  }
}

object ProductQuantizer {
  val nbits                  = 8
  val ksub                   = 1 << nbits
  val max_points_per_cluster = 256
  val max_points             = max_points_per_cluster * ksub
  val seed                   = 1234
  val niter                  = 25
  val eps                    = 1e-7

  def load(is: InputStream): ProductQuantizer = {
    val dim      = IOUtil.readInt(is)
    val nsubq    = IOUtil.readInt(is)
    val dsub     = IOUtil.readInt(is)
    val lastdsub = IOUtil.readInt(is)

    val centroids = new ArrayBuffer[Real](dim * ksub)
    for (_ <- 0 until dim * ksub) {
      centroids.append(IOUtil.readFloat(is))
    }

    new ProductQuantizer(dim, nsubq, dsub, lastdsub, centroids)
  }
}
