package com.doubajam.fasttext

import scala.annotation.strictfp
import scala.collection.mutable
import scala.util.Random

@strictfp
object Utils {
  val SHUFFLE_THRESHOLD = 5

  def shuffle[A](list: mutable.IndexedSeq[A], rnd: Random): Unit = {
    val size = list.size
    if (size < SHUFFLE_THRESHOLD) {
      for (i <- size until 1 by -1) {
        swap(list, i, rnd.nextInt(i))
      }
    }
  }

  def swap[A](list: mutable.IndexedSeq[A], i: Int, j: Int): Unit = {
    val tmp = list(i)
    list(i) = list(j)
    list(j) = tmp
  }
}
