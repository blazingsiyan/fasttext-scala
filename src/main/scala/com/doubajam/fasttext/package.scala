package com.doubajam

import java.util
import java.util.{Collections, Random}

import scala.collection.mutable.ArrayBuffer

package object fasttext {
  type Real  = Float
  type UInt8 = Int

  class StdVector[A](initialSize: Int) extends ArrayBuffer[A](initialSize) {
    def shuffle(from: Int, to: Int, rnd: Random) = {
      Collections.shuffle(util.Arrays.asList(array: _*).subList(from, to), rnd)
    }
  }
}
