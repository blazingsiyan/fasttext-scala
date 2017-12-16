package com.doubajam.fasttext

import java.io.InputStream

import scala.annotation.strictfp
import scala.util.Random

@strictfp
class Matrix(val m: Int, val n: Int) {
  val data: Array[Array[Float]] = Array.ofDim[Float](m, n)

  def zero(): Unit = {
    for {
      i <- 0 until m
      j <- 0 until n
    } {
      data(i)(j) = 0.0f
    }
  }

  def uniform(a: Float): Unit = {
    val random = new Random(1l)
    for {
      i <- 0 until m
      j <- 0 until n
    } {
      data(i)(j) = -a + random.nextFloat() * a * 2
    }
  }

  def addRow(vec: Vector, i: Int, a: Float): Unit = {
    require(i >= 0)
    require(i < m)
    require(vec.size == n)

    for (j <- 0 until n) {
      data(i)(j) += a * vec(j)
    }
  }

  def dotRow(vec: Vector, i: Int): Float = {
    require(i >= 0)
    require(i < m)
    require(vec.size == n)

    var d = 0.0f
    for (j <- 0 until n) {
      d += data(i)(j) * vec(j)
    }
    d
  }

  def apply(i: Int)(j: Int): Float = data(i)(j)

  override def toString: String = {
    val builder = new StringBuilder()
    builder.append("Matrix (data=")
    if (data != null) {
      builder.append("[[")
      for {
        i <- 0 until math.min(m, 1)
        j <- 0 until math.min(n, 10)
      } {
        builder.append(data(i)(j)).append(",")
      }
      builder.setLength(builder.length() - 1)
      builder.append("],...]")
    } else {
      builder.append("null")
    }
    builder.append(", m=")
    builder.append(m)
    builder.append(", n=")
    builder.append(n)
    builder.append(")")
    builder.toString()
  }
}

object Matrix {
  def load(is: InputStream): Matrix = {
    val m = IOUtil.readLong(is).toInt
    val n = IOUtil.readLong(is).toInt

    IOUtil.setFloatArrayBufferSize(n)

    val matrix = new Matrix(m, n)
    for (i <- 0 until m) {
      IOUtil.readFloat(is, matrix.data(i))
    }
    matrix
  }
}
