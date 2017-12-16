package com.doubajam.fasttext

import scala.annotation.strictfp

@strictfp
class Vector(val size: Int) {
  val data: Array[Float] = new Array[Float](size)

  def +=(source: Vector) = {
    for (i <- 0 until size) {
      data(i) += source.data(i)
    }
  }

  def norm(): Float = {
    var sum = 0f
    for (i <- 0 until size) {
      sum += data(i) * data(i)
    }
    math.sqrt(sum).toFloat
  }

  def zero(): Unit = {
    for (i <- 0 until size) {
      data(i) = 0.0f
    }
  }

  def *=(a: Float) = {
    for (i <- 0 until size) {
      data(i) *= a
    }
  }

  def argmax: Int = {
    var max    = data(0)
    var argmax = 0
    for (i <- 1 until size) {
      if (data(i) > max) {
        max = data(i)
        argmax = i
      }
    }
    argmax
  }

  def apply(i: Int): Float = data(i)

  def update(i: Int, value: Float): Unit = {
    data(i) = value
  }

  def addRow(A: Matrix, i: Int) = {
    require(i >= 0)
    require(i < A.m)
    require(size == A.n)

    for (j <- 0 until A.n) {
      data(j) += A(i)(j)
    }
  }

  def addRow(A: Matrix, i: Int, a: Float) {
    require(i >= 0)
    require(i < A.m)
    require(size == A.n)
    for (j <- 0 until A.n) {
      data(j) += a * A(i)(j)
    }
  }

  def addRow(A: QMatrix, i: Int) {
    require(i >= 0)
    A.addToVector(this, i)
  }

  def mul(A: Matrix, vec: Vector) = {
    require(A.m == size)
    require(A.n == vec.size)

    for {
      i <- 0 until size
      _ = data(i) = 0.0f
      j <- 0 until A.n
    } {
      data(i) += A(i)(j) * vec(j)
    }
  }

  def mul(A: QMatrix, vec: Vector): Unit = {
    require(A.m == size)
    require(A.n == vec.size)
    for (i <- 0 until size) {
      data(i) = A.dotRow(vec, i)
    }
  }

  override def toString: String = {
    data.mkString(" ")
  }

}
