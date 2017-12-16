package com.doubajam.fasttext

import scala.annotation.strictfp
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

@strictfp
class Model(val wi: Matrix, val wo: Matrix, val args: Args, val seed: Int, var quant: Boolean = false) {
  var qwi: QMatrix = _
  var qwo: QMatrix = _

  val SIGMOID_TABLE_SIZE = 512
  val MAX_SIGMOID        = 8
  val LOG_TABLE_SIZE     = 512

  val NEGATIVE_TABLE_SIZE = 10000000

  case class Node(parent: Int, left: Int, right: Int, count: Long, binary: Boolean)

  private val hidden: Vector = new Vector(args.dim)
  private val output: Vector = new Vector(wo.m)
  private val grad: Vector   = new Vector(args.dim)
  @transient val rng: Random = new Random(seed.toLong)

  private var osz: Int = wo.m     // output vocabSize
  private val hsz: Int = args.dim // dim

  private var loss: Float     = 0.0f
  private var nexamples: Long = 1l

  private val t_sigmoid: Array[Float] = new Array[Float](SIGMOID_TABLE_SIZE + 1)
  initSigmoid()

  private val t_log: Array[Float] = new Array[Float](LOG_TABLE_SIZE + 1)
  initLog()

  private var negatives: ArrayBuffer[Int] = _
  private var negpos: Int                 = _

  def setQuantizePointer(qwi: QMatrix, qwo: QMatrix, qout: Boolean) = {
    this.qwi = qwi
    this.qwo = qwo
    if (qout) {
      osz = qwo.m
    }
  }

  def binaryLogistic(target: Int, label: Boolean, lr: Float): Float = {
    val score = sigmoid(wo.dotRow(hidden, target))
    val alpha = lr * ((if (label) 1.0f else 0.0f) - score)
    grad.addRow(wo, target, alpha)
    wo.addRow(hidden, target, alpha)
    if (label) {
      -log(score)
    } else {
      -log(1.0f - score)
    }
  }

  def negativeSampling(target: Int, lr: Float) = {
    var loss = 0.0f
    grad.zero()
    for (n <- 0 to args.neg) {
      if (n == 0) {
        loss += binaryLogistic(target, true, lr)
      } else {
        loss += binaryLogistic(getNegative(target), false, lr)
      }
    }
    loss
  }

  def computeOutputSoftmax(hidden: Vector, output: Vector): Unit = {
    if (quant && args.qout) {
      output.mul(qwo, hidden)
    } else {
      output.mul(wo, hidden)
    }
    var max = output(0)
    var z   = 0.0f
    for (i <- 1 until osz) {
      max = math.max(output(i), max)
    }
    for (i <- 0 until osz) {
      output(i) = math.exp(output(i) - max).toFloat
      z += output(i)
    }
    for (i <- 0 until osz) {
      output(i) = output(i) / z
    }
  }

  def computeOutputSoftmax(): Unit = {
    computeOutputSoftmax(hidden, output)
  }

  def softmax(target: Int, lr: Float): Float = {
    grad.zero()
    computeOutputSoftmax()
    for (i <- 0 until osz) {
      val label = if (i == target) 1.0f else 0.0f
      val alpha = lr * (label - output(i))
      grad.addRow(wo, i, alpha)
      wo.addRow(hidden, i, alpha)
    }
    -log(output(target))
  }

  def computeHidden(input: Seq[Int], hidden: Vector) = {
    require(hidden.size == hsz)
    hidden.zero()
    input.foreach(i => {
      if (quant) {
        hidden.addRow(qwi, i)
      } else {
        hidden.addRow(wi, i)
      }
    })
    hidden *= (1.0f / input.size)
  }

  def predict(input: Seq[Int], k: Int, hidden: Vector, output: Vector): Iterable[(Float, Int)] = {
    require(k > 0)
    computeHidden(input, hidden)
    val result = findKBest(k, hidden, output)
    result.sortBy(-_._1).take(k)
  }

  def predict(input: Seq[Int], k: Int): Iterable[(Float, Int)] = {
    predict(input, k, hidden, output)
  }

  def findKBest(k: Int, hidden: Vector, output: Vector): Seq[(Float, Int)] = {
    computeOutputSoftmax(hidden, output)
    var heap = new ArrayBuffer[(Float, Int)](k + 1)
    for {
      i <- 0 until osz
    } {
      if (heap.size == k && log(output(i)) < heap(heap.size - 1)._1) {
        // continue
      } else {
        heap += log(output(i)) -> i
        heap = heap.sortBy(-_._1)
        if (heap.size > k) {
          heap = heap.sortBy(-_._1)
          heap.dropRight(1)
        }
      }
    }
    heap
  }

  def update(input: Seq[Int], target: Int, lr: Float) {
    require(target >= 0)
    require(target < osz)
    if (input.isEmpty) {
      return
    }
    computeHidden(input, hidden)

    if (args.loss == LossName.ns) {
      loss += negativeSampling(target, lr)
    } else if (args.loss == LossName.hs) {
//      loss_ += hierarchicalSoftmax(target, lr);
    } else {
//      loss_ += softmax(target, lr);
    }
    nexamples += 1

    if (args.model == ModelName.sup) {
      grad *= (1.0f / input.size)
    }

    input.foreach(i => wi.addRow(grad, i, 1.0f))
  }

  def setTargetCounts(counts: IndexedSeq[Long]) = {
    require(counts.size == osz)
    if (args.loss == LossName.ns) {
      initTableNegatives(counts)
    }
  }

  def initTableNegatives(counts: IndexedSeq[Long]) = {
    negatives = new ArrayBuffer[Int](counts.size)
    var z = 0.0f
    for (i <- counts.indices) {
      z += math.pow(counts(i), 0.5f).toFloat
    }

    for (i <- counts.indices) {
      val c = math.pow(counts(i), 0.5f).toFloat
      for (j <- 0 until (c * NEGATIVE_TABLE_SIZE / z).toInt) {
        negatives += i
      }
    }
    Utils.shuffle(negatives, rng)
  }

  def getNegative(target: Int): Int = {
    var negative: Int = negatives(negpos)
    negpos = (negpos + 1) % negatives.size
    while (target == negative) {
      negative = negatives(negpos)
      negpos = (negpos + 1) % negatives.size
    }
    negative
  }

  def getLoss: Float = {
    loss / nexamples
  }

  private def initSigmoid() {
    for (i <- 0 until SIGMOID_TABLE_SIZE + 1) {
      val x: Float = (i * 2 * MAX_SIGMOID) / SIGMOID_TABLE_SIZE - MAX_SIGMOID
      t_sigmoid(i) = (1.0f / (1.0f + math.exp(-x))).toFloat
    }
  }

  private def initLog() {
    for (i <- 0 until LOG_TABLE_SIZE + 1) {
      val x: Float = (i + 1e-5f) / LOG_TABLE_SIZE
      t_log(i) = math.log(x).toFloat
    }
  }

  def log(x: Float): Float = {
    if (x > 1.0f) {
      0.0f
    } else {
      val i = (x * LOG_TABLE_SIZE).toInt
      t_log(i)
    }
  }

  def sigmoid(x: Float): Float = {
    if (x < -MAX_SIGMOID) {
      0.0f
    } else if (x > MAX_SIGMOID) {
      1.0f
    } else {
      val i = ((x + MAX_SIGMOID) * SIGMOID_TABLE_SIZE / MAX_SIGMOID / 2).toInt
      t_sigmoid(i)
    }
  }
}
