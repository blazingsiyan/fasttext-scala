package com.doubajam.fasttext

import java.io._

import com.doubajam.fasttext.io.{BufferedLineReader, LineReader}

import scala.annotation.strictfp
import scala.collection.mutable.ArrayBuffer

@strictfp
class FastText(val args: Args, val dict: Dictionary, val model: Model) {
  private val charsetName = "UTF-8"
  private val lineReaderClass = classOf[BufferedLineReader]

  def wordVectors() = {
    val vec = new Vector(args.dim)
    var lineReader: LineReader = null
    try {
      lineReader =
        lineReaderClass.getConstructor(classOf[InputStream], classOf[String]).newInstance(System.in, charsetName)
      var word: String = null
      while ({ word = lineReader.readLine(); Option(word).nonEmpty }) {
        getVector(vec, word)
        println(word + " " + vec)
      }
    } catch {
      case e: Exception => e.printStackTrace()
    } finally {
      if (lineReader != null) {
        try {
          lineReader.close()
        } catch {
          case e: Exception => e.printStackTrace()
        }
      }
    }
  }

  def getVector(vec: Vector, word: String): Unit = {
    val ngrams = dict.getSubwords(word)
    vec.zero()
    ngrams.foreach(i => vec.addRow(model.wi, i))
    if (ngrams.nonEmpty) {
      vec *= (1.0f / ngrams.size)
    }
  }

  def predict(tokens: Seq[String], k: Int, predictions: ArrayBuffer[(Float, String)]): Unit = {
    val words = new ArrayBuffer[Int]
    val labels = new ArrayBuffer[Int]
    predictions.clear()
    dict.getLine(tokens, words, labels, model.rng)
    if (words.isEmpty) return

    val hidden = new Vector(args.dim)
    val output = new Vector(dict.nlabels)

    val modelPredictions = model.predict(words, k, hidden, output)
    modelPredictions.foreach(t => predictions += t._1 -> dict.getLabel(t._2))
  }

  def predict(in: InputStream, k: Int, print_prob: Boolean): Unit = {
    val predictions = new ArrayBuffer[(Float, String)](k)

    var lineReader: LineReader = null
    try {
      lineReader = lineReaderClass.getConstructor(classOf[InputStream], classOf[String]).newInstance(in, charsetName)
      var tokens: Array[String] = null
      while ({ tokens = lineReader.readLineTokens(); tokens != null }) {
        if (tokens.length == 1 && "quit".equals(tokens(0))) {
          return
        }
        predictions.clear()
        predict(tokens ++ Array(Dictionary.EOS), k, predictions)
        if (predictions.isEmpty) {
          println("n/a")
        } else {
          for (t <- predictions) {
            print(t._2)
            if (print_prob) {
              printf(" %f", math.exp(t._1))
            }
          }
          println()
        }
      }
    } finally {
      if (lineReader != null) {
        lineReader.close()
      }
    }
  }

  def predictProb(tokens: Seq[String], k: Int): Iterable[(Float, String)] = {
    val predictions = new ArrayBuffer[(Float, String)](k)
    predict(tokens ++ Seq(Dictionary.EOS), k, predictions)
    predictions.map(t => math.exp(t._1).toFloat -> t._2)
  }

  def test(is: InputStream, k: Int): Unit = {
    var nexamples = 0
    var nlabels = 0
    var precision = 0.0f
    val line = new ArrayBuffer[Int]()
    val labels = new ArrayBuffer[Int]()

    var lineReader: LineReader = null
    try {
      lineReader = lineReaderClass.getConstructor(classOf[InputStream], classOf[String]).newInstance(is, charsetName)
      var lineTokens: Array[String] = null
      val tic = System.currentTimeMillis()
      while ({
        lineTokens = lineReader.readLineTokens(); lineTokens != null
      }) {
        if (lineTokens.length == 1 && "quit".equals(lineTokens(0))) {
          return
        }
        dict.getLine(lineTokens, line, labels, model.rng)
        if (labels.nonEmpty && line.nonEmpty) {
          val modelPredictions = model.predict(line, k)
          modelPredictions.foreach(t => {
            if (labels.contains(t._2)) {
              precision += 1.0f
            }
          })
          nexamples += 1
          nlabels += labels.size
        }
      }
      println(s"test finished in ${System.currentTimeMillis() - tic}ms.")
    } finally {
      if (lineReader != null)
        lineReader.close()
    }

    printf("P@%d: %.3f%n", k, precision / (k * nexamples))
    printf("R@%d: %.3f%n", k, precision / nlabels)
    println("Number of examples: " + nexamples)
  }
}

object FastText {
  def loadModel(is: InputStream): FastText = {
    // Read magic number and version
    val magic_number = IOUtil.readInt(is)
    val version = IOUtil.readInt(is)

    val args = Args.load(is)
    val dict = Dictionary.load(is, args)

    var qinput: QMatrix = null
    var input: Matrix = null
    var qoutput: QMatrix = null
    var output: Matrix = null

    // load input matrix
    var quant = false
    val quant_input = is.read() != 0
    if (quant_input) {
      quant = true
      qinput = QMatrix.load(is)
    } else {
      input = Matrix.load(is)
    }

    // load output matrix
    args.qout = is.read() != 0
    if (quant && args.qout) {
      qoutput = QMatrix.load(is)
    } else {
      output = Matrix.load(is)
    }
    val model = new Model(input, output, args, 0)
    model.quant = quant
    model.setQuantizePointer(qinput, qoutput, args.qout)

    println(s"""
               |Loaded model:
               |  magic_number: $magic_number
               |  version: $version
               |  args: $args
               |  quant: $quant
               |  qout: ${args.qout}
               |  input: $input
               |  qinput: $qinput
               |  output: $output
               |  qoutput: $qoutput
               |  model: $model
         """.stripMargin)

    if (args.model == ModelName.sup) {
      model.setTargetCounts(dict.getCounts(EntryType.label))
    } else {
      model.setTargetCounts(dict.getCounts(EntryType.word))
    }
    new FastText(args, dict, model)
  }

  def loadModel(filename: String): FastText = {

    var dis: DataInputStream = null
    var bis: BufferedInputStream = null

    try {
      val file = new File(filename)
      if (!(file.exists() && file.isFile && file.canRead)) {
        throw new IOException("Model file cannot be opened for loading!")
      }
      bis = new BufferedInputStream(new FileInputStream(file))
      dis = new DataInputStream(bis)

      loadModel(dis)
    } finally {
      if (bis != null) {
        bis.close()
      }
      if (dis != null) {
        dis.close()
      }
    }
  }
}
