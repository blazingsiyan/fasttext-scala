package com.doubajam.fasttext

import java.io.{File, FileInputStream, IOException}

object Main {

  def printUsage(): Unit = {
    println(
      """
        |usage: java -jar fasttext.jar <command> <args>
        |The commands supported by fasttext are:
        |
        |supervised          train a supervised classifier
        |test                evaluate a supervised classifier
        |predict             predict most likely labels
        |predict-prob        predict most likely labels with probabilities
        |skipgram            train a skipgram model
        |cbow                train a cbow model
        |print-vectors       print vectors given a trained model
        |
      """.stripMargin
    )
  }

  def printTestUsage() = {
    println(
      """
        |usage: java -jar fasttext.jar test <model> <test-data> [<k>]
        |
        |  <model>      model filename
        |  <test-data>  test data filename (if -, read from stdin)
        |  <k>          (optional; 1 by default) predict top k labels
      """.stripMargin
    )
  }

  def printPredictUsage() = {
    println(
      """
        |usage: java -jar fasttext.jar predict[-prob] <model> <test-data> [<k>]
        |
        |  <model>      model filename
        |  <test-data>  test data filename (if -, read from stdin)
        |  <k>          (optional; 1 by default) predict top k labels
      """.stripMargin
    )
  }

  def printPrintVectorsUsage() = {
    println(
      """
        |usage: java -jar fasttext.jar print-vectors <model>
        |
        |<model> model filename
      """.stripMargin
    )
  }

  def train(args: Array[String]) = ()

  def test(args: Array[String]) = {
    var k = 1
    if (args.length == 3) {
      k = 1
    } else if (args.length == 4) {
      k = args(3).toInt
    } else {
      printTestUsage()
      System.exit(1)
    }
    val fasttext = FastText.loadModel(args(1))
    val infile = args(2)
    if ("-".equals(infile)) {
      fasttext.test(System.in, k)
    } else {
      val file = new File(infile)
      if (!(file.exists && file.isFile && file.canRead)) {
        throw new IOException("Test file cannot be opened!")
      }
      fasttext.test(new FileInputStream(file), k)
    }
  }

  def predict(args: Array[String]) = {
    var k = 1
    if (args.length == 3) {
      k = 1
    } else if (args.length == 4) {
      k = args(3).toInt
    } else {
      printPredictUsage()
      System.exit(1)
    }
    val printProb = "predict-prob" == args(0)

    val fasttext = FastText.loadModel(args(1))

    val infile = args(2)
    if ("-".equals(infile)) {
      fasttext.predict(System.in, k, printProb)
    } else {
      val file = new File(infile)
      if (!(file.exists && file.isFile && file.canRead)) {
        throw new IOException("Input file cannot be opened!")
      }
      fasttext.predict(new FileInputStream(file), k, printProb)
    }
  }

  def printVectors(args: Array[String]) = {
    if (args.length != 2) {
      printPrintVectorsUsage()
      System.exit(1)
    }
    val fasttext = FastText.loadModel(args(1))
  }

  def main(args: Array[String]): Unit = {
    if (args.length == 0) {
      printUsage()
      System.exit(1)
    }

    try {
      val cmd = args(0)
      cmd.toLowerCase match {
        case "skipgram" | "cbow" | "supervised" => train(args)
        case "test"                             => test(args)
        case "print-vectors"                    => printVectors(args)
        case "predict"                          => predict(args)
        case _ =>
          printUsage()
          System.exit(1)
      }
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
        System.exit(1)
    }

    System.exit(0)
  }
}
