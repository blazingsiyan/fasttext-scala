package com.doubajam.fasttext

import java.io.InputStream

object ModelName extends Enumeration {
  type ModelName = Value
  val cbow = Value(1)
  val sg   = Value(2)
  val sup  = Value(3)
}

object LossName extends Enumeration {
  type LossName = Value
  val hs      = Value(1)
  val ns      = Value(2)
  val softmax = Value(3)
}

class Args {
  var input: String     = _
  var output: String    = _
  var test: String      = _
  var lr                = 0.05
  var lrUpdateRate      = 100
  var dim               = 100
  var ws                = 5
  var epoch             = 5
  var minCount          = 5
  var minCountLabel     = 0
  var neg               = 5
  var wordNgrams        = 1
  var loss              = LossName.ns
  var model             = ModelName.sg
  var bucket            = 2000000
  var minn              = 3
  var maxn              = 6
  var thread            = 1
  var t                 = 1e-4
  var label             = "__label__"
  var verbose           = 2
  var pretrainedVectors = ""

  var saveOutput = 0
  var qout       = false
  var retrain    = false
  var qnorm      = false
  var cutoff     = 0
  var dsub       = 0

  override def toString: String = {
    val builder = new StringBuilder()
    builder.append("Args (input=")
    builder.append(input)
    builder.append(", output=")
    builder.append(output)
    builder.append(", test=")
    builder.append(test)
    builder.append(", lr=")
    builder.append(lr)
    builder.append(", lrUpdateRate=")
    builder.append(lrUpdateRate)
    builder.append(", dim=")
    builder.append(dim)
    builder.append(", ws=")
    builder.append(ws)
    builder.append(", epoch=")
    builder.append(epoch)
    builder.append(", minCount=")
    builder.append(minCount)
    builder.append(", minCountLabel=")
    builder.append(minCountLabel)
    builder.append(", neg=")
    builder.append(neg)
    builder.append(", wordNgrams=")
    builder.append(wordNgrams)
    builder.append(", loss=")
    builder.append(loss)
    builder.append(", model=")
    builder.append(model)
    builder.append(", bucket=")
    builder.append(bucket)
    builder.append(", minn=")
    builder.append(minn)
    builder.append(", maxn=")
    builder.append(maxn)
    builder.append(", thread=")
    builder.append(thread)
    builder.append(", t=")
    builder.append(t)
    builder.append(", label=")
    builder.append(label)
    builder.append(", verbose=")
    builder.append(verbose)
    builder.append(", pretrainedVectors=")
    builder.append(pretrainedVectors)
    builder.append(")")
    builder.toString()
  }
}

object Args {
  def printHelp(): Unit = {
    println(
      """The following arguments are mandatory:
        |
        |  -input              training file path
        |  -output             output file path
        |
        |The following arguments are optional:
        |  -lr                 learning rate [" + lr + "]
        |  -lrUpdateRate       change the rate of updates for the learning rate [" + lrUpdateRate + "]
        |  -dim                size of word vectors [" + dim + "]
        |  -ws                 size of the context window [" + ws + "]
        |  -epoch              number of epochs [" + epoch + "]
        |  -minCount           minimal number of word occurences [" + minCount + "]
        |  -minCountLabel      minimal number of label occurences [" + minCountLabel + "]
        |  -neg                number of negatives sampled [" + neg + "]
        |  -wordNgrams         max length of word ngram [" + wordNgrams + "]
        |  -loss               loss function {ns, hs, softmax} [ns]
        |  -bucket             number of buckets [" + bucket + "]
        |  -minn               min length of char ngram [" + minn + "]
        |  -maxn               max length of char ngram [" + maxn + "]
        |  -thread             number of threads [" + thread + "]
        |  -t                  sampling threshold [" + t + "]
        |  -label              labels prefix [" + label + "]
        |  -verbose            verbosity level [" + verbose + "]
        |  -pretrainedVectors  pretrained word vectors for supervised learning []")
      """.stripMargin
    )
  }

  def apply(args: Array[String]): Args = {
    val x = new Args

    val command = args(0)
    command match {
      case "supervised" =>
        x.model = ModelName.sup
        x.loss = LossName.softmax
        x.minCount = 1
        x.minn = 0
        x.maxn = 0
        x.lr = 0.1
      case "cbow" =>
        x.model = ModelName.cbow
      case _ =>
    }

    var ai = 1
    while (ai < args.length) {
      if (args(ai).charAt(0) != '-') {
        println("Provided argument without a dash! Usage:")
        printHelp()
        System.exit(1)
      }

      if ("-h" == args(ai)) {
        System.out.println("Here is the help! Usage:")
        printHelp()
        System.exit(1)
      } else if ("-input".equals(args(ai))) {
        x.input = args(ai + 1)
      } else if ("-test".equals(args(ai))) {
        x.test = args(ai + 1)
      } else if ("-output".equals(args(ai))) {
        x.output = args(ai + 1)
      } else if ("-lr".equals(args(ai))) {
        x.lr = args(ai + 1).toDouble
      } else if ("-lrUpdateRate".equals(args(ai))) {
        x.lrUpdateRate = args(ai + 1).toInt
      } else if ("-dim".equals(args(ai))) {
        x.dim = args(ai + 1).toInt
      } else if ("-ws".equals(args(ai))) {
        x.ws = args(ai + 1).toInt
      } else if ("-epoch".equals(args(ai))) {
        x.epoch = args(ai + 1).toInt
      } else if ("-minCount".equals(args(ai))) {
        x.minCount = args(ai + 1).toInt
      } else if ("-minCountLabel".equals(args(ai))) {
        x.minCountLabel = args(ai + 1).toInt
      } else if ("-neg".equals(args(ai))) {
        x.neg = args(ai + 1).toInt
      } else if ("-wordNgrams".equals(args(ai))) {
        x.wordNgrams = args(ai + 1).toInt
      } else if ("-loss".equals(args(ai))) {
        if ("hs".equalsIgnoreCase(args(ai + 1))) {
          x.loss = LossName.hs
        } else if ("ns".equalsIgnoreCase(args(ai + 1))) {
          x.loss = LossName.ns
        } else if ("softmax".equalsIgnoreCase(args(ai + 1))) {
          x.loss = LossName.softmax
        } else {
          System.out.println("Unknown loss: " + args(ai + 1))
          printHelp()
          System.exit(1)
        }
      } else if ("-bucket".equals(args(ai))) {
        x.bucket = args(ai + 1).toInt
      } else if ("-minn".equals(args(ai))) {
        x.minn = args(ai + 1).toInt
      } else if ("-maxn".equals(args(ai))) {
        x.maxn = args(ai + 1).toInt
      } else if ("-thread".equals(args(ai))) {
        x.thread = args(ai + 1).toInt
      } else if ("-t".equals(args(ai))) {
        x.t = args(ai + 1).toDouble
      } else if ("-label".equals(args(ai))) {
        x.label = args(ai + 1)
      } else if ("-verbose".equals(args(ai))) {
        x.verbose = args(ai + 1).toInt
      } else if ("-pretrainedVectors".equals(args(ai))) {
        x.pretrainedVectors = args(ai + 1)
      } else {
        System.out.println("Unknown argument: " + args(ai))
        printHelp()
        System.exit(1)
      }
      ai += 2
    }
    if (Option(x.input).isEmpty || Option(x.output).isEmpty) {
      println("Empty input or output path.")
      printHelp()
      System.exit(1)
    }
    if (x.wordNgrams <= 1 && x.maxn == 0) {
      x.bucket = 0
    }
    x
  }

  def load(is: InputStream): Args = {
    val args = new Args

    args.dim = IOUtil.readInt(is)
    args.ws = IOUtil.readInt(is)
    args.epoch = IOUtil.readInt(is)
    args.minCount = IOUtil.readInt(is)
    args.neg = IOUtil.readInt(is)
    args.wordNgrams = IOUtil.readInt(is)
    args.loss = LossName(IOUtil.readInt(is))
    args.model = ModelName(IOUtil.readInt(is))
    args.bucket = IOUtil.readInt(is)
    args.minn = IOUtil.readInt(is)
    args.maxn = IOUtil.readInt(is)
    args.lrUpdateRate = IOUtil.readInt(is)
    args.t = IOUtil.readDouble(is)

    args
  }
}
