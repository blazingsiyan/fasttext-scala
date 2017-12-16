package com.doubajam.fasttext

import java.io.InputStream

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object EntryType extends Enumeration {
  type EntryType = Value
  val word  = Value(0)
  val label = Value(1)
}

import EntryType._

case class Entry(word: String, var count: Long, entryType: EntryType, subwords: ArrayBuffer[Int] = ArrayBuffer.empty) {
  override def toString: String = {
    val builder = new StringBuilder()
    builder.append("entry [word=")
    builder.append(word)
    builder.append(", count=")
    builder.append(count)
    builder.append(", type=")
    builder.append(entryType)
    builder.append(", subwords=")
    builder.append(subwords)
    builder.append("]")
    builder.toString()
  }
}

class Dictionary(
    var words: ArrayBuffer[Entry],
    var word2int: Array[Int],
    var size: Int,
    var nwords: Int,
    var nlabels: Int,
    var ntokens: Long,
    var pruneidx_size: Long,
    var pruneidx: Map[Int, Int],
    var args: Args
) {
  import Dictionary._

  var pdiscard: ArrayBuffer[Float] = new ArrayBuffer[Float](size)

  var charsetName = "UTF-8"

  def init() = {}

  def find(w: String): Int = {
    find(w, hash(w))
  }

  def find(w: String, h: Long): Int = {
    var id = (h % MAX_VOCAB_SIZE).toInt
    while (word2int(id) != -1 && words(word2int(id)).word != w) {
      id = (id + 1) % MAX_VOCAB_SIZE
    }
    id
  }

  def getSubwords(i: Int): ArrayBuffer[Int] = {
    require(i >= 0)
    require(i < nwords)
    words(i).subwords
  }

  def getSubwords(word: String): ArrayBuffer[Int] = {
    val i = getId(word)
    if (i >= 0) {
      getSubwords(i)
    } else {
      val ngrams = new ArrayBuffer[Int]()
      computeSubwords(BOW + word + EOW, ngrams)
      ngrams
    }
  }

  def discard(id: Int, rand: Float) = {
    require(id >= 0)
    require(id < nwords)
    if (args.model == ModelName.sup)
      false
    else
      rand > pdiscard(id)
  }

  def getId(w: String, h: Long): Int = {
    val id = find(w, h)
    word2int(id)
  }

  def getId(w: String): Int = {
    val h = find(w)
    word2int(h)
  }

  def getType(id: Int) = {
    require(id >= 0)
    require(id < size)
    words(id).entryType
  }

  def getType(w: String) = {
    if (w.contains(args.label)) EntryType.label else EntryType.word
  }

  def getWord(id: Int) = {
    require(id >= 0)
    require(id < size)
    words(id).word
  }

  /**
    * String FNV-1a Hash
    */
  def hash(str: String): Long = {
    var h = 2166136261L.toInt // 0xffffffc5
    str.getBytes.foreach(byte => {
      h = (h ^ byte) * 16777619 // FNV-1a
    })
    h & 0xffffffffL
  }

  private def charMatches(ch: Char) = {
    ch == ' ' || ch == '\t' || ch == '\n' || ch == '\f' || ch == '\r'
  }

  def computeSubwords(word: String, ngrams: ArrayBuffer[Int]) = {
    for (i <- 0 until word.length) {
      val ngram = new StringBuilder()
      if (charMatches(word.charAt(i))) {
        // continue
      } else {
        var j = i
        var n = 1
        while (j < word.length && n <= args.maxn) {
          j += 1
          ngram.append(word.charAt(j))
          while (j < word.length && charMatches(word.charAt(j))) {
            j += 1
            ngram.append(word.charAt(j))
          }
          if (n >= args.minn && !(n == 1 && (i == 0 || j == word.length))) {
            val h = (nwords + (hash(ngram.toString()) % args.bucket)).toInt
            if (h < 0) {
              System.err.println("computeNgrams h<0: " + h + " on word: " + word)
            }
            ngrams += h
          }
          n += 1
        }
      }
    }
  }

  def initNgrams() = {
    for (i <- 0 until size) {
      val word = BOW + words(i).word + EOW
      words(i).subwords.append(i)
      computeSubwords(word, words(i).subwords)
    }
  }

  def initTableDiscard() = {
    pdiscard = new ArrayBuffer[Real]()
    for (i <- 0 until size) {
      val f = words(i).count / ntokens.toFloat
      pdiscard.append(math.sqrt(args.t / f).toFloat + args.t.toFloat / f)
    }
  }

  def getCounts(entryType: EntryType): ArrayBuffer[Long] = {
    val counts = new ArrayBuffer[Long]()
    words.foreach(w => {
      if (w.entryType == entryType) counts.append(w.count)
    })
    counts
  }

  def addWordNgrams(line: ArrayBuffer[Int], hashes: ArrayBuffer[Long], n: Int): Unit = {
    if (pruneidx_size == 0) return

    for (i <- hashes.indices) {
      var h = hashes(i)
      var j = i + 1
      while (j < hashes.size && j < i + n) {
        h = h * 116049371 + hashes(j)
        var id = (h % args.bucket).toInt
        if (pruneidx_size > 0) {
          if (pruneidx.contains(id)) {
            id = pruneidx(id)
            line.append(nwords + id)
          } else {
            // continue
          }
        }
        j += 1
      }
    }
  }

  def getLabel(lid: Int) = {
    require(lid >= 0)
    require(lid < nlabels)
    words(lid + nwords).word
  }

  def getLine(
      tokens: Seq[String],
      words: ArrayBuffer[Int],
      word_hashes: ArrayBuffer[Long],
      labels: ArrayBuffer[Int],
      rng: Random
  ): Int = {

    words.clear()
    labels.clear()
    word_hashes.clear()
    var ntokens = 0

    var break = false
    for (i <- tokens.indices if !break) {
      if (i < tokens.length && Option(tokens(i)).isEmpty) {
        //
      } else {
        val token = tokens(i)
        val h     = hash(token)
        val wid   = getId(token, h)
        if (wid < 0) {
          val etype = getType(token)
          if (etype == EntryType.word) word_hashes.append(h)
        } else {
          val etype = getType(wid)
          ntokens += 1
          if (etype == EntryType.word && !discard(wid, rng.nextFloat())) {
            words.append(wid)
            word_hashes.append(hash(token))
          }
          if (etype == EntryType.label) {
            labels.append(wid - nwords)
          }
          if (token == EOS) break = true
          if (ntokens > MAX_LINE_SIZE && args.model != ModelName.sup) break = true
        }
      }
    }
    ntokens
  }

  def getLine(
      tokens: Seq[String],
      words: ArrayBuffer[Int],
      labels: ArrayBuffer[Int],
      rng: Random
  ): Int = {
    val word_hashes = new ArrayBuffer[Long]()
    val ntokens     = getLine(tokens, words, word_hashes, labels, rng)
    if (args.model == ModelName.sup) {
      addWordNgrams(words, word_hashes, args.wordNgrams)
    }
    ntokens
  }

  override def toString: String = {
    val builder = new StringBuilder()
    builder.append("Dictionary [words_=")
    builder.append(words.take(10))
    builder.append(", pdiscard_=")
    builder.append(pdiscard)
    builder.append(", word2int_=")
    builder.append(word2int)
    builder.append(", size_=")
    builder.append(size)
    builder.append(", nwords_=")
    builder.append(nwords)
    builder.append(", nlabels_=")
    builder.append(nlabels)
    builder.append(", ntokens_=")
    builder.append(ntokens)
    builder.append("]")
    builder.toString()
  }
}

object Dictionary {
  val MAX_VOCAB_SIZE = 30000000
  val MAX_LINE_SIZE  = 1024
  val WORDID_DEFAULT = -1

  val EOS = "</s>"
  val BOW = "<"
  val EOW = ">"

  def load(is: InputStream, args: Args): Dictionary = {
    val size          = IOUtil.readInt(is)
    val nwords        = IOUtil.readInt(is)
    val nlabels       = IOUtil.readInt(is)
    val ntokens       = IOUtil.readLong(is)
    val pruneidx_size = IOUtil.readLong(is)

    val word2int = Array.fill(MAX_VOCAB_SIZE)(-1)
    val words    = new ArrayBuffer[Entry](size)

    val dict = new Dictionary(words, word2int, size, nwords, nlabels, ntokens, pruneidx_size, null, args)

    for (i <- 0 until size) {
      val word      = IOUtil.readString(is)
      val count     = IOUtil.readLong(is)
      val entryType = EntryType(IOUtil.readByte(is))
      val e         = Entry(word, count, entryType)
      dict.words.append(e)
      dict.word2int(dict.find(e.word)) = i
    }

    val pruneidx = (for (_ <- 0 until pruneidx_size.toInt) yield {
      val first  = IOUtil.readInt(is)
      val second = IOUtil.readInt(is)
      first -> second
    }).toMap

    dict.pruneidx = pruneidx

    dict.initTableDiscard()
    dict.initNgrams()
    dict
  }
}
