package com.doubajam.fasttext

import java.io.InputStream
import java.nio.{ByteBuffer, ByteOrder}

object IOUtil {
  private var string_buf_size = 128
  private val int_bytes       = new Array[Byte](4)
  private val float_bytes     = new Array[Byte](4)
  private val long_bytes      = new Array[Byte](8)
  private val double_bytes    = new Array[Byte](8)
  private var string_bytes    = new Array[Byte](string_buf_size)
  private val stringBuilder   = new StringBuilder()

  private var float_array_bytebuffer: ByteBuffer = _
  private var float_array_bytes: Array[Byte]     = _

  def setStringBufferSize(size: Int) = {
    string_buf_size = size
    string_bytes = new Array[Byte](string_buf_size)
  }

  def setFloatArrayBufferSize(itemSize: Int) = {
    float_array_bytebuffer = ByteBuffer.allocate(itemSize * 4).order(ByteOrder.LITTLE_ENDIAN)
    float_array_bytes = new Array[Byte](itemSize * 4)
  }

  def readByte(is: InputStream) = {
    is.read() & 0xFF
  }

  def readInt(is: InputStream) = {
    is.read(int_bytes)
    getInt(int_bytes)
  }

  def getInt(b: Array[Byte]) = {
    (b(0) & 0xFF) << 0 | (b(1) & 0xFF) << 8 | (b(2) & 0xFF) << 16 | (b(3) & 0xFF) << 24
  }

  def readLong(is: InputStream) = {
    is.read(long_bytes)
    getLong(long_bytes)
  }

  def getLong(b: Array[Byte]) = {
    (b(0) & 0xFFL) << 0 | (b(1) & 0xFFL) << 8 | (b(2) & 0xFFL) << 16 | (b(3) & 0xFFL) << 24 | (b(4) & 0xFFL) << 32 | (b(
      5) & 0xFFL) << 40 | (b(6) & 0xFFL) << 48 | (b(7) & 0xFFL) << 56
  }

  def readFloat(is: InputStream) = {
    is.read(float_bytes)
    getFloat(float_bytes)
  }

  def readFloat(is: InputStream, data: Array[Float]) = {
    is.read(float_array_bytes)
    float_array_bytebuffer.clear()
    float_array_bytebuffer.put(float_array_bytes).flip().asInstanceOf[ByteBuffer].asFloatBuffer().get(data)
  }

  def getFloat(b: Array[Byte]) = {
    java.lang.Float.intBitsToFloat((b(0) & 0xFF) << 0 | (b(1) & 0xFF) << 8 | (b(2) & 0xFF) << 16 | (b(3) & 0xFF) << 24)
  }

  def readDouble(is: InputStream) = {
    is.read(double_bytes)
    getDouble(double_bytes)
  }

  def getDouble(b: Array[Byte]) = {
    java.lang.Double.longBitsToDouble(getLong(b))
  }

  def readString(is: InputStream): String = {
    var b = is.read()
    if (b < 0) {
      return null
    }
    var i = -1
    stringBuilder.setLength(0)
    // ascii space, \n, \0
    while (b > -1 && b != 32 && b != 10 && b != 0) {
      string_bytes(i + 1) = b.toByte
      i += 1
      b = is.read()
      if (i == string_buf_size - 1) {
        stringBuilder.append(new String(string_bytes /*, "utf8"*/ ))
        i = -1
      }
    }
    stringBuilder.append(new String(string_bytes, 0, i + 1 /*, "utf8"*/ ))
    stringBuilder.toString()
  }

  def intToByte(i: Int) = {
    i & 0xFF
  }

  def intToByteArray(i: Int) = {
    int_bytes(0) = ((i >> 0) & 0xff).toByte
    int_bytes(1) = ((i >> 8) & 0xff).toByte
    int_bytes(2) = ((i >> 16) & 0xff).toByte
    int_bytes(3) = ((i >> 24) & 0xff).toByte
    int_bytes
  }

  def longToByteArray(l: Long) = {
    long_bytes(0) = ((l >> 0) & 0xff).toByte
    long_bytes(1) = ((l >> 8) & 0xff).toByte
    long_bytes(2) = ((l >> 16) & 0xff).toByte
    long_bytes(3) = ((l >> 24) & 0xff).toByte
    long_bytes(4) = ((l >> 32) & 0xff).toByte
    long_bytes(5) = ((l >> 40) & 0xff).toByte
    long_bytes(6) = ((l >> 48) & 0xff).toByte
    long_bytes(7) = ((l >> 56) & 0xff).toByte
    long_bytes
  }

  def floatToByteArray(f: Float) = {
    intToByteArray(java.lang.Float.floatToIntBits(f))
  }

  def floatToByteArray(f: Array[Float]) = {
    float_array_bytebuffer.clear()
    float_array_bytebuffer.asFloatBuffer().put(f)
    float_array_bytebuffer.array()
  }

  def doubleToByteArray(d: Double) = {
    longToByteArray(java.lang.Double.doubleToRawLongBits(d))
  }
}
