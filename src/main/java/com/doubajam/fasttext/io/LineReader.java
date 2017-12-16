package com.doubajam.fasttext.io;


import java.io.*;
import java.nio.charset.Charset;

public abstract class LineReader extends Reader {

    protected InputStream inputStream_ = null;
    protected File file_ = null;
    protected Charset charset_ = null;

    protected LineReader() {
        super();
    }

    protected LineReader(Object lock) {
        super(lock);
    }

    public LineReader(String filename, String charsetName) throws IOException, UnsupportedEncodingException {
        this();
        this.file_ = new File(filename);
        this.charset_ = Charset.forName(charsetName);
    }

    public LineReader(InputStream inputStream, String charsetName) throws UnsupportedEncodingException {
        this();
        this.inputStream_ = inputStream;
        this.charset_ = Charset.forName(charsetName);
    }

    /**
     * Skips lines.
     *
     * @param n The number of lines to skip
     * @return The number of lines actually skipped
     * @throws IOException              If an I/O error occurs
     * @throws IllegalArgumentException If <code>n</code> is negative.
     */
    public abstract long skipLine(long n) throws IOException;

    public abstract String readLine() throws IOException;

    public abstract String[] readLineTokens() throws IOException;

    public abstract void rewind() throws IOException;
}
