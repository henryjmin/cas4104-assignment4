package minic.scanner;

/**
 * Class SourceFile implements an abstraction of the source file as a stream of characters.
 *
 * <p>The name of the source file to open is provided to the constructor. Method readChar() is
 * provided to read the next character from the input.
 */
public class SourceFile {

  java.io.File sourceFile;
  java.io.FileInputStream source;
  public static final char EOF = '\u0000';

  /**
   * Constructor of class SourceFile.
   *
   * @param filename the name of the source file to open.
   */
  public SourceFile(String filename) {
    try {
      sourceFile = new java.io.File(filename);
      source = new java.io.FileInputStream(sourceFile);
    } catch (java.io.IOException e) {
      System.err.println("Error opening file " + filename);
      System.err.println("Exiting...");
      System.exit(1);
    }
  }

  /** Method readChar() returns the next character from the source file. */
  public char readChar() {
    try {
      int c = source.read();
      if (c == -1) {
        c = EOF;
      }
      return (char) c;
    } catch (java.io.IOException e) {
      return EOF;
    }
  }
}
