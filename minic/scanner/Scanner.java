package minic.scanner;

import minic.scanner.SourceFile;
import minic.scanner.Token;

/**
 * NOTE.
 *
 * <p>This is a dummy scanner implementation that is only used when the
 * course participant wishes to use the provided scanner class file.
 * The dummy implementation makes the MiniC framework compileable.
 * After compiling, the class-file of the dummy scanner is replaced
 * by the provided scanner's class file.
 *
 * <p>Students who wish to use their own scanner should replace this file with
 * their own scanner from Assignment 1.
 *
 */
public final class Scanner {

  /** Constructor (stub-only). */
  public Scanner(SourceFile source) {
    System.out.println("ERROR: empty scanner implementation used!");
    System.out.println("Provide your own Scanner.java or use the provided"
                       + " class files!");
    System.out.println("See the Assignment specification on how to use the"
                       + " provided classfiles.");
    System.exit(1);
  }

  /** Ask scanner to emit debug output for every token (stub-only). */
  public void enableDebugging() {
    return;
  }

  /** Scans the next token (stub-only). */
  public Token scan() {
    SourcePos pos = new SourcePos();
    Token errorToken = new Token(Token.ERROR, "Empty Scanner!", pos);
    return errorToken;
  }

}
