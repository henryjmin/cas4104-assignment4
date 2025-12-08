package minic.parser;

/** SyntaxError class to report syntax errors during parsing. */
public class SyntaxError extends Exception {

  SyntaxError() {
    super();
  }

  SyntaxError(String s) {
    super(s);
  }

  public static final long serialVersionUID = 3L;
  // Exceptions inherit from Throwable, which implement the
  // Serializable interface.
  // https://docs.oracle.com/javase/8/docs/api/java/lang/RuntimeException.html

}
