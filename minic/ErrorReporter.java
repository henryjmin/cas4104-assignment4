package minic;

import minic.scanner.SourcePos;

/** Report errors on the terminal. */
public class ErrorReporter {

  int numErrors;

  ErrorReporter() {
    numErrors = 0;
  }

  /**
   * Report given error.
   *
   * @param message contains the error message to print.
   * @param tokenName is an optional string to insert in place of % in message.
   * @param pos contains the position information.
   */
  public void reportError(String message, String tokenName, SourcePos pos) {
    System.out.print("ERROR: ");
    for (int c = 0; c < message.length(); c++) {
      if (message.charAt(c) == '%') {
        System.out.print(tokenName);
      } else {
        System.out.print(message.charAt(c));
      }
    }
    System.out.println(" " + pos.startCol + ".." + pos.endCol + ", line " + pos.startLine + ".");
    numErrors++;
  }
}
