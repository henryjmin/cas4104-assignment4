package minic.scanner;

/**
 * Class SourcePos represents the position of a lexeme in the input program.
 * It consists of the start and the end of the lexeme in terms of the line
 * and column number.
 */
public class SourcePos {

  public int startCol;
  public int endCol;
  public int startLine;
  public int endLine;

  /** Constructor for a SourcePos object. The actual position information
   * will later be set by the scanner.
   */
  public SourcePos() {
    startCol = 0;
    endCol = 0;
    startLine = 0;
    endLine = 0;
  }

}
