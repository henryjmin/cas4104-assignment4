package minic.astgen;

import minic.scanner.SourcePos;

public abstract class Terminal extends AST {

  public String Lexeme;

  public Terminal (SourcePos pos) {
    super (pos);
  }

}
