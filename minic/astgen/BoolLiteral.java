package minic.astgen;

import minic.scanner.SourcePos;

public class BoolLiteral extends Terminal {

  public BoolLiteral (String Lexeme, SourcePos pos) {
    super (pos);
    this.Lexeme = Lexeme;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
