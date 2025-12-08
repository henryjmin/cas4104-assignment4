package minic.astgen;

import minic.scanner.SourcePos;

public class StringLiteral extends Terminal {

  public StringLiteral (String Lexeme, SourcePos pos) {
    super (pos);
    this.Lexeme = Lexeme;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
