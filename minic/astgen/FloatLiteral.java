package minic.astgen;

import minic.scanner.SourcePos;

public class FloatLiteral extends Terminal {

  public FloatLiteral (String Lexeme, SourcePos pos) {
    super (pos);
    this.Lexeme = Lexeme;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
