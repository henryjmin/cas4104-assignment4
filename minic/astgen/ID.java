package minic.astgen;

import minic.scanner.SourcePos;

public class ID extends Terminal {

  public AST declAST;

  public ID (String Lexeme, SourcePos pos) {
    super(pos);
    this.Lexeme = Lexeme;
    declAST = null;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
