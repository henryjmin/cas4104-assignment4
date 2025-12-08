package minic.astgen;

import minic.scanner.SourcePos;

public abstract class Expr extends AST {

  public Type type;

  public Expr (SourcePos pos) {
    super (pos);
  }

}
