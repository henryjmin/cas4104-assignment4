package minic.astgen;

import minic.scanner.SourcePos;

public class VarExpr extends Expr {

  public ID Ident;

  public VarExpr (ID Ident, SourcePos pos) {
    super (pos);
    this.Ident = Ident;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }
}
