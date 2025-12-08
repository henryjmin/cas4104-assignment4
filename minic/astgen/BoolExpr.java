package minic.astgen;

import minic.scanner.SourcePos;

public class BoolExpr extends Expr {

  public BoolLiteral astBL;

  public BoolExpr (BoolLiteral astBL, SourcePos pos) {
    super (pos);
    this.astBL = astBL;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
