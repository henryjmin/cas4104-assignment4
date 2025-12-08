package minic.astgen;

import minic.scanner.SourcePos;

public class FloatExpr extends Expr {

  public FloatLiteral astFL;

  public FloatExpr (FloatLiteral astFL, SourcePos pos) {
    super (pos);
    this.astFL = astFL;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
