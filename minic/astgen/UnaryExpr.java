package minic.astgen;

import minic.scanner.SourcePos;

public class UnaryExpr extends Expr {

  public Expr eAST;
  public Operator oAST;

  public UnaryExpr (Operator oAST, Expr eAST, SourcePos pos) {
    super (pos);
    this.oAST = oAST;
    this.eAST = eAST;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }
}
