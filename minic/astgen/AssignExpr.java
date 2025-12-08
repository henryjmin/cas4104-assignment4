package minic.astgen;

import minic.scanner.SourcePos;

public class AssignExpr extends Expr {

  public Expr lAST;
  public Expr rAST;

  public AssignExpr (Expr lAST, Expr rAST, SourcePos pos) {
    super (pos);
    this.lAST = lAST;
    this.rAST = rAST;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
