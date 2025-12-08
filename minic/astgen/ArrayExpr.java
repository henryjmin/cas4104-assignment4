package minic.astgen;

import minic.scanner.SourcePos;

public class ArrayExpr extends Expr {

  public Expr idAST;
  public Expr indexAST;

  public ArrayExpr (Expr idAST, Expr indexAST, SourcePos pos) {
    super (pos);
    this.idAST= idAST;
    this.indexAST = indexAST;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
