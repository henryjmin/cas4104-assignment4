package minic.astgen;

import minic.scanner.SourcePos;

public class ActualParamSequence extends Expr {

  public Expr lAST, rAST;

  public ActualParamSequence (Expr lAST, Expr rAST, SourcePos pos) {
    super (pos);
    this.lAST = lAST;
    this.rAST = rAST;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }
}
