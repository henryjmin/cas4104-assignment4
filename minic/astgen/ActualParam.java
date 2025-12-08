package minic.astgen;

import minic.scanner.SourcePos;

public class ActualParam extends Expr {

  public Expr pAST;

  public ActualParam (Expr pAST, SourcePos pos) {
    super(pos);
    this.pAST = pAST;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }
}
