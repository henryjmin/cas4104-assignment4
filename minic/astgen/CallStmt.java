package minic.astgen;

import minic.scanner.SourcePos;

public class CallStmt extends Stmt {

  public Expr eAST;

  public CallStmt (Expr eAST, SourcePos pos) {
    super (pos);
    this.eAST = eAST;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }
}
