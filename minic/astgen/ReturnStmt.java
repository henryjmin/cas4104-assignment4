package minic.astgen;

import minic.scanner.SourcePos;

public class ReturnStmt extends Stmt {

  public Expr eAST;

  public ReturnStmt (Expr eAST, SourcePos pos) {
    super (pos);
    this.eAST = eAST;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
