package minic.astgen;

import minic.scanner.SourcePos;

public class EmptyStmt extends Stmt {

  public EmptyStmt (SourcePos pos) {
    super (pos);
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
