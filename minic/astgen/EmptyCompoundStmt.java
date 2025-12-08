package minic.astgen;

import minic.scanner.SourcePos;

public class EmptyCompoundStmt extends CompoundStmt {

  public EmptyCompoundStmt (SourcePos pos) {
    super (null, null, pos);
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
