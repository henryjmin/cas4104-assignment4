package minic.astgen;

import minic.scanner.SourcePos;

public class EmptyActualParam extends Expr {

  public EmptyActualParam (SourcePos pos) {
    super (pos);
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
