package minic.astgen;

import minic.scanner.SourcePos;

public class EmptyFormalParamDecl extends FormalParamDecl {

  public EmptyFormalParamDecl (SourcePos pos) {
    super (null, null, pos);
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
