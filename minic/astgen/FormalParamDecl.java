package minic.astgen;

import minic.scanner.SourcePos;

public class FormalParamDecl extends Decl {

  public Type astType;
  public ID astIdent;

  public FormalParamDecl (Type astType, ID astIdent, SourcePos pos) {
    super (pos);
    this.astType = astType;
    this.astIdent = astIdent;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
