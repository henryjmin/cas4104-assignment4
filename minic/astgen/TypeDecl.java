package minic.astgen;

import minic.scanner.SourcePos;

public class TypeDecl extends Decl {

  public Type tAST;

  public TypeDecl(Type tAST, SourcePos pos) {
    super(pos);
    this.tAST = tAST;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
