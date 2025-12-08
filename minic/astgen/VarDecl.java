package minic.astgen;

import minic.scanner.SourcePos;

public class VarDecl extends Decl {

  public Type tAST;
  public ID idAST;
  public Expr eAST;

  public VarDecl (Type tAST, ID idAST, Expr eAST, SourcePos pos) {
    super (pos);
    this.tAST = tAST;
    this.idAST = idAST;
    this.eAST = eAST;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
