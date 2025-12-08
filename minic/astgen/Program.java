package minic.astgen;

import minic.scanner.SourcePos;

public class Program extends AST {

  public Decl D;

  public Program (Decl D, SourcePos pos) {
    super (pos);
    this.D = D;
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

}
