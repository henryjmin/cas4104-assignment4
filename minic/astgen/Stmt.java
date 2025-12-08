package minic.astgen;

import minic.scanner.SourcePos;

public abstract class Stmt extends AST {

  public Stmt (SourcePos pos) {
    super (pos);
  }

}
