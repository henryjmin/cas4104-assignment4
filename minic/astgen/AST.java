package minic.astgen;

import minic.scanner.SourcePos;

public abstract class AST {

  public SourcePos pos;

  public AST (SourcePos pos) {
    this.pos = new SourcePos();
    this.pos.startCol = pos.startCol;
    this.pos.endCol = pos.endCol;
    this.pos.startLine = pos.startLine;
    this.pos.endLine = pos.endLine;
  }

  public SourcePos getPosition() {
    return pos;
  }

  public abstract void accept(Visitor v);
}
