package minic.astgen;

import minic.scanner.SourcePos;

public class ErrorType extends Type {

  public ErrorType (SourcePos pos) {
    super (pos);
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

  public boolean Tequal (Type t) {
    if (t != null && t instanceof ErrorType)
      return true;
    else
      return false;
  }

  public boolean AssignableTo (Type t) {
    return true;
  }

}
