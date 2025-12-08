package minic.astgen;

import minic.scanner.SourcePos;

public class BoolType extends Type {

  public BoolType (SourcePos pos) {
    super (pos);
  }

  public void accept(Visitor v) {
    v.visit(this);
  }

  public boolean Tequal (Type t) {
    if (t != null && t instanceof ErrorType)
      return true;
    else
      return (t != null && t instanceof BoolType);
  }

  public boolean AssignableTo (Type t) {
    // BoolType assignable to t ?
    if (t != null && t instanceof ErrorType)
      return true;
    else
      return (t != null && (t instanceof BoolType));
  }

}
