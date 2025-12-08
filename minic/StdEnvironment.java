package minic;

import minic.astgen.*;
import minic.scanner.SourcePos;

/** The StdEnvironment class that contains the AST that belongs to the
 * MiniC standard environment, i.e., built-in functions and types.
 */
public final class StdEnvironment {

  // The pre-defined language environment for MiniC:

  // ASTs representing the MiniC standard type declarations:
  public static TypeDecl intTypeDecl;
  public static TypeDecl boolTypeDecl;
  public static TypeDecl floatTypeDecl;
  public static TypeDecl stringTypeDecl;
  public static TypeDecl voidTypeDecl;
  public static TypeDecl errorTypeDecl;

  // ASTs representing the MiniC standard types:
  public static Type intType;
  public static Type boolType;
  public static Type floatType;
  public static Type stringType;
  public static Type voidType;
  public static Type errorType;

  // ASTs representing the declarations of our pre-defined MiniC functions:
  public static FunDecl getInt;
  public static FunDecl putInt;
  public static FunDecl getBool;
  public static FunDecl putBool;
  public static FunDecl getFloat;
  public static FunDecl putFloat;
  public static FunDecl getString;
  public static FunDecl putString;
  public static FunDecl putLn;

  public Program ast;
  private static SourcePos dummyPos = new SourcePos();

  /** Constructor of StdEnvironment class. */
  public StdEnvironment() {

    /*
     * Generate the declarations for the StdEnvironment,
     * generate an AST, so that it can be traversed and printed:
     *
     */
    intType = new IntType(dummyPos);
    boolType = new BoolType(dummyPos);
    floatType = new FloatType(dummyPos);
    stringType = new StringType(dummyPos);
    voidType = new VoidType(dummyPos);
    errorType = new ErrorType(dummyPos);

    putLn = new FunDecl(voidType,
        new ID("putLn", dummyPos),
        new EmptyFormalParamDecl(dummyPos),
        new EmptyCompoundStmt(dummyPos),
        dummyPos);
    DeclSequence d;
    d = new DeclSequence(putLn, new EmptyDecl(dummyPos), dummyPos);

    FormalParamDecl parmDecl;
    parmDecl = new FormalParamDecl(stringType,
        new ID("s", dummyPos),
        dummyPos);
    FormalParamDeclSequence parmSeq;
    parmSeq = new FormalParamDeclSequence(parmDecl,
        new EmptyFormalParamDecl(dummyPos),
        dummyPos);
    putString = new FunDecl(voidType,
        new ID("putString", dummyPos),
        parmSeq,
        new EmptyCompoundStmt(dummyPos),
        dummyPos);
    d = new DeclSequence(putString, d, dummyPos);

    getString = new FunDecl(stringType,
        new ID("getString", dummyPos),
        new EmptyFormalParamDecl(dummyPos),
        new EmptyCompoundStmt(dummyPos),
        dummyPos);
    d = new DeclSequence(getString, d, dummyPos);

    parmDecl = new FormalParamDecl(floatType,
        new ID("f", dummyPos),
        dummyPos);
    parmSeq = new FormalParamDeclSequence(parmDecl,
        new EmptyFormalParamDecl(dummyPos),
        dummyPos);
    putFloat = new FunDecl(voidType,
        new ID("putFloat", dummyPos),
        parmSeq,
        new EmptyCompoundStmt(dummyPos),
        dummyPos);
    d = new DeclSequence(putFloat, d, dummyPos);

    getFloat = new FunDecl(floatType,
        new ID("getFloat", dummyPos),
        new EmptyFormalParamDecl(dummyPos),
        new EmptyCompoundStmt(dummyPos),
        dummyPos);
    d = new DeclSequence(getFloat, d, dummyPos);

    parmDecl = new FormalParamDecl(boolType,
        new ID("b", dummyPos),
        dummyPos);
    parmSeq = new FormalParamDeclSequence(parmDecl,
        new EmptyFormalParamDecl(dummyPos),
        dummyPos);
    putBool = new FunDecl(voidType,
        new ID("putBool", dummyPos),
        parmSeq,
        new EmptyCompoundStmt(dummyPos),
        dummyPos);
    d = new DeclSequence(putBool, d, dummyPos);

    getBool = new FunDecl(boolType,
        new ID("getBool", dummyPos),
        new EmptyFormalParamDecl(dummyPos),
        new EmptyCompoundStmt(dummyPos),
        dummyPos);
    d = new DeclSequence(getBool, d, dummyPos);


    parmDecl = new FormalParamDecl(intType,
        new ID("i", dummyPos),
        dummyPos);
    parmSeq = new FormalParamDeclSequence(parmDecl,
        new EmptyFormalParamDecl(dummyPos),
        dummyPos);
    putInt = new FunDecl(voidType,
        new ID("putInt", dummyPos),
        parmSeq,
        new EmptyCompoundStmt(dummyPos), dummyPos);
    d = new DeclSequence(putInt, d, dummyPos);

    getInt = new FunDecl(intType,
        new ID("getInt", dummyPos),
        new EmptyFormalParamDecl(dummyPos),
        new EmptyCompoundStmt(dummyPos),
        dummyPos);
    d = new DeclSequence(getInt, d, dummyPos);

    errorTypeDecl = new TypeDecl(errorType, dummyPos);
    d = new DeclSequence(errorTypeDecl, d, dummyPos);
    voidTypeDecl = new TypeDecl(voidType, dummyPos);
    d = new DeclSequence(voidTypeDecl, d, dummyPos);
    stringTypeDecl = new TypeDecl(stringType, dummyPos);
    d = new DeclSequence(stringTypeDecl, d, dummyPos);
    floatTypeDecl = new TypeDecl(floatType, dummyPos);
    d = new DeclSequence(floatTypeDecl, d, dummyPos);
    boolTypeDecl = new TypeDecl(boolType, dummyPos);
    d = new DeclSequence(boolTypeDecl, d, dummyPos);
    intTypeDecl = new TypeDecl(intType, dummyPos);
    d = new DeclSequence(intTypeDecl, d, dummyPos);

    ast = new Program(d, dummyPos);

  }

}
