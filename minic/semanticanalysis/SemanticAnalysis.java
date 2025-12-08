package minic.semanticanalysis;

import minic.ErrorReporter;
import minic.StdEnvironment;
import minic.astgen.*;
import minic.scanner.SourcePos;

/** SemanticAnalysis visitor class. */
public class SemanticAnalysis implements Visitor {

  private ErrorReporter reporter;
  private ScopeStack scopeStack;
  private boolean isFunctionBlock;
  private Type currentFunctionReturnType;

  /** Constructor of the SemanticAnalysis visitor class. */
  public SemanticAnalysis(ErrorReporter reporter) {
    this.reporter = reporter;
    this.scopeStack = new ScopeStack();
    // Here we enter the entities from the StdEnvironment into the scope stack:
    // The scope stack is on level 1 now (initial setting).
    scopeStack.enter("int", StdEnvironment.intTypeDecl);
    scopeStack.enter("bool", StdEnvironment.boolTypeDecl);
    scopeStack.enter("float", StdEnvironment.floatTypeDecl);
    scopeStack.enter("void", StdEnvironment.voidTypeDecl);
    scopeStack.enter("getInt", StdEnvironment.getInt);
    scopeStack.enter("putInt", StdEnvironment.putInt);
    scopeStack.enter("getBool", StdEnvironment.getBool);
    scopeStack.enter("putBool", StdEnvironment.putBool);
    scopeStack.enter("getFloat", StdEnvironment.getFloat);
    scopeStack.enter("putFloat", StdEnvironment.putFloat);
    scopeStack.enter("getString", StdEnvironment.getString);
    scopeStack.enter("putString", StdEnvironment.putString);
    scopeStack.enter("putLn", StdEnvironment.putLn);
  }

  /*
  //
  // Prints the name of a class,
  // usefull for debugging...
  //
  private void PrintClassName(AST t) {
  System.out.println("The class of " + t +
  " is " + t.getClass().getName());
  }
   */

  /** Method typeOfDecl(). 
   *
   *<p>For FunDecl, VarDecl and FormalParamDecl, this function returns
   * the type of the declaration.
   * 1) for functions declarations, this is the return type of the function
   * 2) for variable declarations, this is the type of the variable
   */
  private Type typeOfDecl(AST d) {
    Type t;
    if (d == null) {
      return StdEnvironment.errorType;
    }
    assert ((d instanceof FunDecl) || (d instanceof VarDecl)
        || (d instanceof FormalParamDecl));
    if (d instanceof FunDecl) {
      t = ((FunDecl) d).tAST;
    } else if (d instanceof VarDecl) {
      t = ((VarDecl) d).tAST;
    } else {
      t = ((FormalParamDecl) d).astType;
    }
    return t;
  }

  /** Method typeOfArrayType(AST d) returns the element type of an
   *  ArrayType AST node.
   */
  private Type typeOfArrayType(AST d) {
    assert (d != null);
    assert (d instanceof ArrayType);
    ArrayType t = (ArrayType) d;
    return t.astType;
  }

  /** Method hasIntOrFloatArgs(Operator op) returns true, if an operator
   * accepts integer or floating point arguments.
   * <int> x <int> -> <sometype>
   * <float> x <float> -> <sometype>
   */
  private boolean hasIntOrFloatArgs(Operator op) {
    return (op.Lexeme.equals("+")
            || op.Lexeme.equals("-")
            || op.Lexeme.equals("*")
            || op.Lexeme.equals("/")
            || op.Lexeme.equals("<")
            || op.Lexeme.equals("<=")
            || op.Lexeme.equals(">")
            || op.Lexeme.equals(">=")
            || op.Lexeme.equals("==")
            || op.Lexeme.equals("!="));
  }

  /** Method hasBoolArgs(Operator op) returns true, if an operator accepts
   * bool arguments.
   * <bool> x <bool> -> <sometype>
   */
  private boolean hasBoolArgs(Operator op) {
    return (op.Lexeme.equals("&&")
            || op.Lexeme.equals("||")
            || op.Lexeme.equals("!")
            || op.Lexeme.equals("!=")
            || op.Lexeme.equals("=="));
  }

  /* Method hasBoolReturnType(Operator op) returns true, if an operator
   * returns a bool value.
   *  <sometype> x <sometype> -> bool
   */
  private boolean hasBoolReturnType(Operator op) {
    return (op.Lexeme.equals("&&")
            || op.Lexeme.equals("||")
            || op.Lexeme.equals("!")
            || op.Lexeme.equals("!=")
            || op.Lexeme.equals("==")
            || op.Lexeme.equals("<")
            || op.Lexeme.equals("<=")
            || op.Lexeme.equals(">")
            || op.Lexeme.equals(">="));
  }

  /** Method i2f(Expr e) performs coercion of an integer-valued
   * expression e.
   * It creates an i2f operator and a unary expression.
   * Expression e becomes the expression-AST of this unary expression.
   * 
   *<p>:        Expr AST for e <int>
   * =>
   *            UnaryExpr <float>
   *              |     \
   *              |      \
   *              |       \
   *           i2f<int>   Expr AST for e <int>
   */
  private Expr i2f(Expr e) {
    Operator op = new Operator("i2f", new SourcePos());
    op.type = StdEnvironment.intType;
    UnaryExpr expAst = new UnaryExpr(op, e, new SourcePos());
    expAst.type = StdEnvironment.floatType;
    return expAst;
  }

  /** Method getNrOfFormalParams(FunDecl f) returns the number
   * of formal parameters for a function. E.g., for the following function
   * void foo (int a, bool b){}
   * the return value will be 2.
   * Note: this function assumes the AST tree layout from Assignment 3.
   */
  private int getNrOfFormalParams(FunDecl f) {
    int nrArgs = 0;
    Decl d = f.paramsAST;
    assert ((d instanceof EmptyFormalParamDecl)
            || (d instanceof FormalParamDeclSequence));
    if (d instanceof EmptyFormalParamDecl) {
      return 0;
    }
    while (d instanceof FormalParamDeclSequence) {
      nrArgs++;
      d = ((FormalParamDeclSequence) d).rAST;
      assert ((d instanceof EmptyFormalParamDecl)
              || (d instanceof FormalParamDeclSequence));
    }
    return nrArgs;
  }

  /** Method getFormalParam(FunDecl f, int nr) returns, for a function
   * declaration, the AST for the formal parameter nr (nr is the number
   * the parameter).
   * E.g., for the following function and nr=2,
   *    void foo (int a, bool b){}
   * the AST returned will be "bool b".
   * Note: this function assumes the AST tree layout from Assignment 3.
   */
  private FormalParamDecl getFormalParam(FunDecl f, int nr) {
    int funArgs = getNrOfFormalParams(f);
    assert (funArgs >= 0);
    assert (nr <= funArgs);
    FormalParamDeclSequence s = (FormalParamDeclSequence) f.paramsAST;
    for (int i = 1; i < nr; i++) {
      assert (s.rAST instanceof FormalParamDeclSequence);
      s = (FormalParamDeclSequence) s.rAST;
    }
    assert (s.lAST instanceof FormalParamDecl);
    return (FormalParamDecl) s.lAST;
  }

  /** Method getNrOfActualParams(CallExpr f) gets the number of actual
   * parameters of a function call expression:
   * Similar to GetNrOfFormalParams above.
   * Note: this function assumes the AST tree layout from Assignment 3.
   */
  private int getNrOfActualParams(CallExpr f) {
    int nrArgs = 0;
    Expr p = f.paramAST;
    assert ((p instanceof EmptyActualParam)
            || (p instanceof ActualParamSequence));
    if (p instanceof EmptyActualParam) {
      return 0;
    }
    while (p instanceof ActualParamSequence) {
      nrArgs++;
      p = ((ActualParamSequence) p).rAST;
      assert ((p instanceof EmptyActualParam)
              || (p instanceof ActualParamSequence));
    }
    return nrArgs;
  }

  /** Method getActualParam(CallExpr f, int nr), given a function
   * call expression, gets the actual parameter nr
   * (nr is the number of the parameter).
   * Similar to GetFormalParam above.
   * Note: this function assumes the AST tree layout from Assignment 3.
   */
  private ActualParam getActualParam(CallExpr f, int nr) {
    int acArgs = getNrOfActualParams(f);
    Expr p = f.paramAST;
    assert (acArgs >= 0);
    assert (nr <= acArgs);
    for (int i = 1; i < nr; i++) {
      assert (p instanceof ActualParamSequence);
      p = ((ActualParamSequence) p).rAST;
    }
    assert (((ActualParamSequence) p).lAST instanceof ActualParam);
    return (ActualParam) ((ActualParamSequence) p).lAST;
  }

  // Given a type t, this function can be used to print the type.
  // Useful for debuggging, a similar mechanism is used in the
  // TreeDrawer Visitor.
  /*
  private String getTypeTag(Type t) {
    String l = new String("");
    if (t == null) {
      l = new String("<?>");
    } else if (t.Tequal(StdEnvironment.intType)) {
      l = new String("<int>");
    } else if (t.Tequal(StdEnvironment.boolType)) {
      l = new String("<bool>");
    } else if (t.Tequal(StdEnvironment.floatType)) {
      l = new String("<float>");
    } else if (t.Tequal(StdEnvironment.stringType)) {
      l = new String("<string>");
    } else if (t.Tequal(StdEnvironment.voidType)) {
      l = new String("<void>");
    } else if (t instanceof ErrorType) {
      l = new String("<error>");
    } else {
      assert (false);
    }
    return l;
  }
  */

  // This array of strings contains the error messages that we generate
  // for errors detected during semantic analysis. These messages are
  // output using the ErrorReporter.
  // Example: reporter.reportError(errMsg[0], "", new SourcePos());
  //          will print "ERROR #0: main function is missing".
  private String[] errMsg = {
    "#0: main function missing",
    "#1: return type of main must be int",

    // defining occurrences of identifiers,
    // for local, global variables and for formal parameters:
    "#2: identifier redeclared",
    "#3: identifier declared void",
    "#4: identifier declared void[]",

    // applied occurrences of identifiers:
    "#5: undeclared identifier",

    // assignment statements:
    "#6: incompatible types for =",
    "#7: invalid lvalue in assignment",

    // expression types:
    "#8: incompatible type for return statement",
    "#9: incompatible types for binary operator",
    "#10: incompatible type for unary operator",

    // scalars:
    "#11: attempt to use a function as a scalar",

    // arrays:
    "#12: attempt to use scalar/function as an array",
    "#13: wrong type for element in array initializer",
    "#14: invalid initializer: array initializer for scalar",
    "#15: invalid initializer: scalar initializer for array",
    "#16: too many elements in array initializer",
    "#17: array subscript is not an integer",
    "#18: array size missing",

    // functions:
    "#19: attempt to reference a scalar/array as a function",

    // conditional expressions:
    "#20: \"if\" conditional is not of type boolean",
    "#21: \"for\" conditional is not of type boolean",
    "#22: \"while\" conditional is not of type boolean",

    // parameters:
    "#23: too many actual parameters",
    "#24: too few actual parameters",
    "#25: wrong type for actual parameter"
  };


  /** Method check().
   *
   *<p>Checks whether the source program, represented by its AST, satisfies the
   * language's scope rules and type rules.
   * Decorates the AST as follows:
   * (a) Each applied occurrence of an identifier or operator is linked to
   * the corresponding declaration of that identifier or operator.
   * (b) Each expression and value-or-variable-name is decorated by its type.
   */
  public void check(Program progAst) {
    visit(progAst);
    // STEP 3:
    // Check Error 0
    // 
    // Retrieve "main" from the scope stack. If it is not there (null is
    // returned, then the program does not contain a main function.

    /* Start of your code: */

    /* End of your code */
  }

  /** visit method for Program. */
  public void visit(Program x) {
    x.D.accept(this);
  }

  /** visit method for EmptyDecl. */
  public void visit(EmptyDecl x) {
  }

  /** visit method for FunDecl. */
  public void visit(FunDecl x) {
    currentFunctionReturnType = x.tAST;
    // STEP 1:
    // Enter this function in the scope stack. Return Error 2 if this
    // name is already present in this scope.

    /* Start of your code: */

    /* End of your code */

    // STEP 3:
    // Check Error 1: 
    // If this function is the "main" function, then ensure that
    // x.tAST is of type int.

    /* Start of your code: */

    /* End of your code */

    // STEP 1:
    // Open a new scope in the scope stack. This will be the scope for the
    // function's formal parameters and the function's body.
    // We will close this scope in the visit procedure of this
    // function's compound_stmt.

    /* Start of your code: */

    /* End of your code */


    // The following flag is needed when we visit compound statements {...},
    // to avoid opening a fresh scope for function bodies (because we have
    // already opened one, for the formal parameters).
    isFunctionBlock = true; // needed in {...}, to avoid opening a fresh scope.

    x.paramsAST.accept(this);
    x.stmtAST.accept(this);
  }

  /** visit method for TypeDecl. */
  public void visit(TypeDecl x) {
    assert (false); // TypeDecl nodes occur only in the StdEnvironment AST.
  }

  /** visit method for FormalParamDecl. */
  public void visit(FormalParamDecl x) {
    if (x.astType instanceof ArrayType) {
      ((ArrayType) x.astType).astExpr.accept(this);
    }
    // STEP 1:
    // Here we visit the declaration of a formal parameter. You should enter
    // the lexeme x.astIdent.Lexeme together with its declaration x into
    // the scope stack. If this name is already present in the current scope,
    // the scope stack enter method will return false. You should report
    // Error 2 in that case.

    /* Start of your code: */

    /* End of your code */

    // STEP 3:
    // Check that the formal parameter is not of type void or void[]. 
    // Report error messages 3 and 4 respectively:

    /* Start of your code: */

    /* End of your code */
  }

  /** visit method for FormalParamDeclSequence. */
  public void visit(FormalParamDeclSequence x) {
    x.lAST.accept(this);
    x.rAST.accept(this);
  }

  /** visit method for EmptyFormalParamDecl. */
  public void visit(EmptyFormalParamDecl x) {
  }

  /** visit method for StmtSequence. */
  public void visit(StmtSequence x) {
    x.s1AST.accept(this);
    x.s2AST.accept(this);
  }

  /** visit method for AssignStmt. */
  public void visit(AssignStmt x) {
    x.lAST.accept(this);
    x.rAST.accept(this);
    //STEP 2:
    // Here we type-check assignment statements
    // Two conditions must be ensured:
    // 1) The type of the right-hand side of the assignment statement
    //    (x.rAST.type) must be assignment-compatible
    //    to the left-hand side of the assignment statement.
    //    You can use x.rAST.type.AssignableTo to test assignment-compatibility
    //    of the type of the left-hand side (x.lAST.type).
    // 2) If 2 types are assignment-compatible, then we need to check
    //    whether a coercion from int to float is needed. You can use
    //    x.lAST.type.Tequal(StdEnvironment.floatType) to check whether
    //    the left-hand side is of type float. Check the right-hand side
    //    for type int and use i2f if a coercion is needed. Hint: the return
    //    statement uses a similar mechanism....
    // If conditions (1) is violated, you should report Error 6.

    /* Start of your code: */

    /* End of your code */

    if (!(x.lAST instanceof VarExpr) && !(x.lAST instanceof ArrayExpr)) {
      reporter.reportError(errMsg[7], "", x.lAST.pos);
    }
  }

  /** visit method for IfStmt. */
  public void visit(IfStmt x) {
    x.eAST.accept(this);
    //STEP 2:
    // Here we are visiting an if statement. If the condition x.eAST.type
    // is not of type bool, we have to issue Error 20. You can have a
    // look at "for" loops, which use a similar check for the loop condition.

    /* Start of your code: */

    /* End of your code */
    x.thenAST.accept(this);
    if (x.elseAST != null) {
      x.elseAST.accept(this);
    }
  }

  /** visit method for WhileStmt. */
  public void visit(WhileStmt x) {
    x.eAST.accept(this);
    //STEP 2:
    // Here we are visiting a while statement. If the loop condition
    // is not of type bool, we have to issue Error 22. You can have a
    // look at "for" loops which use a similar check.

    /* Start of your code: */

    /* End of your code */
    x.stmtAST.accept(this);
  }

  /** visit method for ForStmt. */
  public void visit(ForStmt x) {
    x.e1AST.accept(this);
    if (!(x.e2AST instanceof EmptyExpr)) {
      x.e2AST.accept(this);
      if (!x.e2AST.type.Tequal(StdEnvironment.boolType)) {
        reporter.reportError(errMsg[21], "", x.e2AST.pos);
      }
    }
    if (!(x.e3AST instanceof EmptyExpr)) {
      x.e3AST.accept(this);
    }
    x.stmtAST.accept(this);
  }

  /** visit method for ReturnStmt. */
  public void visit(ReturnStmt x) {
    // STEP 2:
    // The following code checks assignment-compatibility of the return
    // statement's expression with the return type of the function.
    // Uncomment this code
    // as soon as you have finished type-checking of expressions.
    /* START:
    if (x.eAST instanceof EmptyExpr) {
      // ``return;'' requires void function return type:
      if (!currentFunctionReturnType.Tequal(StdEnvironment.voidType)) {
        reporter.reportError(errMsg[8], "", x.eAST.pos);
      }
      return; // done -> early exit
    }
    //
    // Falling through here means x.eAST != EmptyExpr:
    //
    x.eAST.accept(this);
    if (x.eAST.type.AssignableTo(currentFunctionReturnType)) {
      // Check for type coercion: if the function returns float, but
      // the expression of the return statement is of type int, we
      // need to convert this expression to float.
      if (currentFunctionReturnType.Tequal(StdEnvironment.floatType)
          && x.eAST.type.Tequal(StdEnvironment.intType)) {
        // coercion of operand to int:
        x.eAST = i2f(x.eAST);
      }
    } else {
      reporter.reportError(errMsg[8], "", x.eAST.pos);
    }
    END */
  }

  /** visit method for CompoundStmt. */
  public void visit(CompoundStmt x) {
    /*
     * If this CompoundStmt is the CompoundStmt of a Function, then
     * we already opened the scope before visiting the formal parameters.
     * No need to open a scope in that case. Otherwise set isFunctionBlock
     * to false, to remember for nested {...}.
     *
     */
    if (isFunctionBlock) {
      isFunctionBlock = false; // nested {...} need to open their own scope.
    } else {
      // STEP 1:
      // Open a new scope for the compound statement (nested block within
      // a function body.

      /* Start of your code: */

      /* End of your code */
    }
    // STEP 1:
    // Invoke the semantic analysis visitor for the declarations and the
    // statements of this CompoundStmt. Hint: look up the file
    // AstGen/CompoundStmt.java to learn about the AST children of this node.

    /* Start of your code: */

    /* End of your code */

    // STEP 1:
    // Visiting of this {...} compound statement is done. Close the scope
    // for this compound statement (even if it represents a function body).

    /* Start of your code: */

    /* End of your code */
  }

  /** visit method for EmptyStmt. */
  public void visit(EmptyStmt x) {
  }

  /** visit method for EmptyCompoundStmt. */
  public void visit(EmptyCompoundStmt x) {
    // STEP 1:
    // Close this scope if this EmptyCompoundStmt is the body of
    // a function.

    /* Start of your code: */

    /* End of your code */
  }

  /** visit method for CallStmt. */
  public void visit(CallStmt x) {
    x.eAST.accept(this);
  }

  /** visit method for VarDecl. */
  public void visit(VarDecl x) {
    if (x.tAST instanceof ArrayType) {
      ((ArrayType) x.tAST).astExpr.accept(this);
    }
    if (!(x.eAST instanceof EmptyExpr)) {
      x.eAST.accept(this);
      if (x.tAST instanceof ArrayType) {
        //STEP 4:
        //
        // Array declarations.
        // Check for error messages 15, 16, 13.
        // Perform i2f coercion if necessary.

        /* Start of your code: */

        /* End of your code */
      } else {
        //STEP 4:
        //
        // Non-array declarations, i.e., scalar variables.
        // Check for error messages 14, 6.
        // Perform i2f coercion if necessary.

        /* Start of your code: */

        /* End of your code */
      }
    }
    //STEP 1:
    // Here we are visiting a variable declaration x.
    // Enter this variable into the scope stack. Like with formal parameters,
    // if an identifier of the same name is already present, then you should
    // report Error 2.

    /* Start of your code: */

    /* End of your code */

    // STEP 3:
    // Check that the variable is not of type void or void[]. 
    // Report error messages 3 and 4 respectively:

    /* Start of your code: */

    /* End of your code */
  }

  /** visit method for DeclSequence. */
  public void visit(DeclSequence x) {
    x.D1.accept(this);
    x.D2.accept(this);
  }

  /** visit method for VarExpr. */
  public void visit(VarExpr x) {
    x.Ident.accept(this);
    //STEP 2:
    // Here we are visiting a variable expression.
    // Its type is synthesized from the type of the applied occurrence
    // of its identifier. Use "instanceof" to find out whether x.Ident.declAST
    // is a function declaration (FunDecl). In that case you should report
    // Error 11 and set x.type to the error type from StdEnvironment.
    x.type = typeOfDecl(x.Ident.declAST);
    /* Start of your code: */

    /* End of your code */
  }

  /** visit method for AssignExpr. */
  public void visit(AssignExpr x) {
    x.lAST.accept(this);
    x.rAST.accept(this);
    if (x.rAST.type.AssignableTo(x.lAST.type)) {
      // check for type coercion:
      if (x.lAST.type.Tequal(StdEnvironment.floatType)
          && x.rAST.type.Tequal(StdEnvironment.intType)) {
        // coercion of right operand to int:
        x.rAST = i2f(x.rAST);
      }
    } else {
      reporter.reportError(errMsg[6], "", x.rAST.pos);
    }
    if (!(x.lAST instanceof VarExpr) && !(x.lAST instanceof ArrayExpr)) {
      reporter.reportError(errMsg[7], "", x.lAST.pos);
    }
  }

  /** visit method for IntExpr. */
  public void visit(IntExpr x) {
    //STEP 2:
    // Here we are visiting an integer literal. Set x.type of this
    // AST node to the int type from the standard environment
    // (StdEnvironment.intType).

    /* Start of your code: */

    /* End of your code */
  }

  /** visit method for FloatExpr. */
  public void visit(FloatExpr x) {
    //STEP 2:
    // Here we are visiting a float literal. Set x.type of this
    // AST node to the float type from the standard environment
    // (StdEnvironment.floatType).

    /* Start of your code: */

    /* End of your code */
  }

  /** visit method for BoolExpr. */
  public void visit(BoolExpr x) {
    //STEP 2:
    // Here we are visiting a bool literal. Set x.type of this
    // AST node to the bool type from the standard environment
    // (StdEnvironment.boolType).

    /* Start of your code: */

    /* End of your code */
  }

  /** visit method for StringExpr. */
  public void visit(StringExpr x) {
    //STEP 2:
    // Here we are visiting a string literal. Set x.type of this
    // AST node to the string type from the standard environment
    // (StdEnvironment.stringType).

    /* Start of your code: */

    /* End of your code */
  }

  /** visit method for ArrayExpr. */
  public void visit(ArrayExpr x) {
    x.idAST.accept(this);
    x.indexAST.accept(this);
    if (!x.indexAST.type.Tequal(StdEnvironment.intType)) {
      reporter.reportError(errMsg[17], "", x.indexAST.pos);
    }
    VarExpr ve = (VarExpr) x.idAST;
    if (!(typeOfDecl(ve.Ident.declAST) instanceof ArrayType)) {
      reporter.reportError(errMsg[12], "", x.pos);
      x.type = StdEnvironment.errorType; 
    } else {
      x.type = typeOfArrayType(x.idAST.type);
    }
  }

  /** visit method for BinaryExpr. */
  public void visit(BinaryExpr x) {
    x.lAST.accept(this);
    x.oAST.accept(this);
    x.rAST.accept(this);
    if (hasIntOrFloatArgs(x.oAST)) {
      if (x.lAST.type.Tequal(StdEnvironment.intType)
          && x.rAST.type.Tequal(StdEnvironment.intType)) {
        x.oAST.type = StdEnvironment.intType;
        if (hasBoolReturnType(x.oAST)) {
          x.type = StdEnvironment.boolType;
        } else {
          x.type = StdEnvironment.intType;
        }
        return;
      } else if (x.lAST.type.Tequal(StdEnvironment.floatType)
                 && x.rAST.type.Tequal(StdEnvironment.floatType)) {
        x.oAST.type = StdEnvironment.floatType;
        if (hasBoolReturnType(x.oAST)) {
          x.type = StdEnvironment.boolType;
        } else {
          x.type = StdEnvironment.floatType;
        }
        return;
      } else if (x.lAST.type.Tequal(StdEnvironment.intType)
                 && x.rAST.type.Tequal(StdEnvironment.floatType)) {
        // coercion of left operand to float:
        x.lAST = i2f(x.lAST);
        x.oAST.type = StdEnvironment.floatType;
        if (hasBoolReturnType(x.oAST)) {
          x.type = StdEnvironment.boolType;
        } else {
          x.type = StdEnvironment.floatType;
        }
        return;
      } else if (x.lAST.type.Tequal(StdEnvironment.floatType)
                 && x.rAST.type.Tequal(StdEnvironment.intType)) {
        // STEP 2:
        // This code is part of the type checking for binary
        // expressions. In this case,
        // the left-hand operand is float, the right-hand operand is int.
        // We have to type-cast the right operand to float.
        // This is the dual case to "int x float" above.

        /* Start of your code: */

        /* End of your code */
        return;
      }
      // BBURG, 19.12. 2012: fall through here means we have bool args.
      // following 'else' has thus been removed, to make e.g., true==true work.
    }
    if (hasBoolArgs(x.oAST)) {
      if (x.lAST.type.Tequal(StdEnvironment.boolType)
          && x.rAST.type.Tequal(StdEnvironment.boolType)) {
        x.oAST.type = StdEnvironment.intType; // !
        x.type = StdEnvironment.boolType;
        return;
      }
    }
    x.oAST.type = StdEnvironment.errorType;
    x.type = StdEnvironment.errorType;
    if (!((x.lAST.type instanceof ErrorType)
        || (x.rAST.type instanceof ErrorType))) {
      // Error not spurious, because AST children are ok.
      reporter.reportError(errMsg[9], "", x.pos);
    }
  }

  /** visit method for UnaryExpr. */
  public void visit(UnaryExpr x) {
    x.oAST.accept(this);
    x.eAST.accept(this);
    //STEP 2:
    // Here we synthesize the type attribute for a unary operator.
    // x.eAST.type contains the type of the subexpression of this
    // unary operator.
    //
    // If x.eAST is of type int or float, and if oAST is an operator
    // that supports these types, then x.oAST.type and x.type
    // have to be set to x.eAST.type.
    //
    // If x.eAST is of type bool, and if x.oAST is an operator that
    // supports bool, then x.type is bool, but  x.oAST.type is of type
    // int (because of the JVM convention to represent true and false
    // as ints.
    //
    // In all other cases, x.oAST.type and x.type have to be set to
    // errorType, and Error 10 must be reported.
    //
    // You can have a look at visit(BinaryExpr) for a similar, yet
    // slightly more complicated case.

    /* Start of your code: */

    /* End of your code */
  }

  /** visit method for EmptyExpr. */
  public void visit(EmptyExpr x) {
  }

  /** visit method for ActualParam. */
  public void visit(ActualParam x) {
    x.pAST.accept(this);
    x.type = x.pAST.type;
  }

  /** visit method for EmptyActualParam. */
  public void visit(EmptyActualParam x) {
  }

  /** visit method for ActualParamSequence. */
  public void visit(ActualParamSequence x) {
    x.lAST.accept(this);
    x.rAST.accept(this);
  }

  /** visit method for CallExpr. */
  public void visit(CallExpr x) {
    // Here we perform semantic analysis of function calls:
    x.type = StdEnvironment.errorType;
    x.idAST.accept(this);
    x.paramAST.accept(this);
    // Retrieve the declaration of x from the scope stack:
    Decl d = scopeStack.retrieve(x.idAST.Lexeme);
    // STEP 3:
    // Use "instanceof" to find out if D is a FunDecl. If not, report
    // Error 19 and *return*.
    // This check detects cases like
    //  int f; f(22);
    // where f is not a function. 

    /* Start of your code: */

    /* End of your code */
    FunDecl f = (FunDecl ) d;
    // STEP 2:
    // Check that the number of formal args from f and the number of actual
    // parameters of the function call x match.
    // Use the functions getNrOfFormalParams and
    // getNrOfActualParams from the beginning of this file to retrieve
    // the number of formal and actual parameters.

    /* Start of your code: */

    /* End of your code */

    // STEP 2:
    // Here we check that the types of the formal and actual parameters
    // match (Error 25). This is similar to type-checking the left-hand
    // and right-hand sides of assignment statements. Two steps need
    // to be carried out:
    //
    // (1)
    // Check that types of formal and actual args match: this means that
    // the actual parameter must be assignable to the formal parameter.
    // You can imagine passing an actual parameter to a formal parameter
    // like an assignment statement: formal_par = actual_par.
    //
    // (2)
    // Perform type coercion (int->float) of the *actual* parameter if necessary.
    //

    /* You can use the following code as part of your solution. Uncomment
     * the following code as soon as you have type-checking
     * of expressions working.
     *
     * Start of your code:
     */

    /*
    int nrFormalParams = getNrOfFormalParams(f);
    for (int i = 1; i <= nrFormalParams; i++) {
      FormalParamDecl form = getFormalParam(f, i);
      ActualParam act = getActualParam(x, i);
      Type formalT = form.astType;
      Type actualT = act.pAST.type;
      // ... 

    }
    */
    /* End of your code */

    // If we fall through here, no semantic error occurred -> set the 
    // return type of the call expression to the return type of
    // its function:
    x.type = typeOfDecl(f);
  }

  /** visit method for ExprSequence. */
  public void visit(ExprSequence x) {
    x.lAST.accept(this);
    x.rAST.accept(this);
  }

  /** visit method for ID. */
  public void visit(ID x) {
    // STEP 1:
    // Here we look up the declaration of an identifier
    // from the scope stack. If no declaration can be found on the
    // scope stack, you should report Error 5.
    Decl binding = scopeStack.retrieve(x.Lexeme);
    if (binding != null) {
      x.declAST = binding;
    }
    /* Start of your code: */

    /* End of your code */
  }

  /** visit method for Operator. */
  public void visit(Operator x) {

  } 

  /** visit method for IntLiteral. */
  public void visit(IntLiteral x) {

  } 

  /** visit method for FloatLiteral. */
  public void visit(FloatLiteral x) {

  } 

  /** visit method for BoolLiteral. */
  public void visit(BoolLiteral x) {

  } 

  /** visit method for StringLiteral. */
  public void visit(StringLiteral x) {

  } 

  /** visit method for IntType. */
  public void visit(IntType x) {

  }

  /** visit method for FloatType. */
  public void visit(FloatType x) {

  }

  /** visit method for BoolType. */
  public void visit(BoolType x) {

  }

  /** visit method for StringType. */
  public void visit(StringType x) {

  }

  /** visit method for VoidType. */
  public void visit(VoidType x) {

  }


  /** visit method for ArrayType. */
  public void visit(ArrayType x) {

  }

  /** visit method for ErrorType. */
  public void visit(ErrorType x) {

  }

}
