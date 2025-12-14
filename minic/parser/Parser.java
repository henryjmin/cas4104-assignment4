package minic.parser;

import minic.ErrorReporter;
import minic.astgen.*;
import minic.parser.SyntaxError;
import minic.scanner.Scanner;
import minic.scanner.SourcePos;
import minic.scanner.Token;

/** Parser class to perform syntax analysis of a MiniC program. */


public class Parser {

  private Scanner scanner;
  private ErrorReporter errorReporter;
  private Token currentToken;
  private SourcePos previousTokenPosition;

  /** Constructor.
   *
   * @param lexer is the scanner provided to the parser.
   * @param reporter is the ErrorReporter object to report syntax errors.
   */
  public Parser(Scanner lexer, ErrorReporter reporter) {
    scanner = lexer;
    errorReporter = reporter;
  }

  // accept() checks whether the current token matches tokenExpected.
  // If so, it fetches the next token.
  // If not, it reports a syntax error.
  private void accept(int tokenExpected) throws SyntaxError {
    if (currentToken.kind == tokenExpected) {
      previousTokenPosition = currentToken.getSourcePos();
      currentToken = scanner.scan();
    } else {
      syntaxError("\"%\" expected here", Token.spell(tokenExpected));
    }
  }

  // acceptIt() unconditionally accepts the current token
  // and fetches the next token from the scanner.
  private void acceptIt() {
    previousTokenPosition = currentToken.getSourcePos();
    currentToken = scanner.scan();
  }

  // start records the position of the start of a phrase.
  // This is defined to be the position of the first
  // character of the first token of the phrase.
  private void start(SourcePos pos) {
    pos.startCol = currentToken.getSourcePos().startCol;
    pos.startLine = currentToken.getSourcePos().startLine;
  }

  // finish records the position of the end of a phrase.
  // This is defined to be the position of the last
  // character of the last token of the phrase.
  private void finish(SourcePos pos) {
    pos.endCol = previousTokenPosition.endCol;
    pos.endLine = previousTokenPosition.endLine;
  }

  private void syntaxError(String messageTemplate, String tokenQuoted) throws SyntaxError {
    SourcePos pos = currentToken.getSourcePos();
    errorReporter.reportError(messageTemplate, tokenQuoted, pos);
    throw(new SyntaxError());
  }

  boolean isTypeSpecifier(int token) {
    if (token == Token.VOID
        || token == Token.INT
        || token == Token.BOOL
        || token == Token.FLOAT) {
      return true;
    } else {
      return false;
    }
  }

  /**
   * parseArrayIndexDecl(): helper to take [INTLITERAL] and generate an
   * ArrayType. 
   */
  private ArrayType parseArrayIndexDecl(Type t, SourcePos allPos) throws SyntaxError {
    accept(Token.LEFTBRACKET);
    SourcePos pos = currentToken.getSourcePos();
    IntLiteral l;
    l = new IntLiteral(currentToken.getLexeme(), pos);
    accept(Token.INTLITERAL);
    accept(Token.RIGHTBRACKET);
    finish(pos);
    finish(allPos);
    IntExpr ie = new IntExpr(l, pos);
    return new ArrayType(t, ie, allPos);
  }

  /**
   * parse(): public-facing top-level parsing routine.
   */
  public Program parse() { // called from the MiniC driver

    Program progAst = null;

    previousTokenPosition = new SourcePos();
    previousTokenPosition.startLine = 0;
    previousTokenPosition.startCol = 0;
    previousTokenPosition.endLine = 0;
    previousTokenPosition.endCol = 0;

    currentToken = scanner.scan(); // get first token from scanner...

    try {
      progAst = parseProgram();
      if (currentToken.kind != Token.EOF) {
        syntaxError("\"%\" not expected after end of program",
            currentToken.getLexeme());
      }
    } catch (SyntaxError s) {
      return null;
    }
    return progAst;
  }

  /**
   * parseProgram(): parses the entire MiniC program.
   *
   *<p>program ::= ( (VOID|INT|BOOL|FLOAT) ID ( FunPart | VarPart ) )*
   */

  // parseProgDecls: recursive helper function to facilitate AST construction.
  private Decl parseProgDecls() throws SyntaxError {
    if (! isTypeSpecifier(currentToken.kind)) {
      return new EmptyDecl(previousTokenPosition);
    }
    SourcePos pos = new SourcePos();
    start(pos);
    Type t = parseTypeSpecifier();
    ID id = parseId();
    if (currentToken.kind == Token.LEFTPAREN) {
      Decl newD = parseFunPart(t, id, pos);
      return new DeclSequence(newD, parseProgDecls(), previousTokenPosition);
    } else {
      DeclSequence vars = parseVarPart(t, id, pos);
      DeclSequence varsTail = vars.GetRightmostDeclSequenceNode();
      Decl remainderDecls = parseProgDecls();
      varsTail.SetRightSubtree(remainderDecls);
      return vars;
    }
  }

  private Program parseProgram() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Decl d = parseProgDecls();
    finish(pos);
    Program p = new Program(d, pos);
    return p;
  }

  /**
   * parseFunPart(): parses the ``function'' part of a declaration.
   *
   *<p>FunPart ::= ( "(" ParamsList? ")" CompoundStmt )
   */
  private Decl parseFunPart(Type t, ID id, SourcePos pos) throws SyntaxError {
    // We already know that the current token is "(".
    // Otherwise use accept()
    acceptIt();
    Decl parDecl = parseParamsList(); // can also be empty...
    accept(Token.RIGHTPAREN);
    CompoundStmt compStmt = parseCompoundStmt();
    finish(pos);
    return new FunDecl(t, id, parDecl, compStmt, pos);
  }

  /**
   * parseParamsList(): parses the parameter declarations of a function.
   *
   *<p>ParamsList ::= ParameterDecl ( "," ParameterDecl ) *
   */
  private Decl parseParamsList() throws SyntaxError {
    if (!isTypeSpecifier(currentToken.kind)) {
      return new EmptyFormalParamDecl(previousTokenPosition);
    }
    Decl decl1 = parseParameterDecl();
    Decl declR = new EmptyFormalParamDecl(previousTokenPosition);
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
      declR = parseParamsList();
      if (declR instanceof EmptyFormalParamDecl) {
        syntaxError("Declaration after comma expected", "");
      }
    }
    return new FormalParamDeclSequence(decl1, declR, previousTokenPosition);
  }

  /**
   * parseParameterDecl(): parses a MiniC parameter declaration.
   *
   *<p>ParameterDecl ::= (VOID|INT|BOOL|FLOAT) Declarator
   */
  private Decl parseParameterDecl() throws SyntaxError {
    Type t = null;
    Decl d = null;

    SourcePos pos = new SourcePos();
    start(pos);
    if (isTypeSpecifier(currentToken.kind)) {
      t = parseTypeSpecifier();
    } else {
      syntaxError("Type specifier instead of % expected",
          Token.spell(currentToken.kind));
    }
    d = parseDeclarator(t, pos);
    return d;
  }

  /**
   * parseDeclarator(): parses the declarator part of a declaration.
   *
   *<p>Declarator ::= ID ( "[" INTLITERAL "]" )?
   */
  private Decl parseDeclarator(Type t, SourcePos pos) throws SyntaxError {
    ID id = parseId();
    if (currentToken.kind == Token.LEFTBRACKET) {
      ArrayType arrT = parseArrayIndexDecl(t, pos);
      finish(pos);
      return new FormalParamDecl(arrT, id, pos);
    }
    finish(pos);
    return new FormalParamDecl(t, id, pos);
  }

  /**
   * parseVarPart(): parses variable declaration past the ID.
   *
   *<p>VarPart ::= ( "[" INTLITERAL "]" )?  ( "=" initializer ) ? ( "," init_decl)* ";"
   */

  // Recursive helper method to parse ( "," init_decl)*
  private Decl parseInitDeclList(Type t) throws SyntaxError {
    if (currentToken.kind != Token.COMMA) {
      return new EmptyDecl(previousTokenPosition);
    }
    acceptIt();
    Decl d = parseInitDecl(t);
    return new DeclSequence(d, parseInitDeclList(t), previousTokenPosition);
  }

  // InitDecl ::= ID ("[" INTLITERAL "]")? ("=" Initializer)?
  private Decl parseInitDecl(Type t) throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    ID id = parseId();
    if (currentToken.kind == Token.LEFTBRACKET) {
      t = parseArrayIndexDecl(t, pos);
    }
    Expr e = new EmptyExpr(previousTokenPosition);
    if (currentToken.kind == Token.ASSIGN) {
      acceptIt();
      e = parseInitializer();
    }
    finish(pos);
    return new VarDecl(t, id, e, pos);
  }

  // Exprs ::= ("," Expr)*
  private Expr parseExprs() throws SyntaxError {
    if (currentToken.kind != Token.COMMA) {
      return new EmptyExpr(previousTokenPosition);
    }
    acceptIt();
    Expr e = parseExpr();
    return new ExprSequence(e, parseExprs(), previousTokenPosition);
  }

  // Initializer ::= Expr | "{" Expr ("," Expr)* "}"
  private Expr parseInitializer() throws SyntaxError {
    SourcePos pos = new SourcePos();
    if (currentToken.kind == Token.LEFTBRACE) {
      acceptIt();
      Expr e = parseExpr();
      Expr exprs = new ExprSequence(e, parseExprs(), previousTokenPosition);
      accept(Token.RIGHTBRACE);
      return exprs;
    } else {
      return parseExpr();
    }
  }

  private DeclSequence parseVarPart(Type t, ID id, SourcePos pos) throws SyntaxError {
    Type theType = t;
    Expr e = new EmptyExpr(previousTokenPosition);
    if (currentToken.kind == Token.LEFTBRACKET) {
      theType = parseArrayIndexDecl(t, pos);
    }
    if (currentToken.kind == Token.ASSIGN) {
      acceptIt();
      e = parseInitializer();
    }
    finish(pos); 
    Decl d = new VarDecl(theType, id, e, pos);
    DeclSequence seq = null;
    seq = new DeclSequence(d, parseInitDeclList(t), previousTokenPosition);
    accept(Token.SEMICOLON);
    return seq;
  }

  // Expr ::= AndExpr ("||" AndExpr)*
  private Expr parseExpr() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Expr e1 = parseAndExpr();
    while (currentToken.kind == Token.OR) {
      Operator op = new Operator(currentToken.getLexeme(), currentToken.getSourcePos());
      acceptIt();
      Expr e2 = parseAndExpr();
      finish(pos);
      e1 = new BinaryExpr(e1, op, e2, pos);
    }
    return e1;
  }

  // AndExpr ::= RelationalExpr ("&&" RelationalExpr)*
  private Expr parseAndExpr() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Expr e1 = parseRelationalExpr();
    while (currentToken.kind == Token.AND) {
      Operator op = new Operator(currentToken.getLexeme(), currentToken.getSourcePos());
      acceptIt();
      Expr e2 = parseRelationalExpr();
      finish(pos);
      e1 = new BinaryExpr(e1, op, e2, pos);
    }
    return e1;
  }

  // RelationalExpr ::= AddExpr (("==" | "!=" | "<" | "<=" | ">" | ">=") AddExpr)?
  private Expr parseRelationalExpr() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Expr e1 = parseAddExpr();
    if (currentToken.kind == Token.EQ ||
        currentToken.kind == Token.NOTEQ ||
        currentToken.kind == Token.LESS ||
        currentToken.kind == Token.LESSEQ ||
        currentToken.kind == Token.GREATER ||
        currentToken.kind == Token.GREATEREQ) {
      Operator op = new Operator(currentToken.getLexeme(), currentToken.getSourcePos());
      acceptIt();
      Expr e2 = parseAddExpr();
      finish(pos);
      return new BinaryExpr(e1, op, e2, pos);
    }
    return e1;
  }

  // AddExpr ::= MultExpr (("+" | "-") MultExpr)*
  private Expr parseAddExpr() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Expr e1 = parseMultExpr();
    while (currentToken.kind == Token.PLUS || currentToken.kind == Token.MINUS) {
      Operator op = new Operator(currentToken.getLexeme(), currentToken.getSourcePos());
      acceptIt();
      Expr e2 = parseMultExpr();
      finish(pos);
      e1 = new BinaryExpr(e1, op, e2, pos);
    }
    return e1;
  }

  // MultExpr ::= UnaryExpr (("*" | "/") UnaryExpr)*
  private Expr parseMultExpr() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Expr e1 = parseUnaryExpr();
    while (currentToken.kind == Token.TIMES || currentToken.kind == Token.DIV) {
      Operator op = new Operator(currentToken.getLexeme(), currentToken.getSourcePos());
      acceptIt();
      Expr e2 = parseUnaryExpr();
      finish(pos);
      e1 = new BinaryExpr(e1, op, e2, pos);
    }
    return e1;
 }

  /**
   * parseUnaryExpr(): parses a MiniC unary expression.
   *
   *<p>UnaryExpr ::= ("+"|"-"|"!")* PrimaryExpr
   */
  private Expr parseUnaryExpr() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    if (currentToken.kind == Token.PLUS
        || currentToken.kind == Token.MINUS
        || currentToken.kind == Token.NOT) {
      Operator opAst = new Operator(currentToken.getLexeme(),
          currentToken.getSourcePos());
      acceptIt();
      Expr tmp = parseUnaryExpr();
      finish(pos);
      return new UnaryExpr(opAst, tmp, pos);
    }
    return parsePrimaryExpr();
  }

  /**
   * parsePrimaryExpr(): parses a MiniC primary expression.
   *
   *<p>PrimaryExpr ::= ID arglist?
   *                | ID "[" expr "]"
   *                |  "(" expr ")"
   *                |  INTLITERAL | BOOLLITERAL | FLOATLITERAL | STRINGLITERAL
   */
  private Expr parsePrimaryExpr() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    switch (currentToken.kind) {
      case Token.INTLITERAL: {
        IntLiteral intLiteral = new IntLiteral(currentToken.getLexeme(), currentToken.getSourcePos());
        acceptIt();
        finish(pos);
        return new IntExpr(intLiteral, pos);
      }
      case Token.BOOLLITERAL: {
        BoolLiteral boolLiteral = new BoolLiteral(currentToken.getLexeme(), currentToken.getSourcePos());
        acceptIt();
        finish(pos);
        return new BoolExpr(boolLiteral, pos);
      }
      case Token.FLOATLITERAL: {
        FloatLiteral floatLiteral = new FloatLiteral(currentToken.getLexeme(), currentToken.getSourcePos());
        acceptIt();
        finish(pos);
        return new FloatExpr(floatLiteral, pos);
      }
      case Token.STRINGLITERAL: {
        StringLiteral stringLiteral = new StringLiteral(currentToken.getLexeme(), currentToken.getSourcePos());
        acceptIt();
        finish(pos);
        return new StringExpr(stringLiteral, pos);
      }
      case Token.LEFTPAREN: {
        acceptIt();
        Expr expr = parseExpr();
        accept(Token.RIGHTPAREN);
        finish(pos);
        return expr;
      }
      case Token.ID: {
        ID id = parseId();
        Expr var = new VarExpr(id, id.getPosition());
        if (currentToken.kind == Token.LEFTPAREN) {
          Expr args = parseArgList();
          finish(pos);
          return new CallExpr(id, args, pos);
        } else if (currentToken.kind == Token.LEFTBRACKET) {
          acceptIt();
          Expr index = parseExpr();
          accept(Token.RIGHTBRACKET);
          finish(pos);
          return new ArrayExpr(var, index, pos);
        } else {
          return var;
        }
      }
      default:
        syntaxError("\"%\" not a primary expression", currentToken.getLexeme());
        return null;
    }
  }

  /**
   * parseCompoundStmt(): parses a MiniC compound statement.
   *
   *<p>CompoundStmt ::= "{" VariableDef* Stmt* "}"
   */

  // Recursive helper function parseCompoundDecls():
  private Decl parseCompoundDecls() throws SyntaxError {
    if (!isTypeSpecifier(currentToken.kind)) {
      return new EmptyDecl(previousTokenPosition);
    }
    SourcePos pos = new SourcePos();
    start(pos);
    Type t = parseTypeSpecifier();
    ID id = parseId();
    DeclSequence vars = parseVarPart(t, id, pos);
    DeclSequence varsTail = vars.GetRightmostDeclSequenceNode();
    Decl remainderDecls = parseCompoundDecls();
    varsTail.SetRightSubtree(remainderDecls);
    return vars;
  }

  // Recursive helper function parseCompoundStmts():
  private Stmt parseCompoundStmts() throws SyntaxError {
    if (! (currentToken.kind == Token.LEFTBRACE
          || currentToken.kind == Token.IF
          || currentToken.kind == Token.WHILE
          || currentToken.kind == Token.FOR
          || currentToken.kind == Token.RETURN
          || currentToken.kind == Token.ID)
    ) {
      return new EmptyStmt(previousTokenPosition);
    }
    SourcePos pos = new SourcePos();
    start(pos);
    Stmt st = null;
    st = parseStmt();
    Stmt stRest = parseCompoundStmts();
    finish(pos);
    return new StmtSequence(st, stRest, pos);
  }

  private CompoundStmt parseCompoundStmt() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    accept(Token.LEFTBRACE);
    Decl d = parseCompoundDecls();
    Stmt s = parseCompoundStmts();
    accept(Token.RIGHTBRACE);
    finish(pos);
    if ((d.getClass() == EmptyDecl.class)
        && (s.getClass() == EmptyStmt.class)) {
      return new EmptyCompoundStmt(previousTokenPosition);
    } else {
      return new CompoundStmt(d, s, pos);
    }
  }

  // Stmt ::= CompoundStmt | IfStmt | WhileStmt | ForStmt | RETURN Expr? ";"
  //          | ID ("=" Expr ";" | "[" Expr "]" "=" Expr ";" | Arglist ";")
  private Stmt parseStmt() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    switch (currentToken.kind) {
      case Token.LEFTBRACE:
        return parseCompoundStmt();
      case Token.IF:
        return parseIfStmt();
      case Token.WHILE:
        return parseWhileStmt();
      case Token.FOR:
        return parseForStmt();
      case Token.RETURN: {
        acceptIt();
        Expr retExpr = new EmptyExpr(previousTokenPosition);
        if (currentToken.kind != Token.SEMICOLON) {
          retExpr = parseExpr();
        }
        accept(Token.SEMICOLON);
        finish(pos);
        return new ReturnStmt(retExpr, pos);
      }
      case Token.ID: {
        ID id = parseId();
        Expr var = new VarExpr(id, id.getPosition());
        if (currentToken.kind == Token.ASSIGN) {
          acceptIt();
          Expr rvalue = parseExpr();
          accept(Token.SEMICOLON);
          finish(pos);
          return new AssignStmt(var, rvalue, pos);
        } else if (currentToken.kind == Token.LEFTBRACKET) {
          acceptIt();
          Expr index = parseExpr();
          accept(Token.RIGHTBRACKET);
          accept(Token.ASSIGN);
          Expr rvalue = parseExpr();
          accept(Token.SEMICOLON);
          finish(pos);
          var = new ArrayExpr(var, index, pos);
          finish(pos);
          return new AssignStmt(var, rvalue, pos);
        } else {
          Expr args = parseArgList();
          finish(pos);
          Expr callExpr = new CallExpr(id, args, pos);
          accept(Token.SEMICOLON);
          finish(pos);
          return new CallStmt(callExpr, pos);
        }
      }
      default:
        syntaxError("\"%\" not a statement", currentToken.getLexeme());
        return null;
    }
  }

  // IfStmt ::= IF "(" Expr ")" Stmt (ELSE Stmt)?
  private Stmt parseIfStmt() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    accept(Token.IF);
    accept(Token.LEFTPAREN);
    Expr expr = parseExpr();
    accept(Token.RIGHTPAREN);
    Stmt thenStmt = parseStmt();
    if (currentToken.kind == Token.ELSE) {
      acceptIt();
      Stmt elseStmt = parseStmt();
      finish(pos);
      return new IfStmt(expr, thenStmt, elseStmt, pos);
    }
    finish(pos);
    return new IfStmt(expr, thenStmt, pos);
  }

  // WhileStmt ::= WHILE "(" Expr ")" Stmt
  private Stmt parseWhileStmt() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    accept(Token.WHILE);
    accept(Token.LEFTPAREN);
    Expr expr = parseExpr();
    accept(Token.RIGHTPAREN);
    Stmt stmt = parseStmt();
    finish(pos);
    return new WhileStmt(expr, stmt, pos);
  }

  // ForStmt ::= FOR "(" Asgnexpr? ";" Expr? ";" Asgnexpr? ")" stmt
  private Stmt parseForStmt() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    accept(Token.FOR);
    accept(Token.LEFTPAREN);
    Expr e1 = new EmptyExpr(previousTokenPosition);
    if (currentToken.kind == Token.ID) {
      e1 = parseAsgnexpr();
    }
    accept(Token.SEMICOLON);
    Expr e2 = new EmptyExpr(previousTokenPosition);
    if (currentToken.kind != Token.SEMICOLON) {
      e2 = parseExpr();
    }
    accept(Token.SEMICOLON);
    Expr e3 = new EmptyExpr(previousTokenPosition);
    if (currentToken.kind == Token.ID) {
      e3 = parseAsgnexpr();
    }
    accept(Token.RIGHTPAREN);
    Stmt stmt = parseStmt();
    finish(pos);
    return new ForStmt(e1, e2, e3, stmt, pos);
  }

  private Expr parseAsgnexpr() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    ID id = parseId();
    Expr var = new VarExpr(id, id.getPosition());
    accept(Token.ASSIGN);
    Expr rvalue = parseExpr();
    finish(pos);
    return new AssignExpr(var, rvalue, pos);
  }

  /**
   * parseArgList() parses a MiniC procedure arg list.
   *
   *<p>ArgList ::= "(" ( arg ( "," arg )* )? ")"
   */

  // Recursive helper function to parse args:
  private Expr parseArgs() throws SyntaxError {
    if (currentToken.kind == Token.RIGHTPAREN) {
      return new  EmptyActualParam(previousTokenPosition);
    } 
    SourcePos pos = new SourcePos();
    start(pos);
    Expr param = null;
    Expr params = null;
    Expr restargs = null;
    param = parseExpr();
    finish(pos);
    params = new ActualParam(param, pos);
    if (currentToken.kind == Token.COMMA) {
      // Comma case:
      acceptIt();
      restargs = parseArgs();
      if (restargs instanceof EmptyActualParam) {
        syntaxError("Argument after comma expected", "");
      }
    } else {
      // No comma case: 
      restargs = parseArgs();
      if (!(restargs instanceof EmptyActualParam)) {
        syntaxError("Comma between preceeding arguments expected", "");
      }
    }
    finish(pos);
    return new ActualParamSequence(params, restargs, pos);
  }
    
  private Expr parseArgList() throws SyntaxError {
    accept(Token.LEFTPAREN);
    Expr params = parseArgs();
    accept(Token.RIGHTPAREN);
    return params;
  }

  /**
   * parseId() parses a MiniC identifier.
   *
   *<p>ID (terminal)
   */
  private ID parseId() throws SyntaxError {
    ID id = new ID(currentToken.getLexeme(), currentToken.getSourcePos());
    accept(Token.ID);
    return id;
  }

  /**
   * parseTypeSpecifier() parses a MiniC typespecifier.
   *
   *<p>VOID | INT | FLOAT | BOOL (all terminals)
   */
  private Type parseTypeSpecifier() throws SyntaxError {
    Type t = null;
    switch (currentToken.kind) {
      case Token.INT:
        t = new IntType(currentToken.getSourcePos());
        break;
      case Token.FLOAT:
        t = new FloatType(currentToken.getSourcePos());
        break;
      case Token.BOOL:
        t = new BoolType(currentToken.getSourcePos());
        break;
      case Token.VOID:
        t = new VoidType(currentToken.getSourcePos());
        break;
      default:
        syntaxError("Type specifier expected", "");
    }
    acceptIt();
    return t;
  }

}
