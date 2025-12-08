package minic.scanner;

/** An instance of class Token represents one token from the input. */
public final class Token extends Object {

  public int kind;
  protected SourcePos srcPos;
  private String lexeme;
  private static int overall_nrtokens = 0;
  private int myTokenNr;

  /**
   * Constructor of class Token.
   *
   * @param kind the token kind
   * @param lexeme the lexeme (string) of the token
   * @param srcPos the position information of the token
   */
  public Token(int kind, String lexeme, SourcePos srcPos) {
    if (kind == Token.ID) {
      int index = firstKeyword;
      boolean searching = true;
      while (searching) {
        int compare = tokenTable[index].compareTo(lexeme);
        if (compare == 0) {
          this.kind = index;
          searching = false;
        } else if (compare > 0 || index == lastKeyword) {
          this.kind = Token.ID;
          searching = false;
        } else {
          index++;
        }
      }
    } else {
      this.kind = kind;
    }
    this.srcPos = srcPos;
    this.lexeme = lexeme;
    overall_nrtokens++;
    myTokenNr = overall_nrtokens;
  }

  /**
   * Method print() prints the entire information of a token on the console. This method is provided
   * to debug the scanner.
   */
  public void print() {
    System.out.println("token" + myTokenNr + ".kind = Token." + tokenTable[kind].toUpperCase());
    System.out.println("token" + myTokenNr + ".lexeme = \"" + lexeme + "\"");
    System.out.println("token" + myTokenNr + ".srcPos.startLine = " + srcPos.startLine);
    System.out.println("token" + myTokenNr + ".srcPos.endLine = " + srcPos.endLine);
    System.out.println("token" + myTokenNr + ".srcPos.startCol = " + srcPos.startCol);
    System.out.println("token" + myTokenNr + ".srcPos.endCol = " + srcPos.endCol + "\n");
  }

  // identifiers, operators, literals:
  public static final int ID = 0; // identifier
  public static final int ASSIGN = 1; // a = ...
  public static final int OR = 2; // ||
  public static final int AND = 3; // &&
  public static final int NOT = 4; // !
  public static final int EQ = 5; // ==
  public static final int NOTEQ = 6; // !=
  public static final int LESSEQ = 7; // <=
  public static final int LESS = 8; // <
  public static final int GREATER = 9; // >
  public static final int GREATEREQ = 10; // >=
  public static final int PLUS = 11; // +
  public static final int MINUS = 12; // -
  public static final int TIMES = 13; // *
  public static final int DIV = 14; // /
  public static final int INTLITERAL = 15;
  public static final int FLOATLITERAL = 16;
  public static final int BOOLLITERAL = 17;
  public static final int STRINGLITERAL = 18;

  // keywords:
  public static final int BOOL = 19; // bool
  public static final int ELSE = 20; // else
  public static final int FLOAT = 21; // float
  public static final int FOR = 22; // for
  public static final int IF = 23; // if
  public static final int INT = 24; // int
  public static final int RETURN = 25; // return
  public static final int VOID = 26; // void
  public static final int WHILE = 27; // while

  // punctuation:
  public static final int LEFTBRACE = 28; // {
  public static final int RIGHTBRACE = 29; // }
  public static final int LEFTBRACKET = 30; // [
  public static final int RIGHTBRACKET = 31; // ]
  public static final int LEFTPAREN = 32; // (
  public static final int RIGHTPAREN = 33; // )
  public static final int COMMA = 34; // ,
  public static final int SEMICOLON = 35; // ;

  // special tokens:
  public static final int ERROR = 36;
  public static final int EOF = 37; // end-of-file

  private static String[] tokenTable =
      new String[] {
        "ID",
        "ASSIGN",
        "OR",
        "AND",
        "NOT",
        "EQ",
        "NOTEQ",
        "LESSEQ",
        "LESS",
        "GREATER",
        "GREATEREQ",
        "PLUS",
        "MINUS",
        "TIMES",
        "DIV",
        "INTLITERAL",
        "FLOATLITERAL",
        "BOOLLITERAL",
        "STRINGLITERAL",
        "bool",
        "else",
        "float",
        "for",
        "if",
        "int",
        "return",
        "void",
        "while",
        "LEFTBRACE",
        "RIGHTBRACE",
        "LEFTBRACKET",
        "RIGHTBRACKET",
        "LEFTPAREN",
        "RIGHTPAREN",
        "COMMA",
        "SEMICOLON",
        "ERROR",
        "EOF"
      };

  private static String[] lexemeTable =
      new String[] {
        "ID",
        "=",
        "||",
        "&&",
        "!",
        "==",
        "!=",
        "<=",
        "<",
        ">",
        ">=",
        "+",
        "-",
        "*",
        "/",
        "INTLITERAL",
        "FLOATLITERAL",
        "BOOLLITERAL",
        "STRINGLITERAL",
        "bool",
        "else",
        "float",
        "for",
        "if",
        "int",
        "return",
        "void",
        "while",
        "{",
        "}",
        "[",
        "]",
        "(",
        ")",
        ",",
        ";",
        "ERROR",
        "EOF"
      };

  private static final int firstKeyword = Token.BOOL;
  private static final int lastKeyword = Token.WHILE;

  /**
   * Given a token constant, method spell() returns the lexeme corresponding to the token constant.
   */
  public static String spell(int kind) {
    return lexemeTable[kind];
  }

  /** Method getSourcePos() returns the SourcePos position information object of a token. */
  public SourcePos getSourcePos() {
    return srcPos;
  }

  /** Method getLexeme() returns the lexeme (string) of a token. */
  public String getLexeme() {
    return lexeme;
  }
}
