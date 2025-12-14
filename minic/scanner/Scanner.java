package minic.scanner;

/** Implements the scanner with functionality to provide the next token in the input. */
public final class Scanner {

  private SourceFile sourceFile;

  private char currentChar;
  private boolean verbose;
  private StringBuffer currentLexeme;
  private boolean currentlyScanningToken;
  private int currentLineNr;
  private int currentColNr;
  private StringBuffer lookahead;

  private boolean isDigit(char c) {
    return (c >= '0' && c <= '9');
  }

  ///////////////////////////////////////////////////////////////////////////////

  /**
   * Constructs the scanner object.
   *
   * @param source the source code buffer.
   */
  public Scanner(SourceFile source) {
    sourceFile = source;
    currentChar = sourceFile.readChar();
    verbose = false;
    currentLineNr = 1;
    currentColNr = 1;
    lookahead = new StringBuffer("");
  }

  /** Ask scanner to emit debug output for every token. */
  public void enableDebugging() {
    verbose = true;
  }

  // takeIt appends the current character to the current token, and gets
  // the next character from the source program (or the to-be-implemented
  // "untake" buffer in case of look-ahead characters that got 'pushed back'
  // into the input stream).

  private void takeIt() {
    if (currentChar == '\n') {
      currentLineNr++;
      currentColNr= 0;
    }
    if (currentlyScanningToken) {
      currentLexeme.append(currentChar);
    }
    if (lookahead.length() > 0) {
      lookahead.deleteCharAt(0);
    }
    if (lookahead.length() > 0) {
      currentChar = lookahead.charAt(0);
    } else {
      currentChar = sourceFile.readChar();
    }
    currentColNr++;
  }

  private boolean isLetter(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
  }

  private boolean takeExponent() {
    if (currentChar != 'e' && currentChar != 'E') {
      return false;
    }
    lookahead.append(currentChar);
    currentChar = sourceFile.readChar();

    if (currentChar == '+' || currentChar == '-') {
      lookahead.append(currentChar);
      currentChar = sourceFile.readChar();
    }

    char c = currentChar;
    lookahead.append(currentChar);
    currentChar = lookahead.charAt(0);
    if (!isDigit(c)) {
      return false;
    }

    while (lookahead.length() > 0) {
      takeIt();
    }
    while (isDigit(currentChar)) {
      takeIt();
    }

    return true;
  }

  private int scanToken() {
    boolean digit = false;
    switch (currentChar) {
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        digit = true;
        takeIt();
        while (isDigit(currentChar)) {
          takeIt();
        }
        if (currentChar != '.') {
          if (takeExponent()) {
            return Token.FLOATLITERAL;
          }
          return Token.INTLITERAL;
        }
      /* fall through */
      case '.':
        takeIt();
        if (!digit && !isDigit(currentChar)) {
          return Token.ERROR;
        }
        while (isDigit(currentChar)) {
          takeIt();
        }
        takeExponent();
        return Token.FLOATLITERAL;

      // operators
      case '+':
        takeIt();
        return Token.PLUS;
      case '-':
        takeIt();
        return Token.MINUS;
      case '*':
        takeIt();
        return Token.TIMES;
      case '<':
        takeIt();
        if (currentChar == '=') {
          takeIt();
          return Token.LESSEQ;
        }
        return Token.LESS;
      case '>':
        takeIt();
        if (currentChar == '=') {
          takeIt();
          return Token.GREATEREQ;
        }
        return Token.GREATER;
      case '=':
        takeIt();
        if (currentChar == '=') {
          takeIt();
          return Token.EQ;
        }
        return Token.ASSIGN;
      case '!':
        takeIt();
        if (currentChar == '=') {
          takeIt();
          return Token.NOTEQ;
        }
        return Token.NOT;
      case '&':
        takeIt();
        if (currentChar == '&') {
          takeIt();
          return Token.AND;
        }
        return Token.ERROR;
      case '|':
        takeIt();
        if (currentChar == '|') {
          takeIt();
          return Token.OR;
        }
        return Token.ERROR;
      case '/':
        takeIt();
        // C++ style comments
        if (currentChar == '/') {
          while (currentChar != '\n') {
            takeIt();
          }
          takeIt();
          return -1;
        // C style comments
        } else if (currentChar == '*') {
          takeIt();
          while (true) {
            while (currentChar != '*' && currentChar != SourceFile.EOF) {
              takeIt();
            }
            if (currentChar == SourceFile.EOF) {
              System.out.println("ERROR: unterminated multi-line comment.");
              return -1;
            }
            takeIt();
            if (currentChar != '/') {
              continue;
            }
            takeIt();
            return -1;
          }
        }
        return Token.DIV;

      // separators
      case '{':
        takeIt();
        return Token.LEFTBRACE;
      case '}':
        takeIt();
        return Token.RIGHTBRACE;
      case '(':
        takeIt();
        return Token.LEFTPAREN;
      case ')':
        takeIt();
        return Token.RIGHTPAREN;
      case '[':
        takeIt();
        return Token.LEFTBRACKET;
      case ']':
        takeIt();
        return Token.RIGHTBRACKET;
      case ',':
        takeIt();
        return Token.COMMA;
      case ';':
        takeIt();
        return Token.SEMICOLON;

      // string literals
      case '\"':
        currentlyScanningToken = false;
        takeIt();
        currentlyScanningToken = true;
        boolean escape = false;
        while (currentChar != '\n') {
          if (escape) {
            if (currentChar != 'n') {
              System.out.println("ERROR: illegal escape sequence");
            }
            escape = false;
          } else if (currentChar == '\\') {
            escape = true;
          } else if (currentChar == '\"') {
            currentlyScanningToken = false;
            takeIt();
            currentlyScanningToken = true;
            return Token.STRINGLITERAL;
          }
          takeIt();
        }
        System.out.println("ERROR: unterminated string literal");
        return Token.STRINGLITERAL;

      case SourceFile.EOF:
        currentLexeme.append('$');
        currentColNr++;
        return Token.EOF;

      default:
        if (isLetter(currentChar)) {
          takeIt();
          while (isDigit(currentChar) || isLetter(currentChar)) {
            takeIt();
          }
          String lexeme = currentLexeme.toString();
          if (lexeme.equals("true") || lexeme.equals("false")) {
            return Token.BOOLLITERAL;
          }
          return Token.ID;
        }
        takeIt();
        return Token.ERROR;
    }
  }

  /** Scans the next token. */
  public Token scan() {

    currentlyScanningToken = false;
    while (currentChar == ' '
        || currentChar == '\f'
        || currentChar == '\n'
        || currentChar == '\r'
        || currentChar == '\t') {
      takeIt();
    }

    currentlyScanningToken = true;
    currentLexeme = new StringBuffer("");
    SourcePos pos = new SourcePos();
    pos.startLine = currentLineNr;
    pos.endLine = currentLineNr;
    pos.startCol = currentColNr;
    int kind = scanToken();
    if (kind < 0) {
      return scan();
    }
    Token currentToken = new Token(kind, currentLexeme.toString(), pos);
    pos.endCol = currentColNr - 1;
    if (verbose) {
      currentToken.print();
    }
    return currentToken;
  }
}
