package minic;

import minic.StdEnvironment;
import minic.astgen.Program;
import minic.parser.Parser;
import minic.scanner.Scanner;
import minic.scanner.SourceFile;
import minic.semanticanalysis.SemanticAnalysis;
import minic.treedrawer.Drawer;
import minic.treeprinter.Printer;
import minic.unparser.Unparser;

/**
 * Class with the main method of the MiniC compiler.
 *
 * <p>Represents the compiler driver that initializes the compiler's infra- structure such as the
 * character stream from the source file and then conducts the compilation phases (lexing, parsing,
 * semantic analysis and code generation.
 */
public class MiniC {

  private static Scanner scanner;
  private static Parser parser;
  private static SemanticAnalysis sem;
  private static ErrorReporter reporter;
  private static Drawer drawer;
  private static Printer printer;
  private static Unparser unparser;
  private static StdEnvironment stdenv;
  /* The abstract syntax tree representing
   * the source program:
   */
  private static Program AST;
  // commandline args:
  private static String sourceName;
  private static boolean DrawTree1;
  private static boolean DrawTree2;
  private static boolean DrawStdEnvTree;
  private static boolean PrintTree;
  private static boolean UnparseTree;
  private static String PrintTreeF;
  private static String UnparseTreeF;


  static void compileProgram(String sourceName) {
    System.out.println("********** MiniC Compiler **********");

    SourceFile source = new SourceFile(sourceName);
    scanner = new Scanner(source);
    /*
     * Uncomment to observe the sequence of tokens
     * delivered by the scanner:
     *
     */
    // scanner.enableDebugging();
    reporter = new ErrorReporter();
    stdenv   = new StdEnvironment();
    parser   = new Parser(scanner, reporter);
    sem      = new SemanticAnalysis(reporter);
    drawer   = new Drawer();
    printer  = new Printer();
    unparser = new Unparser();

    if (DrawStdEnvTree) {
      Drawer envdrawer = new Drawer();
      envdrawer.draw(stdenv.ast);
    }

    System.out.println("Syntax Analysis ...");
    AST = parser.parse();  // 1st pass

    if (reporter.numErrors == 0) {
      if (PrintTree) {
        printer.print(AST, PrintTreeF);
      }
      if (UnparseTree) {
        unparser.unparse(AST, UnparseTreeF);
      }
      if (DrawTree1) {
        drawer.draw(AST);
      }
      System.out.println("Semantic Analysis ...");
      sem.check(AST);  // 2nd pass
      if (DrawTree2) {
        drawer.draw(AST);
      }
    }

    boolean successful = (reporter.numErrors == 0);
    if (successful) {
      System.out.println("Compilation was successful.");
    } else {
      System.out.println("Compilation was unsuccessful.");
    }
  }

  private static void usage() {
    System.out.println("Usage: MiniC [options] filename");
    System.out.println("Option: -ast1 to draw the AST before semantic analysis");
    System.out.println("Option: -ast2 to draw the AST after semantic analysis");
    System.out.println("Option: -envast to draw the StdEnvironment AST"); 
    System.out.println("Option: -t <file> to dump the AST to <file>");
    System.out.println("Option: -u <file> to unparse the AST to <file>");
    System.exit(1);
  }

  private static void processCmdLine(String[] args) {
    DrawTree1 = false;
    DrawTree2 = false;
    DrawStdEnvTree = false;
    PrintTree = false;
    PrintTreeF = "";
    UnparseTree = false;
    UnparseTreeF = "";
    sourceName = "";
    int argIndex = 0;
    while (argIndex < args.length) {
      if (args[argIndex].equals("-ast1")) {
        DrawTree1 = true;
        argIndex++;
      } else if (args[argIndex].equals("-ast2")) {
        DrawTree2 = true;
        argIndex++;
      } else if (args[argIndex].equals("-envast")) {
        DrawStdEnvTree = true;
        argIndex++;
      } else if (args[argIndex].equals("-t")) {
        PrintTree = true;
        if (args.length < argIndex + 1) {
          usage();
        } else {
          argIndex++;
          PrintTreeF = args[argIndex];
          argIndex++;
        }
      } else if (args[argIndex].equals("-u")) {
        UnparseTree = true;
        if (args.length < argIndex + 1) {
          usage();
        } else {
          argIndex++;
          UnparseTreeF = args[argIndex];
          argIndex++;
        }
      } else {
        sourceName = args[argIndex];
        argIndex++;
        if (argIndex < args.length) {
          // After the input source file no arg is allowed:
          usage();
        }
      }
    }
    if (sourceName.equals("")) {
      usage();
    }
  }

  /** Main method, which is the entry point when the MiniC compiler is run. */
  public static void main(String[] args) {
    processCmdLine(args);
    compileProgram(sourceName);
  }

}
