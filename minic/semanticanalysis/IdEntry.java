package minic.semanticanalysis;

import minic.astgen.Decl;

/** An instance of class IdEntry represents a scopestack entry. */
public class IdEntry {

  // Identifier lexeme of this entry:
  protected String id;
  // AST that has the declaration of this entry:
  protected Decl declAst;
  // Scope-stack level:
  protected int level;
  // Link to previous scope stack entry to support look-up:
  protected IdEntry previous;

  /** Constructor of scopestack entry class. */
  public IdEntry(String id, Decl declAst, int level, IdEntry previous) {
    this.id = id;
    this.declAst = declAst;
    this.level = level;
    this.previous = previous;
  }

}
