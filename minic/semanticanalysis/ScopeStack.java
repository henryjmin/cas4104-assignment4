package minic.semanticanalysis;

import minic.astgen.*;

/** Class ScopeStack, for MiniC scopestacks. */
final class ScopeStack {

  private int level;
  private IdEntry latest;

  public ScopeStack() {
    level = 1;  // MiniC's global scope is on level 1.
    latest = null;
  }

  /* Method openScope().
   *
   *<p>Opens a new level in the scope stack, 1 higher than the
   * current topmost level.
   */
  public void openScope() {
    level++;
  }

  /** Method closeScope().
   *
   *<p>Closes the topmost level in the scope stack, discarding
   * all entries belonging to that level.
   */
  public void closeScope() {

    IdEntry entry;

    // Presumably, idTable.level > 0:
    assert (this.level > 0);
    entry = this.latest;
    while (entry.level == this.level) {
      /*
         local = entry;
         entry = local.previous;
       */
      assert (entry.previous != null);
      entry = entry.previous;
    }
    this.level--;
    this.latest = entry;
  }

  /**
   * Method enter().
   *
   *<p>Makes a new entry in the scope stack for the given identifier
   * and attribute. The new entry belongs to the current level.
   * Returns false iff there is already an entry for the
   * same identifier at the current level.
   */
  public boolean enter(String id, Decl declAst) {

    IdEntry entry = this.latest;
    boolean searching = true;

    // Check for duplicate entry ...
    while (searching) {
      if (entry == null || entry.level < this.level) {
        searching = false;
      } else if (entry.id.equals(id)) {
        // duplicate entry dedected:
        return false;
      } else {
        entry = entry.previous;
      }
    }

    // "id" does not exist on this scope level, add new entry for "id":...
    entry = new IdEntry(id, declAst, this.level, this.latest);
    this.latest = entry;
    return true;
  }

  /**
   * Method retrieve().
   *
   *<p>Finds an entry for the given identifier in the scope stack,
   * if any. If there are several entries for that identifier, finds the
   * entry that is highest in the stack, in accordance with the scope rules.
   * Returns null if no entry is found.
   * Otherwise returns the declAST field of the scope stack entry found.
   *
   * @param id the id string to retrieve
   */
  public Decl retrieve(String id) {

    IdEntry entry;
    Decl declAst = null;
    boolean searching = true;

    entry = this.latest;
    while (searching) {
      if (entry == null) {
        searching = false;
      } else if (entry.id.equals(id)) {
        searching = false;
        declAst = entry.declAst;
      } else {
        entry = entry.previous;
      }
    }
    return declAst;
  }

}
