/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AsproOIFitsFile.java,v 1.1 2010-09-02 15:46:32 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.model.oifits;

import fr.jmmc.oitools.model.OIFitsFile;
import java.util.ArrayList;
import java.util.List;

/**
 * This class extends OIFitsFile (oitools) in order to add warning messages produced while computation
 * @author bourgesl
 */
public final class AsproOIFitsFile extends OIFitsFile implements WarningContainer {

  /** warning messages */
  private List<String> warningMessages = null;

  /**
   * Public constructor
   */
  public AsproOIFitsFile() {
    super();
  }

  /**
   * Add the given message to the warning messages
   * @param msg message to add
   */
  public void addWarningMessage(final String msg) {
    if (this.warningMessages == null) {
      this.warningMessages = new ArrayList<String>(4);
    }
    this.warningMessages.add(msg);
  }

  /**
   * Return true if there are warning messages
   * @return true if there are warning messages
   */
  public boolean hasWarningMessages() {
    return this.warningMessages != null && this.warningMessages.size() > 0;
  }

  /**
   * Return the list of warning messages
   * @return warning messages or null
   */
  public List<String> getWarningMessages() {
    return this.warningMessages;
  }
}
