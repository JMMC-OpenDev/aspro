/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import java.util.ArrayList;
import java.util.List;

/**
 * This class is a container for warning messages produced while computation
 * @author bourgesl
 */
public final class WarningContainer {

  /** warning messages */
  private List<String> warningMessages = null;

  /**
   * Public constructor
   */
  public WarningContainer() {
    super();
  }

  /**
   * Add the given warning container to this warning container only if the new message does not already exist in this container
   * @param container messages to add
   */
  public void addWarningMessages(final WarningContainer container) {
    if (container.hasWarningMessages()) {
      if (this.warningMessages == null) {
        this.warningMessages = new ArrayList<String>(4);
      }
      for (String msg : container.getWarningMessages()) {
        if (!this.warningMessages.contains(msg)) {
          this.warningMessages.add(msg);
        }
      }
    }
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
    return this.warningMessages != null && !this.warningMessages.isEmpty();
  }

  /**
   * Return the list of warning messages
   * @return warning messages or null
   */
  public List<String> getWarningMessages() {
    return this.warningMessages;
  }
}
