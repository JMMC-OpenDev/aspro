/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TargetSelectionEvent.java,v 1.2 2011-02-24 17:14:12 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2011/02/22 18:11:30  bourgesl
 * Major UI changes : configuration multi-selection, unique target selection in main form
 *
 * Revision 1.2  2011/02/02 17:40:36  bourgesl
 * added observation for debugging purposes
 *
 * Revision 1.1  2011/01/31 15:26:31  bourgesl
 * new event to contain warnings
 *
 ******************************************************************************/
package fr.jmmc.aspro.model.event;

import fr.jmmc.aspro.model.oi.Target;

/**
 * This event extends the ObservationEvent to contain the selected target
 */
public final class TargetSelectionEvent extends ObservationEvent {

  /** selected target */
  private final Target target;

  /**
   * Public constructor
   * @param target selected target
   */
  public TargetSelectionEvent(final Target target) {
    super(ObservationEventType.TARGET_SELECTION_CHANGED);
    this.target = target;
  }

  /**
   * Return the selected target
   * @return selected target
   */
  public Target getTarget() {
    return this.target;
  }
}
