/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservationTaskSwingWorker.java,v 1.3 2011-02-04 17:18:59 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2011/02/03 17:29:24  bourgesl
 * removed getVersion
 *
 * Revision 1.1  2011/02/02 17:41:52  bourgesl
 * add observation and its version in task logs
 *
 */
package fr.jmmc.aspro.gui.task;

import fr.jmmc.aspro.model.ObservationVersion;
import fr.jmmc.aspro.model.oi.ObservationSetting;

/**
 * This class extends TaskSwingWorker to :
 * - define observation associated to this worker
 * - define the observation version
 * - simplify debugging / logging by adding observation version to task logs
 *
 * @author bourgesl
 *
 * @param <T> the result type returned by this {@code TaskSwingWorker}
 */
public abstract class ObservationTaskSwingWorker<T> extends TaskSwingWorker<T> {

  /* members */
  /** observation associated to this worker */
  private final ObservationSetting observation;

  /**
   * Create a new ObservationTaskSwingWorker instance
   * @param task related task
   * @param observation observation used for computations (different from main observation) associated with this worker
   */
  public ObservationTaskSwingWorker(final Task task, final ObservationSetting observation) {
    super(task, (DEBUG_FLAG) ? observation.getVersion().toString() : "");
    this.observation = observation;
  }

  /**
   * Return the associated observation
   * @return associated observation
   */
  protected final ObservationSetting getObservation() {
    return this.observation;
  }
}
