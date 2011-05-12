/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservationCollectionTaskSwingWorker.java,v 1.1 2011-03-01 17:07:44 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.6  2011/02/25 16:52:11  bourgesl
 * removed unused code
 *
 * Revision 1.5  2011/02/24 17:14:13  bourgesl
 * Major refactoring to support / handle observation collection (multi-conf)
 *
 * Revision 1.4  2011/02/14 17:13:07  bourgesl
 * Use JMCS Task / TaskSwingWorker ...
 *
 * Revision 1.3  2011/02/04 17:18:59  bourgesl
 * minor
 *
 * Revision 1.2  2011/02/03 17:29:24  bourgesl
 * removed getVersion
 *
 * Revision 1.1  2011/02/02 17:41:52  bourgesl
 * add observation and its version in task logs
 *
 ******************************************************************************/
package fr.jmmc.aspro.gui.task;

import fr.jmmc.aspro.model.oi.ObservationCollection;
import fr.jmmc.mcs.gui.task.Task;
import fr.jmmc.mcs.gui.task.TaskSwingWorker;

/**
 * This class extends TaskSwingWorker to :
 * - define the observation collection associated to this worker
 * - simplify debugging / logging by adding observation version to task logs
 *
 * @author bourgesl
 *
 * @param <T> the result type returned by this {@code TaskSwingWorker}
 */
public abstract class ObservationCollectionTaskSwingWorker<T> extends TaskSwingWorker<T> {

  /* members */
  /** observation collection */
  private final ObservationCollection obsCollection;

  /**
   * Create a new ObservationCollectionTaskSwingWorker instance
   * @param task related task
   * @param obsCollection observation collection used by computations associated with this worker
   */
  public ObservationCollectionTaskSwingWorker(final Task task, final ObservationCollection obsCollection) {
    super(task, (DEBUG_FLAG) ? obsCollection.getVersion().toString() : "");
    this.obsCollection = obsCollection;
  }

  /**
   * Return the observation collection
   * @return observation collection
   */
  protected final ObservationCollection getObservationCollection() {
    return this.obsCollection;
  }
}
