/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
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
