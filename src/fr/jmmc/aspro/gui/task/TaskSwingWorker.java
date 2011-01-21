/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TaskSwingWorker.java,v 1.1 2011-01-21 16:31:41 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.task;

import fr.jmmc.mcs.gui.FeedbackReport;
import java.util.concurrent.ExecutionException;
import java.util.logging.Level;

/**
 * This class extends SwingWorker backport for Java 5 to :
 * - define related task to cancel easily the task and its child tasks
 * - simplify debugging / logging.
 * Requires library :
 * swing-worker-1.2.jar (org.jdesktop.swingworker.SwingWorker)
 *
 * @author bourgesl
 *
 * @param <T> the result type returned by this {@code TaskSwingWorker's}
 */
public abstract class TaskSwingWorker<T> extends org.jdesktop.swingworker.SwingWorker<T, Void> {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.util.SwingWorker";
  /** Class logger */
  protected static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** log Level to use (Level.INFO to help debugging) */
  protected final static Level logLevel = Level.FINE;

  /* members */
  /** related task */
  private final Task task;
  /** version */
  private final int version;
  /** log prefix using the format 'SwingWorker[" + task.name + "]' used by debugging statements */
  protected final String logPrefix;

  /**
   * Create a new TaskSwingWorker instance
   * @param task related task
   * @param version version to use
   */
  public TaskSwingWorker(final Task task, final int version) {
    this.task = task;
    this.version = version;
    this.logPrefix = "SwingWorker[" + task.getName() + "]";
  }

  /**
   * Return the task related to this SwingWorker
   * @return related task
   */
  public final Task getTask() {
    return task;
  }

  /**
   * Return the version
   * @return version
   */
  public final int getVersion() {
    return this.version;
  }

  @Override
  public final String toString() {
    return this.logPrefix + "{version=" + this.version + "}@" + Integer.toHexString(hashCode());
  }

  /**
   * Compute the data in background
   * @return data computed data
   */
  @Override
  public final T doInBackground() {
    if (logger.isLoggable(logLevel)) {
      logger.log(logLevel, logPrefix + ".doInBackground : START");
    }

    T data = null;

    // compute the data :
    data = this.computeInBackground();

    if (isCancelled()) {
      if (logger.isLoggable(logLevel)) {
        logger.log(logLevel, logPrefix + ".doInBackground : CANCELLED");
      }
      // no result if task was cancelled :
      data = null;
    } else {
      if (logger.isLoggable(logLevel)) {
        logger.log(logLevel, logPrefix + ".doInBackground : DONE");
      }
    }
    return data;
  }

  /**
   * Refresh the plot using the computed observability data.
   * This code is executed by the Swing Event Dispatcher thread (EDT)
   */
  @Override
  public final void done() {
    // check if the worker was cancelled :
    if (isCancelled()) {
      if (logger.isLoggable(logLevel)) {
        logger.log(logLevel, logPrefix + ".done : CANCELLED");
      }
    } else {
      try {
        // Get the computed results :
        final T data = get();

        if (data == null) {
          if (logger.isLoggable(logLevel)) {
            logger.log(logLevel, logPrefix + ".done : NO DATA");
          }
        } else {
          if (logger.isLoggable(logLevel)) {
            logger.log(logLevel, logPrefix + ".done : UI START");
          }

          // refresh UI with data :
          this.refreshUI(data);

          if (logger.isLoggable(logLevel)) {
            logger.log(logLevel, logPrefix + ".done : UI DONE");
          }
        }

      } catch (InterruptedException ignore) {
      } catch (ExecutionException ee) {
        handleException(ee);
      }
    }
  }

  /**
   * Compute operation invoked by a Worker Thread (not Swing EDT) in background
   * Called by @see #doInBackground()
   * @return computed data
   */
  public abstract T computeInBackground();

  /**
   * Refresh GUI invoked by the Swing Event Dispatcher Thread (Swing EDT)
   * Called by @see #done()
   * @param data computed data
   */
  public abstract void refreshUI(final T data);

  /**
   * Handle the execution exception that occured in the compute operation {@see #computeInBackground()}.
   * This default implementation opens the feedback report (modal and do not exit on close).
   *
   * @param ee execution exception
   */
  public void handleException(final ExecutionException ee) {
    // Show feedback report (modal and do not exit on close) :
    new FeedbackReport(true, ee.getCause());
  }
}
