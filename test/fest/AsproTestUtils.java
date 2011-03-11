/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AsproTestUtils.java,v 1.1 2011-03-11 16:02:05 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fest;

import static org.fest.swing.timing.Pause.*;
import org.fest.swing.timing.Timeout;

import fr.jmmc.mcs.gui.task.TaskSwingWorkerExecutor;
import java.util.logging.Level;
import org.fest.swing.timing.Condition;

/**
 * This class contains utility methods
 * @author bourgesl
 */
public final class AsproTestUtils {

  /** Class logger */
  protected static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          AsproTestUtils.class.getName());
  /** 10s timeout */
  protected static final Timeout LONG_TIMEOUT = Timeout.timeout(10000l);

  /**
   * Forbidden constructor
   */
  private AsproTestUtils() {
    super();
  }

  /**
   * Waits until the Aspro computation tasks are done (+ timeout)
   */
  public static void checkRunningTasks() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("checkRunningTasks : enter");
    }

    pause(new Condition("TaskRunning") {

      /**
       * Checks if the condition has been satisfied.
       * @return <code>true</code> if the condition has been satisfied, otherwise <code>false</code>.
       */
      public boolean test() {
        final boolean done = !TaskSwingWorkerExecutor.isTaskRunning();

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("checkRunningTasks : test = " + done);
        }
        return done;
      }
    }, LONG_TIMEOUT);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("checkRunningTasks : exit");
    }
  }
}
