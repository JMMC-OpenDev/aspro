/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fest;

import static org.fest.swing.timing.Pause.*;
import org.fest.swing.timing.Timeout;

import fr.jmmc.jmcs.gui.task.TaskSwingWorkerExecutor;
import java.util.logging.Level;
import org.fest.swing.edt.GuiActionRunner;
import org.fest.swing.edt.GuiQuery;
import org.fest.swing.timing.Condition;

/**
 * This class contains utility methods
 * @author bourgesl
 */
public final class AsproTestUtils {

  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(AsproTestUtils.class.getName());
  /** 5s timeout */
  private static final Timeout LONG_TIMEOUT = Timeout.timeout(5000l);

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
      @Override
      public boolean test() {

        return GuiActionRunner.execute(new GuiQuery<Boolean>() {

          @Override
          protected Boolean executeInEDT() {
            final boolean done = !TaskSwingWorkerExecutor.isTaskRunning();

            if (logger.isLoggable(Level.FINE)) {
              logger.fine("checkRunningTasks : test = " + done);
            }
            return done;
          }
        });

      }
    }, LONG_TIMEOUT);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("checkRunningTasks : exit");
    }
  }
}
