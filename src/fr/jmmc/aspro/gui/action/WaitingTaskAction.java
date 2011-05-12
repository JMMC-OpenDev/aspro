/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: WaitingTaskAction.java,v 1.4 2011-03-08 17:25:36 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.3  2011/02/14 17:13:07  bourgesl
 * Use JMCS Task / TaskSwingWorker ...
 *
 * Revision 1.2  2011/02/04 10:04:35  bourgesl
 * change the cursor (wait) when an action is pending
 *
 * Revision 1.1  2011/02/03 17:30:55  bourgesl
 * new abstract action that detect if any task is running and waits for task completion before running action (use Timer)
 *
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.mcs.gui.App;
import fr.jmmc.mcs.gui.task.TaskSwingWorkerExecutor;
import fr.jmmc.mcs.util.RegisteredAction;
import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.logging.Level;
import javax.swing.Timer;

/**
 * This specific action executes only when there is no running task and waits for task to complete before executing.
 *
 * Note : when an action is waiting for completion, other action calls are discarded.
 *
 * @author bourgesl
 */
public abstract class WaitingTaskAction extends RegisteredAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(WaitingTaskAction.class.getName());
  /** delay in milliseconds between each poll to check if there is still a task running */
  private final static int POLLING_DELAY = 100;
  /** shared flag to know if there is already a  pending action */
  private static boolean pending = false;

  /**
   * Define the shared pending flag and the main frame cursor (default|wait)
   * @param value new value to set
   */
  private static void setPending(final boolean value) {
    pending = value;

    App.getFrame().setCursor(
            (value)
            ? Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR)
            : Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
  }

  /**
   * Constructor, that automatically register the action in RegisteredAction.
   * Action name, icon, accelerator and description is first inited using
   * fieldName to build a MCSAction.
   * @param classPath the path of the class containing the field pointing to
   * the action, in the form returned by 'getClass().getName();'.
   * @param fieldName the name of the field pointing to the action.
   */
  public WaitingTaskAction(final String classPath, final String fieldName) {
    super(classPath, fieldName);
  }

  /**
   * Handle the action event
   * @param ae action event
   */
  public final void actionPerformed(final ActionEvent ae) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("actionPerformed");
    }

    // If there is already a pending action ?
    if (pending) {
      // discard this action event :

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("discard action (another action is pending) : " + this.getClass().getName());
      }
      return;
    }

    // check if there is any running task :
    if (TaskSwingWorkerExecutor.isTaskRunning()) {
      // indicate to other actions that this action is pending for execution :
      setPending(true);

      // delay the delegate action until there is no running task :
      new DelayedActionPerformer(this).start();

    } else {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("execute action : " + this);
      }

      actionPerformed();
    }
  }

  /**
   * This inner class uses a timer internally to check if there is still running tasks.
   */
  private final static class DelayedActionPerformer implements ActionListener {

    /** defered action */
    private final WaitingTaskAction adapter;
    /** Swing timer */
    private Timer timer;

    /**
     * Constructor that creates a timer to delay the action execution
     * @param adapter WaitingTaskAction
     */
    protected DelayedActionPerformer(final WaitingTaskAction adapter) {
      this.adapter = adapter;

      this.timer = new Timer(POLLING_DELAY, this);

      this.timer.setRepeats(true);
      this.timer.setCoalesce(false);
    }

    /**
     * Starts the <code>Timer</code>
     */
    protected void start() {
      this.timer.start();
    }

    /**
     * Handle the timer calls until there is no running task
     * @param ae action event
     */
    public void actionPerformed(final ActionEvent ae) {
      final boolean taskRunning = TaskSwingWorkerExecutor.isTaskRunning();

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("running task : " + taskRunning);
      }

      if (!taskRunning) {
        // indicate to other actions that this action is no more pending :
        setPending(false);

        // stop this timer :
        this.timer.stop();

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("execute action : " + this.adapter);
        }

        // execute the action :
        this.adapter.actionPerformed();
      }
    }
  }

  /**
   * Handle the action event
   */
  public abstract void actionPerformed();
}
