/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TaskSwingWorkerExecutor.java,v 1.4 2011-02-02 17:43:09 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.3  2011/01/25 12:29:37  bourgesl
 * fixed javadoc errors
 *
 * Revision 1.2  2011/01/25 10:40:42  bourgesl
 * fixed class name
 *
 * Revision 1.1  2011/01/21 16:33:19  bourgesl
 * refactored SwingWorkerExecutor to use TaskSwingWorker and simplify and clean up the cancellation of child tasks before executing a new worker
 *
 * Revision 1.5  2010/09/25 13:42:22  bourgesl
 * use installThreadHandler(thread) for worker threads
 *
 * Revision 1.4  2010/09/20 12:16:26  bourgesl
 * created inner class + logs
 *
 * Revision 1.3  2010/07/05 14:50:34  bourgesl
 * cancel method made public
 *
 * Revision 1.2  2010/06/17 10:02:51  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.1  2010/02/03 16:06:47  bourgesl
 * special swing worker executor to have a single thread (serialized computations) and manages cancellation of previous computation task of the same kind
 *
 */
package fr.jmmc.aspro.gui.task;

import fr.jmmc.mcs.util.MCSExceptionHandler;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.FutureTask;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReferenceArray;
import java.util.logging.Level;

/**
 * This class is a customization of the standard SwingWorker to have a single thread only
 * processing workers because our computations require serialization and cancellation
 * @author bourgesl
 */
public final class TaskSwingWorkerExecutor {

  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          TaskSwingWorkerExecutor.class.getName());
  /** flag to log debugging information */
  protected final static boolean DEBUG_FLAG = false;
  /** singleton instance */
  private static TaskSwingWorkerExecutor instance;

  /**
   * Create the TaskSwingWorkerExecutor singleton
   * @param registry task registry only used to get the number of tasks
   */
  public static void create(final TaskRegistry registry) {
    if (instance == null) {
      instance = new TaskSwingWorkerExecutor(registry);

      if (DEBUG_FLAG) {
        logger.info("created SwingWorkerExecutor : " + instance);
      }
    }
  }

  /**
   * Shutdown the TaskSwingWorkerExecutor
   */
  public static void stop() {
    if (instance != null) {
      instance.shutdown();
      if (DEBUG_FLAG) {
        logger.info("stopped SwingWorkerExecutor : " + instance);
      }
    }
    instance = null;
  }

  /**
   * This code returns the singleton instance.
   * @return TaskSwingWorkerExecutor
   */
  private static TaskSwingWorkerExecutor getInstance() {
    if (instance == null) {
      throw new IllegalStateException("SwingWorkerExecutor singleton not available !");
    }
    return instance;
  }

  /**
   * Schedules the given {@code TaskSwingWorker} for execution on a <i>worker</i>
   * thread.
   * @see #execute(TaskSwingWorker)
   * @param worker TaskSwingWorker instance to execute
   */
  public final static void executeTask(final TaskSwingWorker<?> worker) {
    getInstance().execute(worker);
  }

  /* members */
  /**
   * Single threaded thread pool
   */
  private final ExecutorService executorService;
  /**
   * Current (or old) worker atomic reference for all tasks
   */
  private final AtomicReferenceArray<TaskSwingWorker<?>> currentTaskWorkers;

  /**
   * Private constructor
   * @param registry task registry only used to get the number of tasks
   */
  private TaskSwingWorkerExecutor(final TaskRegistry registry) {
    super();

    this.currentTaskWorkers = new AtomicReferenceArray<TaskSwingWorker<?>>(registry.getTaskCount());

    // Prepare the custom executor service with a single thread :
    this.executorService = new SwingWorkerSingleThreadExecutor(this);
  }

  /**
   * Stop all active worker threads immediately (interrupted)
   */
  private void shutdown() {
    this.executorService.shutdownNow();
  }

  /**
   * Schedules the given {@code TaskSwingWorker} for execution on a <i>worker</i>
   * thread. There is a Single <i>worker</i> thread available. In the
   * event the <i>worker</i> thread is busy handling other
   * {@code SwingWorkers} the given {@code SwingWorker} is placed in a waiting
   * queue.
   * NOTE : No synchronization HERE as it must be called from Swing EDT
   * @param worker TaskSwingWorker instance to execute
   */
  private final void execute(final TaskSwingWorker<?> worker) {
    // note : there is no synchronisation here because this method must be called from Swing EDT
    final Task task = worker.getTask();

    if (DEBUG_FLAG) {
      logger.info("execute task : " + task + " with worker = " + worker);
    }

    // cancel the running worker for the task and child tasks
    // and memorize the reference to the new worker before execution :
    this.cancelRelatedTasks(task, worker);

    if (DEBUG_FLAG) {
      logger.info("execute worker = " + worker);
    }

    // finally, execute the new worker with the custom executor service :
    this.executorService.execute(worker);
  }

  /**
   * Cancel any busy worker related to the given task and its child tasks
   * NOTE : No synchronization HERE as it must be called from Swing EDT
   * @param task to use
   * @param newWorker new worker for the given task
   */
  private final void cancelRelatedTasks(final Task task, final TaskSwingWorker<?> newWorker) {
    if (DEBUG_FLAG) {
      logger.info("cancel related tasks for = " + task);
    }
    // cancel any busy worker related to the given task :
    cancel(task, newWorker);

    // cancel any busy worker related to any child task :
    for (Task child : task.getChildTasks()) {
      cancel(child, null);
    }
  }

  /**
   * Cancel any busy worker for the given task
   * NOTE : No synchronization HERE as it must be called from Swing EDT
   * @param task task to find the current worker
   * @param newWorker new worker for the given task (can be null)
   */
  private final void cancel(final Task task, final TaskSwingWorker<?> newWorker) {
    // get current worker and set new worker :
    final TaskSwingWorker<?> currentWorker = this.currentTaskWorkers.getAndSet(task.getId(), newWorker);

    // cancel the current running worker for the given task :
    if (currentWorker != null) {
      // worker is still running ...
      if (DEBUG_FLAG) {
        logger.info("cancel worker = " + currentWorker);
      }

      // note : if the worker was previously cancelled, it has no effect.
      // interrupt the thread to have Thread.isInterrupted() == true :
      currentWorker.cancel(true);
    }
  }

  /**
   * Remove the given worker from the busy workers for its task.
   * Useful when the worker terminates its execution (cancelled or not)
   * @param worker worker to remove
   */
  private final void clearWorker(final TaskSwingWorker<?> worker) {
    // get current worker and clear reference :
    if (this.currentTaskWorkers.compareAndSet(worker.getTask().getId(), worker, null)) {
      if (DEBUG_FLAG) {
        logger.info("cleared worker = " + worker);
      }
    } else {
      if (DEBUG_FLAG) {
        logger.info("NOT cleared worker = " + worker + " - value is = " + this.currentTaskWorkers.get(worker.getTask().getId()));
      }
    }
  }

  /**
   * Single threaded Swing Worker executor
   */
  private static final class SwingWorkerSingleThreadExecutor extends ThreadPoolExecutor {

    /* members */
    /** TaskSwingWorkerExecutor reference for clearWorker callback */
    private final TaskSwingWorkerExecutor executor;

    /**
     * Create a single threaded Swing Worker executor
     * @param executor TaskSwingWorkerExecutor reference for clearWorker callback
     */
    public SwingWorkerSingleThreadExecutor(final TaskSwingWorkerExecutor executor) {
      super(1, 1,
              0L, TimeUnit.MILLISECONDS,
              new LinkedBlockingQueue<Runnable>(),
              new SwingWorkerThreadFactory());
      this.executor = executor;

      // Create the thread now :
      this.prestartCoreThread();
    }

    /**
     * Method invoked prior to executing the given Runnable in the
     * given thread.  This method is invoked by thread <tt>t</tt> that
     * will execute task <tt>r</tt>, and may be used to re-initialize
     * ThreadLocals, or to perform logging.
     *
     * <p>This implementation does nothing, but may be customized in
     * subclasses. Note: To properly nest multiple overridings, subclasses
     * should generally invoke <tt>super.beforeExecute</tt> at the end of
     * this method.
     *
     * @param t the thread that will run task r.
     * @param r the task that will be executed.
     */
    @Override
    protected void beforeExecute(final Thread t, final Runnable r) {
      if (DEBUG_FLAG) {
        logger.info("beforeExecute : " + r);
      }
    }

    /**
     * Method invoked upon completion of execution of the given Runnable.
     * This method is invoked by the thread that executed the task. If
     * non-null, the Throwable is the uncaught <tt>RuntimeException</tt>
     * or <tt>Error</tt> that caused execution to terminate abruptly.
     *
     * <p><b>Note:</b> When actions are enclosed in tasks (such as
     * {@link FutureTask}) either explicitly or via methods such as
     * <tt>submit</tt>, these task objects catch and maintain
     * computational exceptions, and so they do not cause abrupt
     * termination, and the internal exceptions are <em>not</em>
     * passed to this method.
     *
     * <p>This implementation does nothing, but may be customized in
     * subclasses. Note: To properly nest multiple overridings, subclasses
     * should generally invoke <tt>super.afterExecute</tt> at the
     * beginning of this method.
     *
     * @param r the runnable that has completed.
     * @param t the exception that caused termination, or null if
     * execution completed normally.
     */
    @Override
    protected void afterExecute(final Runnable r, final Throwable t) {
      if (DEBUG_FLAG) {
        if (t != null) {
          logger.log(Level.INFO, "afterExecute : " + r, t);
        } else {
          logger.info("afterExecute : " + r);
        }
      }
      if (r instanceof TaskSwingWorker<?>) {
        final TaskSwingWorker<?> worker = (TaskSwingWorker<?>) r;
        if (!worker.isCancelled()) {
          this.executor.clearWorker(worker);
        }
      }
    }
  }

  /**
   * Custom ThreadFactory implementation
   */
  private static final class SwingWorkerThreadFactory implements ThreadFactory {

    /** thread count */
    private final AtomicInteger threadNumber = new AtomicInteger(1);

    /**
     * Constructs a new {@code Thread}.
     *
     * @param r a runnable to be executed by new thread instance
     * @return constructed thread, or {@code null} if the request to
     *         create a thread is rejected
     */
    public Thread newThread(final Runnable r) {
      final StringBuilder name = new StringBuilder("SwingWorker-pool-");
      name.append(threadNumber.getAndIncrement());

      final Thread thread = new Thread(r, name.toString());
      if (thread.isDaemon()) {
        thread.setDaemon(false);
      }
      if (thread.getPriority() != Thread.NORM_PRIORITY) {
        thread.setPriority(Thread.NORM_PRIORITY);
      }

      // define UncaughtExceptionHandler :
      MCSExceptionHandler.installThreadHandler(thread);

      if (DEBUG_FLAG) {
        logger.info("new thread : " + thread.getName());
      }

      return thread;
    }
  }
}
