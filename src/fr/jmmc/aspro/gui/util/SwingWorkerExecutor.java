/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SwingWorkerExecutor.java,v 1.4 2010-09-20 12:16:26 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
package fr.jmmc.aspro.gui.util;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import org.jdesktop.swingworker.SwingWorker;

/**
 * This class is a customization of the standard SwingWorker to have a single thread only
 * processing workers because our computations require serialization and cancellation
 * @author bourgesl
 */
public final class SwingWorkerExecutor {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.util.SwingWorkerExecutor";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** singleton instance */
  private static SwingWorkerExecutor instance = null;

  /* members */
  /**
   * Single threaded thread pool
   */
  private final ExecutorService executorService;
  /**
   * Current (or old) worker reference for a given task family
   * (can be a leaked reference if the computation is done)
   */
  private final Map<String, WeakReference<SwingWorker<?, ?>>> currentWorkerMap = new HashMap<String, WeakReference<SwingWorker<?, ?>>>();

  /**
   * This code returns the singleton instance.
   * NOTE : No synchronization HERE as it must be called from Swing EDT
   * @return SwingWorkerExecutor
   */
  public static SwingWorkerExecutor getInstance() {
    if (instance == null) {
      // note : there is no synchronisation here because this method must be called from EDT thread
      instance = new SwingWorkerExecutor();
    }
    return instance;
  }

  /**
   * Private constructor
   */
  private SwingWorkerExecutor() {
    super();

    this.executorService = getWorkersExecutorService();
  }

  /**
   * Schedules the given {@code SwingWorker} for execution on a <i>worker</i>
   * thread. There is a Single <i>worker</i> threads available. In the
   * event the <i>worker</i> thread is busy handling other
   * {@code SwingWorkers} the given {@code SwingWorker} is placed in a waiting
   * queue.
   * NOTE : No synchronization HERE as it must be called from Swing EDT
   * @param taskFamily task identifier
   * @param worker SwingWorker instance to execute
   */
  public final void execute(final String taskFamily, final SwingWorker<?, ?> worker) {
    // note : there is no synchronisation here because this method must be called from EDT thread

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("execute : " + taskFamily + " = " + worker);
    }
    // first, cancel the current running worker for the given task family :
    this.cancel(taskFamily);

    // second, memorize the reference to the new worker before execution :
    final WeakReference<SwingWorker<?, ?>> newRef = new WeakReference<SwingWorker<?, ?>>(worker);
    this.currentWorkerMap.put(taskFamily, newRef);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("execute worker = " + worker);
    }

    // finally, execute the new worker with the custom executor service :
    this.executorService.execute(worker);
  }

  /**
   * Cancel any busy worker for the given task family
   * NOTE : No synchronization HERE as it must be called from Swing EDT
   * @param taskFamily task identifier
   */
  public final void cancel(final String taskFamily) {
    final WeakReference<SwingWorker<?, ?>> oldRef = this.currentWorkerMap.get(taskFamily);

    // cancel the current running worker for the given task family :
    if (oldRef != null) {
      final SwingWorker<?, ?> currentWorker = oldRef.get();
      if (currentWorker != null) {
        // reference is valid, so it is still running ...
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("currentWorker to cancel = " + currentWorker);
        }

        // note : if the worker was previously cancelled, it has no effect.
        // interrupt the thread to have Thread.isInterrupted() == true :
        currentWorker.cancel(true);
      }
      oldRef.clear();
    }
  }

  /**
   * Prepare the executor service with a single thread
   * @return executor service with a single thread
   */
  private static ExecutorService getWorkersExecutorService() {
    return new SwingWorkerSingleThreadExecutor();
  }

  /**
   * Single threaded Swing Worker executor
   */
  private static final class SwingWorkerSingleThreadExecutor extends ThreadPoolExecutor {

    /**
     * Create a single threaded Swing Worker executor
     */
    public SwingWorkerSingleThreadExecutor() {
      super(1, 1,
              0L, TimeUnit.MILLISECONDS,
              new LinkedBlockingQueue<Runnable>(),
              new SwingWorkerThreadFactory());
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
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("beforeExecute : " + r);
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
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("afterExecute : " + r);
      }
    }

    /**
     * Method invoked when the Executor has terminated.  Default
     * implementation does nothing. Note: To properly nest multiple
     * overridings, subclasses should generally invoke
     * <tt>super.terminated</tt> within this method.
     */
    @Override
    protected void terminated() {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("terminated");
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
      name.append(System.identityHashCode(this));
      name.append("-thread-");
      name.append(threadNumber.getAndIncrement());

      final Thread t = new Thread(r, name.toString());
      if (t.isDaemon()) {
        t.setDaemon(false);
      }
      if (t.getPriority() != Thread.NORM_PRIORITY) {
        t.setPriority(Thread.NORM_PRIORITY);
      }

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("newThread : " + t.getName());
      }

      return t;
    }
  }
}
