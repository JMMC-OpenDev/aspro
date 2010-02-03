/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SwingWorkerExecutor.java,v 1.1 2010-02-03 16:06:47 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.util;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
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
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
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
   * @param worker SwingWorker instance to execute
   */
  public final void execute(final String taskFamily, final SwingWorker<?, ?> worker) {
    // note : there is no synchronisation here because this method must be called from EDT thread

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("execute : " + taskFamily + " = " + worker);
    }

    final WeakReference<SwingWorker<?, ?>> oldRef = this.currentWorkerMap.get(taskFamily);

    // first, cancel the current running worker for the given task family :
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
   * Prepare the executor service with a single thread
   */
  private static ExecutorService getWorkersExecutorService() {
    // custom thread factory :
    final ThreadFactory threadFactory = new ThreadFactory() {

      private final AtomicInteger threadNumber = new AtomicInteger(1);

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
        return t;
      }
    };

    // Single threaded Swing Worker executor :
    return Executors.newSingleThreadExecutor(threadFactory);
  }
}
