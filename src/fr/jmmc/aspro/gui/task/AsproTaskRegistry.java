/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AsproTaskRegistry.java,v 1.1 2011-01-21 16:30:38 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.task;

/**
 * This class describes the Aspro tasks associated to SwingWorker and their ordering
 * @author bourgesl
 */
public final class AsproTaskRegistry implements TaskRegistry {

  /* Aspro tasks */
  /** Observability task */
  public final static Task TASK_OBSERVABILITY;
  /** UV Coverage task */
  public final static Task TASK_UV_COVERAGE;
  /** UV Map task */
  public final static Task TASK_UV_MAP;
  /** Fits task (future) */
//  public final static Task TASK_OIFITS;
  /** task count */
  public final static int TASK_COUNT;
  /** task registry singleton */
  private final static AsproTaskRegistry instance;

  /**
   * Static initializer to define tasks (id, name) and child tasks
   */
  static {
    int n = 0;

    // create tasks :
    TASK_OBSERVABILITY = new Task(n++, "Observability");
    TASK_UV_COVERAGE = new Task(n++, "UVCoverage");
    TASK_UV_MAP = new Task(n++, "UVMap");
//    TASK_OIFITS = new Task(n++, "OIFits");

    final Task[] tasks = new Task[]{
      TASK_OBSERVABILITY, TASK_UV_COVERAGE, TASK_UV_MAP /* , TASK_OIFITS */};

    TASK_COUNT = tasks.length;

    // iterate over tasks :
    for (int i = 0; i < n; i++) {

      final int childCount = n - i - 1;

      final Task[] childTasks = new Task[childCount];

      for (int j = 0; j < childCount; j++) {
        childTasks[j] = tasks[i + j + 1];
      }

      tasks[i].setChildTasks(childTasks);
    }

    instance = new AsproTaskRegistry();
  }

  /**
   * Singleton pattern for the registry itself
   * @return registry instance
   */
  public static TaskRegistry getInstance() {
    return instance;
  }

  /**
   * Private constructor
   */
  private AsproTaskRegistry() {
    // no-op
  }

  /**
   * Return the number of tasks
   * @return number of tasks
   */
  public int getTaskCount() {
    return TASK_COUNT;
  }
}
