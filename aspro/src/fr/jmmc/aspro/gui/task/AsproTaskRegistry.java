/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AsproTaskRegistry.java,v 1.2 2011-02-14 17:13:07 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2011/01/21 16:30:38  bourgesl
 * create Aspro Tasks and describe their dependencies
 *
 */
package fr.jmmc.aspro.gui.task;

import fr.jmmc.mcs.gui.task.Task;
import fr.jmmc.mcs.gui.task.TaskRegistry;

/**
 * This class describes the Aspro tasks associated with SwingWorker(s) and their execution order
 * @author bourgesl
 */
public final class AsproTaskRegistry extends TaskRegistry {

  /** task registry singleton */
  private final static AsproTaskRegistry instance;

  /* ASPRO tasks */
  /** Observability task */
  public final static Task TASK_OBSERVABILITY;
  /** UV Coverage task */
  public final static Task TASK_UV_COVERAGE;
  /** UV Map task */
  public final static Task TASK_UV_MAP;

  /** Fits task (future) */
  /*
  public final static Task TASK_OIFITS;
   */
  /**
   * Static initializer to define tasks and their child tasks
   */
  static {
    // create the task registry singleton :
    instance = new AsproTaskRegistry();

    // create tasks :
    TASK_OBSERVABILITY = new Task("Observability");
    TASK_UV_COVERAGE = new Task("UVCoverage");
    TASK_UV_MAP = new Task("UVMap");
//    TASK_OIFITS = new Task(n++, "OIFits");

    // register tasks :
    instance.addTask(TASK_OBSERVABILITY);
    instance.addTask(TASK_UV_COVERAGE);
    instance.addTask(TASK_UV_MAP);

    // task chain :
    final Task[] tasks = new Task[]{
      TASK_OBSERVABILITY, TASK_UV_COVERAGE, TASK_UV_MAP /* , TASK_OIFITS */};

    // iterate over tasks :
    for (int i = 0, n = tasks.length; i < n; i++) {

      final int childCount = n - i - 1;

      final Task[] childTasks = new Task[childCount];

      for (int j = 0; j < childCount; j++) {
        childTasks[j] = tasks[i + j + 1];
      }

      instance.setChildTasks(tasks[i], childTasks);
    }
  }

  /**
   * Singleton pattern for the registry itself
   * @return registry instance
   */
  public static TaskRegistry getInstance() {
    return instance;
  }

  /**
   * Protected constructor
   */
  private AsproTaskRegistry() {
    super();
  }
}
