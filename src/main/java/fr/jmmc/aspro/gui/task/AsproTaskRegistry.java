/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.task;

import fr.jmmc.jmcs.gui.task.Task;
import fr.jmmc.jmcs.gui.task.TaskRegistry;

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
    /** Fits task */
    public final static Task TASK_OIFITS;
    /** QueryObs task (future) */
    public final static Task TASK_QUERY_OBS;

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
        TASK_OIFITS = new Task("OIFits");
        TASK_QUERY_OBS = new Task("QueryObs");

        // register tasks :
        instance.addTask(TASK_OBSERVABILITY);
        instance.addTask(TASK_UV_COVERAGE);
        instance.addTask(TASK_OIFITS);
        instance.addTask(TASK_UV_MAP);

        // task chain :
        final Task[] tasks = new Task[]{TASK_OBSERVABILITY, TASK_UV_COVERAGE, TASK_OIFITS, TASK_UV_MAP};

        // iterate over tasks :
        for (int i = 0, n = tasks.length; i < n; i++) {

            final int childCount = n - i - 1;

            final Task[] childTasks = new Task[childCount];

            for (int j = 0; j < childCount; j++) {
                childTasks[j] = tasks[i + j + 1];
            }

            instance.setChildTasks(tasks[i], childTasks);
        }
        // out of loop to not add child tasks:
        instance.addTask(TASK_QUERY_OBS);
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
