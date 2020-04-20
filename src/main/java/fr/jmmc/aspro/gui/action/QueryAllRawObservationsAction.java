/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.Aspro2;
import fr.jmmc.aspro.gui.task.AsproTaskRegistry;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.gui.task.TaskSwingWorkerExecutor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.util.List;
import javax.swing.JPanel;
import javax.swing.JProgressBar;

/**
 * Query raw observations action
 * @author bourgesl
 */
public final class QueryAllRawObservationsAction extends QueryRawObservationsAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public QueryAllRawObservationsAction() {
        super(QueryAllRawObservationsAction.class.getName(), "query");
    }

    /**
     * Handle the action event
     * @param evt action event
     */
    @Override
    public void actionPerformed(final ActionEvent evt) {
        logger.debug("actionPerformed");

        // Use main observation to check instrument :
        final ObservationSetting observation = ObservationManager.getInstance().getMainObservation();

        // retrieve the targets:
        final List<Target> targets = observation.getTargets();

        if (!targets.isEmpty()) {
            if (targets.size() == 1) {
                QueryOneRawObservationsAction.query(targets.get(0));
            } else {
                // Create progress panel:
                final JProgressBar progressBar = new JProgressBar();
                final JPanel progressPanel = createQueryAllProgressPanel(progressBar);

                StatusBar.addCustomPanel(progressPanel);

                // launch a new worker
                new QueryObsPortalWorker(targets, new QueryObsListener() {

                    @Override
                    public void propertyChange(final PropertyChangeEvent pce) {
                        if ("progress".equals(pce.getPropertyName())) {
                            progressBar.setValue((Integer) pce.getNewValue());
                        }
                    }

                    @Override
                    public void done(final boolean cancelled) {
                        StatusBar.removeCustomPanel(progressPanel);
                    }
                }).executeTask(true);
            }
        }
    }

    static JPanel createQueryAllProgressPanel(final JProgressBar progressBar) {
        return Aspro2.createProgressPanel("querying observations ...", progressBar,
                new ActionListener() {

            @Override
            public void actionPerformed(final ActionEvent e) {
                TaskSwingWorkerExecutor.cancelTask(AsproTaskRegistry.TASK_QUERY_OBS);
            }
        });
    }
}
