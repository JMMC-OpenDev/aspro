/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import java.awt.event.ActionEvent;
import java.util.Arrays;

/**
 * Query raw observations action
 * @author bourgesl
 */
public final class QueryOneRawObservationsAction extends QueryRawObservationsAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    public final static String className = QueryOneRawObservationsAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "queryRawObservations";

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public QueryOneRawObservationsAction() {
        super(className, actionName);
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

        // retrieve the selected target from its name:
        final Target target = observation.getTarget(observation.getSelectedTargetName());

        if (target != null) {
            // launch a new worker
            new QueryObsPortalWorker(Arrays.asList(new Target[]{target})).executeTask();
        }
    }
}
