/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import java.awt.event.ActionEvent;
import java.util.List;

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
            this.process(targets);
        }
    }

}
