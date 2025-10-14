/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.interop.SearchCalVOTableHandler;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.jmal.star.GetStarResolveJob;
import fr.jmmc.jmal.star.GetStarResolverResult;
import static fr.jmmc.jmal.star.StarResolver.GETSTAR_ALLOW_SCENARIO;
import static fr.jmmc.jmal.star.StarResolver.GETSTAR_QUERY_ID;
import fr.jmmc.jmal.star.StarResolverListener;
import fr.jmmc.jmcs.gui.action.RegisteredAction;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.service.BrowserLauncher;
import java.awt.event.ActionEvent;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * GetStar service wrapper: http query to get votable and import VOTable (targets)
 * @author bourgesl
 */
public final class GetStarAction extends RegisteredAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    public final static String className = GetStarAction.class.getName();
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(className);

    public static final StarResolverListener<GetStarResolverResult> GETSTAR_LISTENER = new StarResolverListener<GetStarResolverResult>() {
        /**
         * Handle the star resolver result as String (raw http response) or StarResolverResult instance (status, error messages, stars) ...
         * @param result star resolver result
         */
        @Override
        public void handleResult(final GetStarResolverResult result) {
            if (!result.isErrorStatus()) {
                final String votable = result.getXml();
                try {
                    logger.debug("votable :\n{}", votable);

                    // TODO: handle missing objects compared to queried names !
                    SearchCalVOTableHandler.processMessage(votable, "undefined", result.getNames());

                } catch (IOException ioe) {
                    MessagePane.showErrorMessage("Could not process GetStar VOTable", ioe);
                }
            }
        }
    };

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public GetStarAction() {
        super(className, "getStar");
    }

    /** 
     * Handle the action event
     * @param evt action event
     */
    @Override
    public void actionPerformed(final ActionEvent evt) {
        logger.debug("actionPerformed");

        // Use main observation to get (selected) targets :
        final ObservationSetting observation = ObservationManager.getInstance().getMainObservation();

        // retrieve the selected target from its name:
        final Target target = observation.getSelectedTarget();
        if (target == null) {
            return;
        }
        final List<Target> editTargets = Arrays.asList(new Target[]{target});

        if (editTargets.isEmpty()) {
            return;
        }

        // later use async HttpSwingWorker ...
        final int len = editTargets.size();

        final String[] ids = new String[len];
        for (int i = 0; i < len; i++) {
            final Target t = editTargets.get(i);
            ids[i] = t.getName();
        }

        final String url = buildQuery(ids);
        if (url != null) {
            logger.debug("GetStar url = {}", url);
            BrowserLauncher.openURL(url);
        }
    }

    public static void openGetStarInBrowser(final String id) {
        final String url = buildQuery(id);
        if (url != null) {
            logger.debug("GetStar url = {}", url);
            BrowserLauncher.openURL(url);
        }
    }

    private static String buildQuery(final String... ids) {
        final String queryParameters = (((ids != null) && (ids.length == 1)) ? GETSTAR_ALLOW_SCENARIO : null);
        final String queryString = GetStarResolveJob.buildQueryString(queryParameters, ids);
        if (queryString != null) {
            return GETSTAR_QUERY_ID + queryString;
        }
        return null;
    }
}
