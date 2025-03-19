/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.interop.SearchCalVOTableHandler;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.jmcs.gui.action.RegisteredAction;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.network.http.Http;
import fr.jmmc.jmcs.service.BrowserLauncher;
import fr.jmmc.jmcs.util.FileUtils;
import fr.jmmc.jmcs.util.UrlUtils;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
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
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "getAllStars";
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(className);
    /** GetStar URL (query by identifier) */
    public static final String GETSTAR_QUERY_ID =
        "http://sclws.jmmc.fr/sclwsGetStarProxy.php?star=";

    /** GetStar separator (multiple identifier separator) */
    public static final String GETSTAR_SEPARATOR = ",";

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public GetStarAction() {
        super(className, actionName);
    }

    /** 
     * Handle the action event
     * @param evt action event
     */
    @Override
    public void actionPerformed(final ActionEvent evt) {
        logger.debug("actionPerformed");

        final List<Target> editTargets = ObservationManager.getInstance().getTargets();

        if (editTargets.isEmpty()) {
            return;
        }

        // later use async HttpSwingWorker ...
        final int len = editTargets.size();
        // TODO: check len ?

        final String[] ids = new String[len];
        for (int i = 0; i < len; i++) {
            final Target t = editTargets.get(i);
            ids[i] = t.getName();
        }

        final String url = buildQuery(ids);

        if (url != null) {
            logger.debug("GetStar url = {}", url);

            final File voTableFile = FileUtils.getTempFile("getstar.vot");

            try {
                final URI uri = new URI(url);

                logger.debug("downloading {} to {}", uri, voTableFile);

                if (Http.download(uri, voTableFile, false)) {
                    try {
                        final String votable = FileUtils.readFile(voTableFile);

                        StatusBar.show("file loaded : " + voTableFile.getName());

                        logger.debug("votable :\n{}", votable);

                        SearchCalVOTableHandler.processMessage(votable, "undefined");

                    } catch (IOException ioe) {
                        MessagePane.showErrorMessage("Could not load the file : " + voTableFile.getAbsolutePath(), ioe);
                    }
                }
            } catch (URISyntaxException use) {
                logger.warn("Bad URI:", use);
            } catch (IOException ioe) {
                logger.info("IO failure (no network / internet access ?):", ioe);
            }
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
        if (ids != null && ids.length != 0) {
            final String starValue;
            if (ids.length == 1) {
                starValue = ids[0];
            } else {
                final StringBuilder sb = new StringBuilder(ids.length * 20);
                for (String id : ids) {
                    sb.append(id).append(GETSTAR_SEPARATOR);
                }
                sb.deleteCharAt(sb.length() - 1);
                starValue = sb.toString();
            }
            return GetStarAction.GETSTAR_QUERY_ID + UrlUtils.encode(starValue);
        }
        return null;
    }
}
