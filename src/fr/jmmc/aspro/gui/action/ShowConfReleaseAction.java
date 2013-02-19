/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.jmcs.data.ApplicationDescription;
import fr.jmmc.jmcs.data.model.Change;
import fr.jmmc.jmcs.data.model.Prerelease;
import fr.jmmc.jmcs.data.model.Release;
import fr.jmmc.jmcs.gui.action.RegisteredAction;
import fr.jmmc.jmcs.gui.component.ResizableTextViewFactory;
import fr.jmmc.jmcs.util.StringUtils;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This action generates release notes for the loaded configuration
 * @author bourgesl
 */
public final class ShowConfReleaseAction extends RegisteredAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    public final static String className = ShowConfReleaseAction.class.getName();
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "showReleaseNotes";
    /** Class logger */
    private final static Logger logger = LoggerFactory.getLogger(className);
    /* members */
    /** version */
    private String _windowTitle;
    /** html content (cached) */
    private String _windowContent;

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public ShowConfReleaseAction() {
        super(className, actionName);

        generateHtml();
    }

    /**
     * Handle the action event
     * @param evt action event
     */
    @Override
    public void actionPerformed(final ActionEvent evt) {
        logger.debug("actionPerformed");

        ResizableTextViewFactory.createHtmlWindow(_windowContent, this._windowTitle, false);
    }

    /**
     * Generate Html content
     */
    private void generateHtml() {

        // Get jMCS data
        final ApplicationDescription desc = ConfigurationManager.getInstance().getConfDescription();

        this._windowTitle = "Aspro2 configuration " + desc.getProgramVersion() + " release notes";

        // Compose jMCS header
        final StringBuilder generatedHtml = new StringBuilder(8 * 1024);
        generatedHtml.append("<html><body>");

        generatedHtml.append("<h1>").append(this._windowTitle).append("</h1>\n");

        // extracted changes per type:
        final List<Change> changeList = new ArrayList<Change>();

        for (Release r : desc.getReleases()) {
            generatedHtml.append("<h2>").append("Version ").append(r.getVersion()).append("</h2>\n");
            generatedHtml.append("<p>");

            if (r.getPubDate() != null) {
                generatedHtml.append(r.getPubDate()).append('\n');
            }

            generatedHtml.append("<ul>\n");

            processChangeType("FEATURE", "Features", r.getPrereleases(), generatedHtml, changeList);
            processChangeType("CHANGE", "Changes", r.getPrereleases(), generatedHtml, changeList);
            processChangeType("BUGFIX", "Bug fixes", r.getPrereleases(), generatedHtml, changeList);
            processChangeType(null, "Other", r.getPrereleases(), generatedHtml, changeList);

            generatedHtml.append("</ul>\n");

            generatedHtml.append("</p>\n");
        }

        generatedHtml.append("</body></html>");

        this._windowContent = generatedHtml.toString();
    }

    private void processChangeType(final String type, final String label, final List<Prerelease> prereleases, final StringBuilder generatedHtml, final List<Change> changeList) {
        if (findChangeByType(type, prereleases, changeList)) {
            generatedHtml.append("<li>").append(label).append(":</li>\n");
            generatedHtml.append("<ul>\n");

            for (Change c : changeList) {
                generatedHtml.append("<li>").append(c.getValue()).append("</li>\n");
            }
            generatedHtml.append("</ul>\n");
        }
    }

    private boolean findChangeByType(final String type, final List<Prerelease> prereleaseList, final List<Change> changeList) {
        changeList.clear();

        final boolean noType = StringUtils.isEmpty(type);

        for (Prerelease p : prereleaseList) {
            for (Change c : p.getChanges()) {
                if (noType) {
                    if (StringUtils.isEmpty(c.getType())) {
                        changeList.add(c);
                    }
                } else {
                    if (type.equalsIgnoreCase(c.getType())) {
                        changeList.add(c);
                    }
                }
            }
        }
        return !changeList.isEmpty();
    }
}
