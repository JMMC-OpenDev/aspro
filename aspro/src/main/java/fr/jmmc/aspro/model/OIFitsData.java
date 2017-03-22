/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.oitools.model.OIFitsFile;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * This class gathers OIFits structures with their corresponding warning container
 * @author bourgesl
 */
public final class OIFitsData {

    /** empty OIFits List indicating that the user did not accept the warning confirmation dialog */
    public final static List<OIFitsFile> IGNORE_OIFITS_LIST = Collections.emptyList();

    /** computed OIFits structures */
    private final List<OIFitsFile> oiFitsList;
    /** warning container corresponding to the computed OIFits structures */
    private final WarningContainer oiFitsWarningContainer;

    /**
     * Public constructor
     * @param oiFitsList OIFits structures
     * @param warningContainer warning container including the OIFits generation
     */
    public OIFitsData(final List<OIFitsFile> oiFitsList, final WarningContainer warningContainer) {
        this.oiFitsList = oiFitsList;
        this.oiFitsWarningContainer = warningContainer;
    }

    /**
     * Return the computed OIFits structures (read only)
     * @return OIFits structure or null
     */
    public List<OIFitsFile> getOIFitsList() {
        return this.oiFitsList;
    }

    /**
     * Return the computed OIFits structures (read only) but check warnings and potentially display a confirmation dialog
     * @return OIFits structure or EMPTY_OIFITS (confirmation dialog discarded) or null (no OIFITS computed)
     */
    public List<OIFitsFile> checkAndGetOIFitsList() {
        if ((this.oiFitsList != null) && (this.oiFitsWarningContainer != null)) {
            // Show warning dialog if missing magnitudes:
            final List<String> warnings = getWarningMessages();
            if (warnings != null) {
                final StringBuilder sb = new StringBuilder(256);
                sb.append("The OIFits computation reported an important warning:\n\n");
                for (String msg : warnings) {
                    sb.append(msg).append("\n");
                }
                sb.append("\n Are you sure you want to continue ?");

                if (!MessagePane.showConfirmMessage(sb.toString())) {
                    return IGNORE_OIFITS_LIST;
                }
            }
        }
        return this.oiFitsList;
    }

    /**
     * Return the list of important warning messages
     * @return list of important warning messages or null if no match
     */
    public List<String> getWarningMessages() {
        if (this.oiFitsWarningContainer != null) {
            List<String> messages = null;

            final String msg = this.oiFitsWarningContainer.getMatchingWarningMessage(AsproConstants.WARN_MISSING_MAGS);
            if (msg != null) {
                if (messages == null) {
                    messages = new ArrayList<String>(2);
                }
                messages.add(msg);
            }
            if (messages != null && !messages.isEmpty()) {
                return messages;
            }
        }
        return null;
    }

    /**
     * Return the warning container corresponding to the computed OIFits structures (read only)
     * (unused)
     * @return warning container or null
     */
    public WarningContainer getOIFitsWarningContainer() {
        return this.oiFitsWarningContainer;
    }

    @Override
    public String toString() {
        return "OIFitsData{" + "oiFitsList=" + oiFitsList + ", oiFitsWarningContainer=" + oiFitsWarningContainer + '}';
    }

}
