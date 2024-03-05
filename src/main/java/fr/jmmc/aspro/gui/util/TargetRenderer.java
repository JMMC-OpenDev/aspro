/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.jmcs.util.ImageUtils;
import javax.swing.ImageIcon;
import javax.swing.JLabel;

/**
 *
 * @author bourgesl
 */
public final class TargetRenderer {

    /** Common resource directory containing icon files */
    private final static String IMAGE_RESOURCE_COMMON_PATH = "fr/jmmc/aspro/gui/icons/";

    private final static ImageIcon IMG_CAL = ImageUtils.loadResourceIcon(IMAGE_RESOURCE_COMMON_PATH + "calibrator.png");
    private final static ImageIcon IMG_SCI = ImageUtils.loadResourceIcon(IMAGE_RESOURCE_COMMON_PATH + "target.png");

    /* members */
    /** target user informations */
    private final TargetUserInformations targetUserInfos;
    /** temporary buffer */
    protected final StringBuilder sbTmp = new StringBuilder(32);

    /**
     * Public constructor
     * @param targetUserInfos target user informations
     */
    public TargetRenderer(final TargetUserInformations targetUserInfos) {
        this.targetUserInfos = targetUserInfos;
    }

    public TargetUserInformations getTargetUserInfos() {
        return targetUserInfos;
    }

    /**
     * Convert the target to string :
     * Return the display name of the given target using the format 'name' ( ' (cal)')
     * @param target target to use
     * @return display name
     */
    public String convertTargetToString(final Target target) {
        if (this.targetUserInfos != null) {
            final StringBuilder sb = sbTmp;
            sb.setLength(0); // clear
            this.targetUserInfos.getTargetDisplayName(target, sb);
            return sb.toString();
        }
        return null;
    }

    /**
     * Defines the label icon according to the target type (calibrator or science)
     * @param label to use
     * @param target target to check
     */
    public void setIcon(final JLabel label, final Target target) {
        if (this.targetUserInfos != null && this.targetUserInfos.isCalibrator(target)) {
            label.setIcon(IMG_CAL);
        } else {
            label.setIcon(IMG_SCI);
        }
    }
}
