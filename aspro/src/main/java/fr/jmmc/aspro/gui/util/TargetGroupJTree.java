/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.TargetGroup;
import fr.jmmc.aspro.model.oi.TargetUserInformations;

/**
 * This JTree contains Targets and their TargetGroups
 * @author bourgesl
 */
public final class TargetGroupJTree extends AbstractTargetJTree<Object> {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;

    /**
     * Public constructor
     * @param targetUserInfos target user informations
     */
    public TargetGroupJTree(final TargetUserInformations targetUserInfos) {
        super(null, targetUserInfos);
    }

    /**
     * Return the tooltip text for the given user object
     * @param userObject user object to get tooltip
     * @param sbTmp temporary buffer 
     * @return tooltip of the user object or null
     */
    @Override
    protected String getTooltipText(final Object userObject, final StringBuffer sbTmp) {
        if (userObject instanceof TargetGroup) {
            final TargetGroup group = (TargetGroup) userObject;
            return group.getTooltip();
        }
        return super.getTooltipText(userObject, sbTmp);
    }

    /**
     * Convert a non-null value object to string
     * @param userObject user object to convert
     * @return string representation of the user object
     */
    @Override
    protected String convertUserObjectToString(final Object userObject) {
        if (userObject instanceof TargetGroup) {
            final TargetGroup group = (TargetGroup) userObject;
            // Return the description:
            return group.getName();
        }
        return super.convertUserObjectToString(userObject);
    }
}
