/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.jmal.model.targetmodel.Model;

/**
 * This JTree contains Targets and their models
 * @author bourgesl
 */
public final class ModelJTree extends AbstractTargetJTree<Object> {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;

    /**
     * Public constructor
     * @param targetUserInfos target user informations
     */
    public ModelJTree(final TargetUserInformations targetUserInfos) {
        super(null, targetUserInfos);
    }

    /**
     * Convert a non-null value object to string
     * @param userObject user object to convert
     * @return string representation of the user object
     */
    @Override
    protected String convertUserObjectToString(final Object userObject) {
        if (userObject instanceof Model) {
            return ((Model) userObject).getName();
        }
        return super.convertUserObjectToString(userObject);
    }
}
