/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.mcs.model.targetmodel.Model;

/**
 * This JTree contains Targets and their models
 * @author bourgesl
 */
public final class ModelJTree extends TargetJTree {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;

  /**
   * Public constructor
   * @param targetUserInfos target user informations
   */
  public ModelJTree(final TargetUserInformations targetUserInfos) {
    super(targetUserInfos);
  }

  /**
   * Convert a value object to string
   * @param userObject user object to convert
   * @return string representation of the user object
   */
  @Override
  protected String convertUserObjectToString(final Object userObject) {

    if (userObject instanceof Model) {
      final Model model = (Model) userObject;

      return model.getName();
    }

    return super.convertUserObjectToString(userObject);
  }
}
