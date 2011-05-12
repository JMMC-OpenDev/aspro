/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JLabel;

/**
 *
 * @author bourgesl
 */
public final class TargetRenderer {

  /* members */
  /** icon for calibrator targets */
  private final Icon calibratorIcon;
  /** icon for science targets */
  private final Icon targetIcon;
  /** target user informations */
  private final TargetUserInformations targetUserInfos;

  /**
   * Public constructor
   * @param targetUserInfos target user informations
   */
  public TargetRenderer(final TargetUserInformations targetUserInfos) {
    this.calibratorIcon = new ImageIcon(getClass().getResource("/fr/jmmc/aspro/gui/icons/calibrator.png"));
    this.targetIcon = new ImageIcon(getClass().getResource("/fr/jmmc/aspro/gui/icons/target.png"));
    this.targetUserInfos = targetUserInfos;
  }

  /**
   * Convert the target to string :
   * Return the display name of the given target using the format 'name' ( ' (cal)')
   * @param target target to use
   * @return display name
   */
  public final String convertTargetToString(final Target target) {
    return this.targetUserInfos.getTargetDisplayName(target);
  }

  /**
   * Defines the label icon according to the target type (calibrator or science)
   * @param label to use
   * @param target target to check
   */
  public void setIcon(final JLabel label,
                      final Target target) {

    if (this.targetUserInfos != null && this.targetUserInfos.isCalibrator(target)) {
      label.setIcon(this.calibratorIcon);
    } else {
      label.setIcon(this.targetIcon);
    }
  }
}
