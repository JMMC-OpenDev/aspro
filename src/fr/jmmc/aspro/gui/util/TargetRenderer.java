/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TargetRenderer.java,v 1.1 2010-12-10 17:09:48 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
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
