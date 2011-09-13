/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.AsproGui;
import fr.jmmc.aspro.gui.BasicObservationForm;
import fr.jmmc.jmcs.gui.action.RegisteredAction;
import java.awt.event.ActionEvent;
import java.util.logging.Level;

/**
 * This action opens the target editor
 * @author bourgesl
 */
public class TargetEditorAction extends RegisteredAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = TargetEditorAction.class.getName();
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "targetEditor";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);

  /**
   * Public constructor that automatically register the action in RegisteredAction.
   */
  public TargetEditorAction() {
    super(className, actionName);
  }

  /**
   * Handle the action event
   * @param evt action event
   */
  @Override
  public void actionPerformed(final ActionEvent evt) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("actionPerformed");
    }

    final BasicObservationForm form = AsproGui.getInstance().getSettingPanel().getObservationForm();

    form.showTargetEditor();
  }
}
