/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.interop;

import fr.jmmc.aspro.ob.ExportOBVega;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.interop.SampCapability;
import fr.jmmc.mcs.interop.SampCapabilityAction;
import fr.jmmc.mcs.util.FileUtils;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;

/**
 * This action sends the starlist to Pivot
 *
 * @author bourgesl
 */
public final class StarListSendAction extends SampCapabilityAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  private final static String className = StarListSendAction.class.getName();
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "starListSendAction";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);
  /** SAMP Parameter 'starlist' */
  public final static String PARAM_STAR_LIST = "starlist";
  /** SAMP Parameter 'request_id' */
  public final static String PARAM_REQUEST_ID = "request_id";

  /**
   * Public constructor that automatically register the action in RegisteredAction.
   */
  public StarListSendAction() {
    super(className, actionName, SampCapability.LOAD_STAR_LIST);
  }

  /**
   * Create the Samp message parameters with the SearchCal votable
   * @return Samp message parameters as a map
   */
  public Map<?, ?> composeMessage() {
    Map<String, String> parameters = null;

    final File file = FileUtils.getTempFile("starlist.txt");
    try {

      ExportOBVega.process(file);

      parameters = new HashMap<String, String>(4);
      parameters.put(PARAM_STAR_LIST, file.toURI().toString());
      
      // TODO: use request Id stored in observation context
      parameters.put(PARAM_REQUEST_ID, null);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("parameters = \n" + parameters);
      }

    } catch (IOException ioe) {
      MessagePane.showErrorMessage("Could not export to file : " + file.getAbsolutePath(), ioe);
    }

    return parameters;
  }
}
