/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.interop;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.ob.ExportOBVega;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.network.interop.SampCapability;
import fr.jmmc.jmcs.network.interop.SampCapabilityAction;
import fr.jmmc.jmcs.util.FileUtils;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
  private static final Logger logger = LoggerFactory.getLogger(className);
  /** SAMP Parameter 'starlist' */
  public final static String PARAM_STAR_LIST = "starlist";

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
  @Override
  public Map<?, ?> composeMessage() {
    Map<String, String> parameters = null;

    // Test if chara is the interferometer:

    // Use main observation to check instrument :
    final ObservationSetting observation = ObservationManager.getInstance().getMainObservation();

    if (!observation.isSingle()) {
      MessagePane.showMessage("Aspro 2 can not send a star list when multiple configurations are selected !");
      return null;
    }

    final String insName = observation.getInstrumentConfiguration().getName();

    if (insName.startsWith(AsproConstants.INS_VEGA)) {
      final File file = FileUtils.getTempFile("starlist.txt");
      try {

        ExportOBVega.process(file);

        parameters = new HashMap<String, String>(4);
        parameters.put(PARAM_STAR_LIST, file.toURI().toString());

        logger.debug("parameters = \n{}", parameters);

      } catch (IOException ioe) {
        MessagePane.showErrorMessage("Could not export to file : " + file.getAbsolutePath(), ioe);
      }
    } else {
      MessagePane.showMessage("Aspro 2 can only send a star list for the VEGA instrument !");
    }

    return parameters;
  }
}
