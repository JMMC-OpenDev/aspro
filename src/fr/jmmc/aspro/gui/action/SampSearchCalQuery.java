/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SampSearchCalQuery.java,v 1.1 2010-10-05 18:24:07 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.action;

import edu.dartmouth.AstroSkyCalcObservation;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.AsproGui;
import fr.jmmc.aspro.gui.BasicObservationForm;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.service.NoiseService;
import fr.jmmc.aspro.service.NoiseService.Band;
import fr.jmmc.aspro.util.FileUtils;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.interop.SampCapability;
import fr.jmmc.mcs.interop.SampCapabilityAction;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

/**
 * This action asks SearchCal to search calibrators for the current selected target
 * @author bourgesl
 */
public final class SampSearchCalQuery extends SampCapabilityAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  private final static String className = "fr.jmmc.aspro.gui.action.SampSearchCalQuery";
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "searchCalStartQuery";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);

  /** template name */
  private final static String TEMPLATE_FILE = "fr/jmmc/aspro/gui/action/SearchCal_template.xml";

  /* keywords */
  /** keyword - objectName */
  public final static String KEY_TARGET_NAME = "_TARGET-NAME_";

  /** keyword - mag */
  public final static String KEY_MAG = "_MAG_";

  /** keyword - band */
  public final static String KEY_INS_BAND = "_INSBAND_";

  /** keyword - ra */
  public final static String KEY_RA = "_RA_";
  /** keyword - dec */
  public final static String KEY_DEC = "_DEC_";

  /** keyword - baseMax */
  public final static String KEY_BASE_MAX = "_BASEMAX_";
  /** keyword - wlen */
  public final static String KEY_WAVELENGTH = "_WAVELENGTH_";

  /* members */
  
  /**
   * Public constructor that automatically register the action in RegisteredAction.
   */
  public SampSearchCalQuery() {
    super(className, actionName, SampCapability.SEARCHCAL_START_QUERY);
  }

  public Map<?, ?> composeMessage() {

    final BasicObservationForm form = AsproGui.getInstance().getSettingPanel().getObservationForm();

    // extract UV Coverage Panel information :
    final String targetName = form.getSelectedTargetName();

    if (targetName == null) {
      MessagePane.showMessage("Please select a target before calling SearchCal");
      return null;
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("composeMessage for target : " + targetName);
    }

    final String votable = processTarget(targetName);

    final Map<String, String> parameters = new HashMap<String, String>();
    parameters.put("query", votable);

    logger.severe("votable = \n" + votable);

    return parameters;
  }

  /**
   * Load and fill the searchCal votable
   * @param targetName target to use
   * @return SearchCal votable as string
   */
  private String processTarget(final String targetName) {

    // get OB template :
    String votable = FileUtils.readFile(TEMPLATE_FILE);

    // get observation and target :
    final ObservationSetting observation = ObservationManager.getInstance().getObservation();
    final Target target = observation.getTarget(targetName);

    final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();
    if (insMode == null) {
      throw new IllegalStateException("the instrumentMode is empty !");
    }
    // Get chosen stations :
    final List<Station> stations = observation.getInstrumentConfiguration().getStationList();
    if (stations == null) {
      throw new IllegalStateException("prepareBeams : the station list is null !");
    }

    final double lambda = insMode.getWaveLength();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("lambda = " + lambda);
    }

    final NoiseService.Band band = NoiseService.Band.findBand(lambda);
    final SpectralBand insBand = Band.findBand(band);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("band    = " + band);
      logger.fine("insBand = " + insBand);
    }

    // If a flux / magnitude is missing => user message
    // and it is impossible to compute any error

    final Double flux = target.getFlux(insBand);

    final double objectMag = (flux != null) ? flux.doubleValue() : Double.NaN;
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("objectMag                  = " + objectMag);
    }

    // max base line :
    final double[] range = ConfigurationManager.computeLimitsUVCoverage(stations);
    final double maxBaseline = range[1];

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("maxBaseline = " + maxBaseline);
    }

    // --- Target information ---

    votable = votable.replaceFirst(KEY_TARGET_NAME, targetName);

    // magnitude in instrument band : TODO
    votable = votable.replaceFirst(KEY_MAG, Double.toString(objectMag));
    votable = votable.replaceFirst(KEY_INS_BAND, insBand.value());

    // convert RA/DEC (mas) up to 3 digits :
    final String[] raDec = AstroSkyCalcObservation.toString(target.getRADeg(), target.getDECDeg());

    votable = votable.replaceFirst(KEY_RA, raDec[0]);
    votable = votable.replaceFirst(KEY_DEC, raDec[1]);

    votable = votable.replaceFirst(KEY_BASE_MAX, Double.toString(maxBaseline));
    votable = votable.replaceFirst(KEY_WAVELENGTH, Double.toString(lambda));

    return votable;
  }
}
