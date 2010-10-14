/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SampSearchCalQuery.java,v 1.3 2010-10-14 13:12:53 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/10/06 16:06:33  bourgesl
 * finalization : define bright, min/Max MagRange correctly
 *
 * Revision 1.1  2010/10/05 18:24:07  bourgesl
 * first running searchCal start query integration through SAMP (but cause bugs in SearchCal)
 *
 */
package fr.jmmc.aspro.gui.action;

import edu.dartmouth.AstroSkyCalcObservation;
import fr.jmmc.aspro.AsproGui;
import fr.jmmc.aspro.gui.BasicObservationForm;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.util.SpectralBandUtils;
import fr.jmmc.aspro.util.FileUtils;
import fr.jmmc.mcs.astro.Band;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.interop.SampCapability;
import fr.jmmc.mcs.interop.SampCapabilityAction;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

/**
 * This action asks SearchCal to search calibrators for the current selected target
 *
 * TODO : rename that class
 *
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
  /** keyword - minMagRange */
  public final static String KEY_MAG_MIN = "_MAG_MIN_";
  /** keyword - maxMagRange */
  public final static String KEY_MAG_MAX = "_MAG_MAX_";
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
  /** keyword - bright */
  public final static String KEY_BRIGHT = "_BRIGHT_MODE_";
  /** maximal magnitude for bright scenario */
  private final static double BRIGHT_MAG_MAX = 5.5d;
  /** default minimal magnitude if magnitude is undefined */
  private final static double DEF_MAG_MIN = 2d;
  /** default maximal magnitude if magnitude is undefined */
  private final static double DEF_MAG_MAX = 4d;

  /* members */
  /**
   * Public constructor that automatically register the action in RegisteredAction.
   */
  public SampSearchCalQuery() {
    super(className, actionName, SampCapability.SEARCHCAL_START_QUERY);
  }

  /**
   * Create the Samp message parameters with the SearchCal votable
   * @return Samp message parameters as a map
   */
  public Map<?, ?> composeMessage() {

    final BasicObservationForm form = AsproGui.getInstance().getSettingPanel().getObservationForm();

    // extract UV Coverage Panel information :
    final String targetName = form.getSelectedTargetName();

    if (targetName == null) {
      MessagePane.showMessage("Please select a target before calling SearchCal in the target list");
      return null;
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("composeMessage for target : " + targetName);
    }

    final String votable = processTarget(targetName);

    final Map<String, String> parameters = new HashMap<String, String>();
    parameters.put("query", votable);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("votable = \n" + votable);
    }

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

    final Band band = Band.findBand(lambda);
    final SpectralBand insBand = SpectralBandUtils.findBand(band);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("band    = " + band);
      logger.fine("insBand = " + insBand);
    }

    // If a flux / magnitude is missing => user message
    // and it is impossible to compute any error

    final Double flux = target.getFlux(insBand);

    final double objectMag;
    final double minMag;
    final double maxMag;
    final boolean bright;

    if (flux == null) {
      objectMag = Double.NaN;
      minMag = DEF_MAG_MIN;
      maxMag = DEF_MAG_MAX;
      bright = true;
    } else {
      objectMag = flux.doubleValue();
      minMag = objectMag - 2d;
      maxMag = objectMag + 2d;

      bright = (objectMag <= BRIGHT_MAG_MAX);
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("objectMag = " + objectMag);
      logger.fine("minMag    = " + minMag);
      logger.fine("maxMag    = " + maxMag);
      logger.fine("bright    = " + bright);
    }

    // max base line :
    final double[] range = ConfigurationManager.computeLimitsUVCoverage(stations);
    final double maxBaseline = range[1];

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("maxBaseline = " + maxBaseline);
    }

    // --- Target information ---

    votable = votable.replaceFirst(KEY_TARGET_NAME, targetName);

    // magnitude in instrument band :
    votable = votable.replaceFirst(KEY_MAG, Double.toString(objectMag));
    votable = votable.replaceFirst(KEY_INS_BAND, insBand.value());

    votable = votable.replaceFirst(KEY_MAG_MIN, Double.toString(minMag));
    votable = votable.replaceFirst(KEY_MAG_MAX, Double.toString(maxMag));

    // convert RA/DEC (mas) up to 3 digits :
    final String[] raDec = AstroSkyCalcObservation.toString(target.getRADeg(), target.getDECDeg());

    votable = votable.replaceFirst(KEY_RA, raDec[0]);
    votable = votable.replaceFirst(KEY_DEC, raDec[1]);

    votable = votable.replaceFirst(KEY_BASE_MAX, Double.toString(maxBaseline));
    votable = votable.replaceFirst(KEY_WAVELENGTH, Double.toString(lambda));

    votable = votable.replaceFirst(KEY_BRIGHT, Boolean.toString(bright));

    return votable;
  }
}
