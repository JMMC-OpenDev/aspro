/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.interop;

import edu.dartmouth.AstroSkyCalcObservation;
import fr.jmmc.aspro.AsproGui;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.ObservationCollection;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.util.SpectralBandUtils;
import fr.jmmc.jmal.Band;
import fr.jmmc.jmcs.gui.MessagePane;
import fr.jmmc.jmcs.network.interop.SampCapability;
import fr.jmmc.jmcs.network.interop.SampCapabilityAction;
import fr.jmmc.jmcs.util.FileUtils;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This action asks SearchCal to search calibrators for the current selected target
 *
 * @author bourgesl
 */
public final class SearchCalQueryAction extends SampCapabilityAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  private final static String className = SearchCalQueryAction.class.getName();
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "searchCalStartQuery";
  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(className);
  /** template name */
  private final static String TEMPLATE_FILE = "fr/jmmc/aspro/interop/SearchCal_template.xml";

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
  /** default minimal magnitude if magnitude is undefined */
  private final static double DEF_MAG_MIN = 2d;
  /** default maximal magnitude if magnitude is undefined */
  private final static double DEF_MAG_MAX = 4d;

  /* members */
  /**
   * Public constructor that automatically register the action in RegisteredAction.
   */
  public SearchCalQueryAction() {
    super(className, actionName, SampCapability.SEARCHCAL_START_QUERY);
  }

  /**
   * Create the Samp message parameters with the SearchCal votable
   * @return Samp message parameters as a map
   */
  @Override
  public Map<?, ?> composeMessage() {

    // extract the selected target in the main form :
    final Target target = AsproGui.getInstance().getSettingPanel().getObservationForm().getSelectedTarget();

    if (target == null) {
      MessagePane.showMessage("Please select a target before calling SearchCal !");
      return null;
    }
    if (ObservationManager.getInstance().isCalibrator(target)) {
      MessagePane.showMessage("Please select a science target (not a calibrator target) !");
      return null;
    }

      logger.debug("composeMessage for target: {}", target);

    final String votable = processTarget(target);

      logger.debug("votable = \n{}", votable);

    final Map<String, String> parameters = new HashMap<String, String>(2);
    parameters.put("query", votable);

    return parameters;
  }

  /**
   * Load and fill the searchCal votable
   * @param target target to use
   * @return SearchCal votable as string
   */
  private String processTarget(final Target target) {
    // get OB template :
    String votable = FileUtils.readFile(TEMPLATE_FILE);

    // use observation collection :
    final ObservationCollection obsCollection = ObservationManager.getInstance().getObservationCollection();

    // Get chosen stations :
    final List<Station> stations;

    if (obsCollection.isSingle()) {
      stations = obsCollection.getFirstObservation().getInstrumentConfiguration().getStationList();
    } else {
      // merge station lists to have the largest baseline ...
      stations = new ArrayList<Station>(4);

      for (ObservationSetting observation : obsCollection.getObservations()) {
        for (Station station : observation.getInstrumentConfiguration().getStationList()) {
          if (!stations.contains(station)) {
            stations.add(station);
          }
        }
      }
    }

    if (stations == null || stations.isEmpty()) {
      throw new IllegalStateException("The station list is empty !");
    }

    final FocalInstrumentMode insMode = obsCollection.getFirstObservation().getInstrumentConfiguration().getFocalInstrumentMode();
    if (insMode == null) {
      throw new IllegalStateException("The instrumentMode is empty !");
    }

    final double lambda = insMode.getWaveLength();

    if (logger.isDebugEnabled()) {
      logger.debug("lambda: {}", lambda);
    }

    final Band band = Band.findBand(lambda);
    final SpectralBand insBand = SpectralBandUtils.findBand(band);

    if (logger.isDebugEnabled()) {
      logger.debug("band: {}", band);
      logger.debug("insBand: {}", insBand);
    }

    // If a flux / magnitude is missing => user message
    // and it is impossible to compute any error

    final Double flux = target.getFlux(insBand);

    final double objectMag;

    // TODO: remove soon following parameters:
    final boolean bright = true;
    final double minMag;
    final double maxMag;

    if (flux == null) {
      objectMag = Double.NaN;
      minMag = DEF_MAG_MIN;
      maxMag = DEF_MAG_MAX;
    } else {
      objectMag = flux.doubleValue();
      minMag = objectMag - 2d;
      maxMag = objectMag + 2d;
    }

    if (logger.isDebugEnabled()) {
      logger.debug("objectMag: {}", objectMag);
      logger.debug("minMag: {}", minMag);
      logger.debug("maxMag: {}", maxMag);
      logger.debug("bright: {}", bright);
    }

    // max base line :
    final double[] range = ConfigurationManager.computeBaselineUVBounds(stations);
    final double maxBaseline = range[1];

    if (logger.isDebugEnabled()) {
      logger.debug("stations: {}", stations);
      logger.debug("maxBaseline: {}", maxBaseline);
    }

    // --- Target information ---

    votable = votable.replaceFirst(KEY_TARGET_NAME, target.getName());

    // magnitude in instrument band :
    votable = votable.replaceFirst(KEY_MAG, Double.toString(objectMag));
    votable = votable.replaceFirst(KEY_INS_BAND, insBand.value());

    // convert RA/DEC (mas) up to 3 digits :
    final String[] raDec = AstroSkyCalcObservation.toString(target.getRADeg(), target.getDECDeg());

    votable = votable.replaceFirst(KEY_RA, raDec[0]);
    votable = votable.replaceFirst(KEY_DEC, raDec[1]);

    votable = votable.replaceFirst(KEY_BASE_MAX, Double.toString(maxBaseline));
    votable = votable.replaceFirst(KEY_WAVELENGTH, Double.toString(lambda));

    // TODO: remove soon following parameters:
    votable = votable.replaceFirst(KEY_BRIGHT, Boolean.toString(bright));
    votable = votable.replaceFirst(KEY_MAG_MIN, Double.toString(minMag));
    votable = votable.replaceFirst(KEY_MAG_MAX, Double.toString(maxMag));

    return votable;
  }
}
