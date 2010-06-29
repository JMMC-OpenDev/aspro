/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: UVCoverageService.java,v 1.26 2010-06-29 14:26:35 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.25  2010/06/29 12:14:13  bourgesl
 * minor clean up
 *
 * Revision 1.24  2010/06/28 15:39:35  bourgesl
 * added visibility computation in OI_VIS table (VISDATA / VISAMP / VISPHI)
 *
 * Revision 1.23  2010/06/28 14:36:08  bourgesl
 * refactoring of computeObservableUV() with 2 steps : first find observable HA values then find UV coordinates
 *
 * Revision 1.22  2010/06/25 15:16:27  bourgesl
 * starting OI_VIS table generation : time / mjd / uv coords
 * changed UVTable per base line in UV Coverage Service
 *
 * Revision 1.21  2010/06/23 15:44:06  bourgesl
 * added computed OI_WAVELENGTH table in OIFits
 *
 * Revision 1.20  2010/06/23 12:56:13  bourgesl
 * added OIFits structure generation with OI_ARRAY and OI_TARGET tables
 *
 * Revision 1.19  2010/06/17 10:02:50  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.18  2010/06/10 08:54:59  bourgesl
 * only log the compute duration if the operation completed normally
 *
 * Revision 1.17  2010/05/06 15:42:18  bourgesl
 * use HA Min/Max + FT Mode for the target in the observation settings
 *
 * Revision 1.16  2010/05/05 14:34:00  bourgesl
 * javadoc / comments
 *
 * Revision 1.15  2010/02/19 16:06:07  bourgesl
 * added image size & LUT combo boxes
 *
 * Revision 1.14  2010/02/18 15:52:38  bourgesl
 * added parameter argument validation with an user message
 *
 * Revision 1.13  2010/02/09 16:51:09  bourgesl
 * added change listener for image modes
 *
 * Revision 1.12  2010/02/08 17:00:16  bourgesl
 * added U-V max selector + checkboxes
 *
 * Revision 1.11  2010/02/05 09:46:00  bourgesl
 * no more margin on max UV coordinates
 *
 * Revision 1.10  2010/02/04 17:05:06  bourgesl
 * UV bounds are coming from UVCoverageService
 *
 * Revision 1.9  2010/02/04 14:54:11  bourgesl
 * UVMapData refactoring (uvRect, min/max values) to keep the color mapping consistent when zooming
 * Compute an sub Image when a zoom occurs while the correct model is computed in the background
 *
 * Revision 1.8  2010/02/03 09:48:53  bourgesl
 * target model uvmap added on the uv coverage with zooming supported
 *
 * Revision 1.7  2010/01/29 16:01:20  bourgesl
 * added comments + uv max
 *
 * Revision 1.6  2010/01/21 16:41:30  bourgesl
 * added HA min / max sliders and used only to constraint the UV tracks
 *
 * Revision 1.5  2010/01/19 13:20:20  bourgesl
 * NPE fixed when the observability displays the baseline limits
 *
 * Revision 1.4  2010/01/15 16:14:16  bourgesl
 * added computation of UV points compatible with observability ranges, bandpass and sampling periodicity
 *
 * Revision 1.3  2010/01/15 13:51:27  bourgesl
 * illegalStateException if any transient field is undefined
 *
 * Revision 1.2  2010/01/12 17:10:08  bourgesl
 * less log INFO outputs
 *
 * Revision 1.1  2010/01/08 16:50:53  bourgesl
 * initial uv coverage
 *
 */
package fr.jmmc.aspro.service;

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.uvcoverage.UVCoverageData;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.uvcoverage.UVBaseLineData;
import fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData;
import fr.jmmc.aspro.util.AngleUtils;
import fr.jmmc.mcs.model.ModelUVMapService;
import fr.jmmc.mcs.model.ModelUVMapService.ImageMode;
import java.awt.geom.Rectangle2D;
import java.awt.image.IndexColorModel;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

/**
 * This service is dedicated to compute the UV tracks for a given target
 * @author bourgesl
 */
public final class UVCoverageService {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.service.UVCoverageService";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* members */

  /* output */
  /** uv coverage data */
  private UVCoverageData data = new UVCoverageData();

  /* inputs */
  /** observation settings */
  private final ObservationSetting observation;
  /** target to use */
  private final String targetName;
  /** flag to compute the UV support */
  private final boolean doUVSupport;
  /** flag to compute the model image */
  private final boolean doModelImage;
  /** image mode (amplitude or phase) */
  private ImageMode imageMode;
  /** image size */
  private int imageSize;
  /** image color model */
  private IndexColorModel colorModel;

  /* internal */
  /** Get the current thread to check if the computation is interrupted */
  private final Thread currentThread = Thread.currentThread();
  /** hour angle step (see samplingPeriod) */
  private double haStep;
  /** minimal wavelength */
  private double lambdaMin;
  /** central wavelength */
  private double lambda;
  /** maximal wavelength */
  private double lambdaMax;
  /** number of spectral channels (used by OIFits) */
  private int nSpectralChannels;
  /** maximum U or V coordinate (corrected by the minimal wavelength) */
  private double uvMax;
  /** HA min in decimal hours */
  private double haMin = AsproConstants.HA_MIN;
  /** HA max in decimal hours */
  private double haMax = AsproConstants.HA_MAX;

  /* reused observability data */
  /** observability data */
  private ObservabilityData obsData = null;
  /** sky calc instance */
  private AstroSkyCalc sc = null;
  /** beam list */
  private List<Beam> beams = null;
  /** base line list */
  private List<BaseLine> baseLines = null;
  /** star data */
  private StarData starData = null;

  /**
   * Constructor.
   * Note : This service is statefull so it can not be reused by several calls.
   *
   * @param observation observation settings
   * @param targetName target name
   * @param uvMax U-V max in meter
   * @param doUVSupport flag to compute the UV support
   * @param doModelImage flag to compute the model image
   * @param imageMode image mode (amplitude or phase)
   * @param imageSize number of pixels for both width and height of the generated image
   * @param colorModel color model to use
   */
  public UVCoverageService(final ObservationSetting observation, final String targetName,
                           final double uvMax, final boolean doUVSupport,
                           final boolean doModelImage, final ImageMode imageMode, final int imageSize, final IndexColorModel colorModel) {
    this.observation = observation;
    this.targetName = targetName;
    this.uvMax = uvMax;
    this.doUVSupport = doUVSupport;
    this.doModelImage = doModelImage;
    this.imageMode = imageMode;
    this.imageSize = imageSize;
    this.colorModel = colorModel;
  }

  /**
   * Main operation to compute the UV tracks for a given target
   *
   * @return UVCoverageData container
   */
  public UVCoverageData compute() {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("compute : " + this.observation);
    }

    // Start the computations :
    final long start = System.nanoTime();

    try {
      // check if observability data are available :
      this.obsData = this.observation.getObservabilityData();

      if (this.obsData == null) {
        // invalid data because the observability data are not available :
        this.data = null;
      } else {
        // Get instrument and observability data :
        prepareObservation();

        if (this.starData != null) {
          // Note : for Baseline limits, the starData is null
          // (target observability is not available) :

          // target name :
          this.data.setName(this.targetName);

          // wave length :
          this.data.setLambda(this.lambda);

          // Is the target visible :
          if (this.starData.getHaElev() > 0d) {

            if (this.doUVSupport) {
              computeUVSupport();
            }

            computeObservableUV();

            // OIFits structure :
            createOIFits();
          }

          // fast interrupt :
          if (this.currentThread.isInterrupted()) {
            return null;
          }
          if (logger.isLoggable(Level.FINE)) {
            logger.fine("UV coordinate maximum = [" + this.uvMax + "]");
          }

          // uv Max = max base line * uv margin / minimum wave length
          this.data.setUvMax(this.uvMax);

          if (this.doModelImage) {
            final Rectangle2D.Float uvRect = new Rectangle2D.Float();
            uvRect.setFrameFromDiagonal(-uvMax, -uvMax, uvMax, uvMax);

            // Compute Target Model for the UV coverage limits :
            this.data.setUvMapData(ModelUVMapService.computeUVMap(
                    ObservationManager.getTarget(this.observation, this.targetName).getModels(),
                    uvRect, null, null,
                    this.imageMode, this.imageSize, this.colorModel));
          }

          // fast interrupt :
          if (this.currentThread.isInterrupted()) {
            return null;
          }

        } // starData is defined

      } // obsData is valid

      // fast interrupt :
      if (this.currentThread.isInterrupted()) {
        return null;
      }

      if (logger.isLoggable(Level.INFO)) {
        logger.info("compute : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
      }

    } catch (IllegalArgumentException iae) {
      // special case : the model can throw such exception to indicate that a parameter has an invalid value :
      throw iae;
    } catch (IllegalStateException ise) {
      if (logger.isLoggable(Level.WARNING)) {
        logger.log(Level.WARNING, "invalid observation :", ise);
        logger.log(Level.WARNING, "observation : " + ObservationManager.toString(this.observation));
      }
      // clear invalid data :
      this.data = null;
    } catch (RuntimeException re) {
      if (logger.isLoggable(Level.SEVERE)) {
        logger.log(Level.SEVERE, "compute failure :", re);
        logger.log(Level.SEVERE, "observation : " + ObservationManager.toString(this.observation));
      }
      // clear invalid data :
      this.data = null;
    }

    return this.data;
  }

  /**
   * Compute UV tracks using only rise/set intervals
   */
  private void computeUVSupport() {

    final double haElev = this.starData.getHaElev();

    // 10 minutes is enough to get pretty ellipse :
    final double step = 10d / 60d;

    final int nPoints = (int) Math.round(2d * haElev / step) + 1;

    // precessed target declination in rad :
    final double precDEC = Math.toRadians(this.starData.getPrecDEC());

    final int sizeBL = this.baseLines.size();
    final List<UVBaseLineData> targetUVRiseSet = new ArrayList<UVBaseLineData>(sizeBL);

    UVBaseLineData uvData;
    BaseLine baseLine;
    /* U,V coordinates corrected with central wavelength */
    double[] u;
    double[] v;

    double haRad;

    for (int i = 0, j = 0; i < sizeBL; i++) {
      baseLine = this.baseLines.get(i);

      uvData = new UVBaseLineData(baseLine.getName());

      u = new double[nPoints];
      v = new double[nPoints];

      j = 0;

      for (double ha = -haElev; ha <= haElev; ha += step) {

        haRad = AngleUtils.hours2rad(ha);

        // Baseline projected vector (m) :
        u[j] = CalcUVW.computeU(baseLine, haRad);
        v[j] = CalcUVW.computeV(precDEC, baseLine, haRad);

        // wavelength correction :

        // Spatial frequency (rad-1) :
        u[j] /= this.lambda;
        v[j] /= this.lambda;

        j++;
      }

      uvData.setNPoints(j);
      uvData.setU(u);
      uvData.setV(v);

      targetUVRiseSet.add(uvData);

      // fast interrupt :
      if (this.currentThread.isInterrupted()) {
        return;
      }

    }

    this.data.setTargetUVRiseSet(targetUVRiseSet);
  }

  /**
   * Compute UV points (observable) inside HA min/max ranges
   */
  private void computeObservableUV() {

    final List<Range> obsRangesHA = this.starData.getObsRangesHA();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("obsRangesHA = " + obsRangesHA);
    }

    if (obsRangesHA != null) {

      final double haLower = this.haMin;
      final double haUpper = this.haMax;

      final double step = this.haStep;

      // estimate the number of HA points :
      final int capacity = (int) Math.round((haUpper - haLower) / step) + 1;

      // First pass : find observable HA values :
      final double[] haValues = new double[capacity];

      int j = 0;
      for (double ha = haLower; ha <= haUpper; ha += step) {

        // check HA :
        if (checkObservability(ha, obsRangesHA)) {
          haValues[j] = ha;
          j++;
        }
      }

      // correct number of HA points :
      final int nPoints = j;

      // check if there is at least one observable HA :
      if (nPoints == 0) {
        return;
      }

      final double[] HA = new double[nPoints];
      System.arraycopy(haValues, 0, HA, 0, nPoints);

      this.data.setHA(HA);

      // Second pass : extract UV values for HA points :

      // precessed target declination in rad :
      final double precDEC = Math.toRadians(this.starData.getPrecDEC());

      final int sizeBL = this.baseLines.size();
      final List<UVRangeBaseLineData> targetUVObservability = new ArrayList<UVRangeBaseLineData>(sizeBL);

      UVRangeBaseLineData uvData;
      BaseLine baseLine;

      /* pure U,V coordinates (m) */
      double[] u;
      double[] v;
      /* U,V coordinates corrected with minimal wavelength */
      double[] uWMin;
      double[] vWMin;
      /* U,V coordinates corrected with maximal wavelength */
      double[] uWMax;
      double[] vWMax;

      double haRad;

      for (int i = 0; i < sizeBL; i++) {
        baseLine = this.baseLines.get(i);

        uvData = new UVRangeBaseLineData(baseLine);

        u = new double[nPoints];
        v = new double[nPoints];
        uWMin = new double[nPoints];
        vWMin = new double[nPoints];
        uWMax = new double[nPoints];
        vWMax = new double[nPoints];

        for (j = 0; j < nPoints; j++) {
          haRad = AngleUtils.hours2rad(HA[j]);

          // Baseline projected vector (m) :
          u[j] = CalcUVW.computeU(baseLine, haRad);
          v[j] = CalcUVW.computeV(precDEC, baseLine, haRad);

          // wavelength correction :

          // Spatial frequency (rad-1) :
          uWMin[j] = u[j] / this.lambdaMin;
          vWMin[j] = v[j] / this.lambdaMin;

          uWMax[j] = u[j] / this.lambdaMax;
          vWMax[j] = v[j] / this.lambdaMax;
        }

        uvData.setNPoints(nPoints);
        uvData.setU(u);
        uvData.setV(v);
        uvData.setUWMin(uWMin);
        uvData.setVWMin(vWMin);
        uvData.setUWMax(uWMax);
        uvData.setVWMax(vWMax);

        targetUVObservability.add(uvData);

        // fast interrupt :
        if (this.currentThread.isInterrupted()) {
          return;
        }
      }

      this.data.setTargetUVObservability(targetUVObservability);
    }
  }

  /**
   * Check if the given hour angle is observable
   * @param ha decimal hour angle
   * @param obsRangesHA observable ranges
   * @return true if observable
   */
  private boolean checkObservability(final double ha, final List<Range> obsRangesHA) {
    for (Range range : obsRangesHA) {
      if (ha >= range.getMin() && ha <= range.getMax()) {
        return true;
      }
    }
    return false;
  }

  /**
   * Define the baselines, star data and instrument mode's wavelengths
   * @throws IllegalStateException if the instrument mode is undefined
   */
  private void prepareObservation() throws IllegalStateException {
    // Get AstroSkyCalc instance :
    this.sc = this.obsData.getDateCalc();
    // Get beams :
    this.beams = this.obsData.getBeams();
    // Get baselines :
    this.baseLines = this.obsData.getBaseLines();

    // Get starData for the selected target name :
    this.starData = this.obsData.getStarData(this.targetName);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("starData : " + this.starData);
    }

    final FocalInstrumentMode insMode = this.observation.getInstrumentConfiguration().getFocalInstrumentMode();
    if (insMode == null) {
      throw new IllegalStateException("prepareObservation : the instrumentMode is null !");
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("instrumentMode : " + insMode.getName());
    }

    this.lambdaMin = insMode.getWaveLengthMin() * AsproConstants.MICRO_METER;
    this.lambdaMax = insMode.getWaveLengthMax() * AsproConstants.MICRO_METER;
    this.lambda = insMode.getWaveLength() * AsproConstants.MICRO_METER;
    this.nSpectralChannels = insMode.getEffectiveNumberOfChannels();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("lambdaMin : " + this.lambdaMin);
      logger.fine("lambda    : " + this.lambda);
      logger.fine("lambdaMax : " + this.lambdaMax);
      logger.fine("nChannels : " + this.nSpectralChannels);
    }

    // HA Min / Max :
    final TargetConfiguration targetConf = ObservationManager.getTargetConfiguration(this.observation, targetName);
    if (targetConf != null) {
      if (targetConf.getHAMin() != null) {
        this.haMin = targetConf.getHAMin().doubleValue();
      }
      if (targetConf.getHAMax() != null) {
        this.haMax = targetConf.getHAMax().doubleValue();
      }
    }
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("ha min    : " + this.haMin);
      logger.fine("ha max    : " + this.haMax);
    }

    // hour angle step in decimal hours :
    this.haStep = this.observation.getInstrumentConfiguration().getSamplingPeriod() / 60d;

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("ha step   : " + this.haStep);
    }

    // Adjust the user uv Max = max base line / minimum wave length
    // note : use the minimum wave length to make all uv segment visible :

    this.uvMax = Math.round(this.uvMax / this.lambdaMin);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("uvMax : " + this.uvMax);
    }
  }

  /**
   * Create the OIFits structure (array, target, wave lengths and visibilities)
   */
  private void createOIFits() {
    final List<UVRangeBaseLineData> targetUVObservability = this.data.getTargetUVObservability();

    if (targetUVObservability != null) {

      // get current target :
      final Target target = ObservationManager.getTarget(this.observation, this.targetName);

      // Create the OIFitsCreatorService :
      final OIFitsCreatorService oiFitsCreator =
              new OIFitsCreatorService(this.observation, target,
              this.beams, this.baseLines, this.lambdaMin, this.lambdaMax, this.nSpectralChannels,
              this.data.getHA(), targetUVObservability, this.starData.getPrecRA(), this.sc);

      // Create the OIFits structure :
      this.data.setOiFitsFile(oiFitsCreator.createOIFits());
    }
  }
}
