/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: OIFitsCreatorService.java,v 1.7 2010-06-29 14:26:35 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.6  2010/06/29 12:13:41  bourgesl
 * added some comments
 *
 * Revision 1.5  2010/06/28 15:39:35  bourgesl
 * added visibility computation in OI_VIS table (VISDATA / VISAMP / VISPHI)
 *
 * Revision 1.4  2010/06/28 14:37:38  bourgesl
 * proper OI_VIS creation with : dateObs (from first HA), time (supporting day change), MJD and station indexes
 *
 * Revision 1.3  2010/06/25 15:16:27  bourgesl
 * starting OI_VIS table generation : time / mjd / uv coords
 * changed UVTable per base line in UV Coverage Service
 *
 * Revision 1.2  2010/06/23 15:44:06  bourgesl
 * added computed OI_WAVELENGTH table in OIFits
 *
 * Revision 1.1  2010/06/23 12:56:13  bourgesl
 * added OIFits structure generation with OI_ARRAY and OI_TARGET tables
 *
 */
package fr.jmmc.aspro.service;

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Position3D;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData;
import fr.jmmc.mcs.astro.ALX;
import fr.jmmc.mcs.model.ModelManager;
import fr.jmmc.mcs.model.targetmodel.Model;
import fr.jmmc.oitools.OIFitsConstants;
import fr.jmmc.oitools.model.OIArray;
import fr.jmmc.oitools.model.OIFitsChecker;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OITarget;
import fr.jmmc.oitools.model.OIVis;
import fr.jmmc.oitools.model.OIVis2;
import fr.jmmc.oitools.model.OIWavelength;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.apache.commons.math.complex.Complex;

/**
 * This class contains the code to create OIFits structure from the current observation
 * @author bourgesl
 */
public final class OIFitsCreatorService {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.service.OIFitsCreatorService";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** target Id */
  private final static short TARGET_ID = (short) 1;
  /** enable the OIFits validation */
  private final static boolean DO_VALIDATE = true;

  /* members */
  /* input */
  /** observation settings */
  private final ObservationSetting observation;
  /** selected target */
  private final Target target;

  /* reused observability data */
  /** beam list */
  private final List<Beam> beams;
  /** base line list */
  private final List<BaseLine> baseLines;
  /** precessed target right ascension in decimal hours */
  private final double precRA;
  /** sky calc instance */
  private final AstroSkyCalc sc;

  /* reused uv coverage data */
  /** minimal wavelength */
  private final double lambdaMin;
  /** maximal wavelength */
  private final double lambdaMax;
  /** number of spectral channels */
  private final int nSpectralChannels;
  /** observable decimal hour angles */
  private final double[] obsHa;
  /** list of uv point couples corresponding to the target observability */
  private final List<UVRangeBaseLineData> targetUVObservability;

  /* output */
  /** oifits structure */
  private final OIFitsFile oiFitsFile;

  /* internal */
  /** interferometer name */
  private String arrayName = null;
  /** instrument name */
  private String instrumentName = null;
  /** beam mapping */
  private Map<Beam, Short> beamMapping = null;

  /**
   * Protected constructor
   * @param observation observation settings
   * @param target target to process
   * @param beams beam list
   * @param baseLines base line list
   * @param lambdaMin minimal wavelength (m)
   * @param lambdaMax maximal wavelength (m)
   * @param nSpectralChannels number of spectral channels
   * @param obsHa observable decimal hour angles
   * @param targetUVObservability list of UV coordinates per baseline
   * @param precRA precessed target right ascension in decimal hours
   * @param sc sky calc instance
   */
  protected OIFitsCreatorService(final ObservationSetting observation,
                                 final Target target,
                                 final List<Beam> beams,
                                 final List<BaseLine> baseLines,
                                 final double lambdaMin, final double lambdaMax,
                                 final int nSpectralChannels,
                                 final double[] obsHa,
                                 final List<UVRangeBaseLineData> targetUVObservability,
                                 final double precRA,
                                 final AstroSkyCalc sc) {
    this.observation = observation;
    this.target = target;
    this.beams = beams;
    this.baseLines = baseLines;
    this.lambdaMin = lambdaMin;
    this.lambdaMax = lambdaMax;
    this.nSpectralChannels = nSpectralChannels;
    this.obsHa = obsHa;
    this.targetUVObservability = targetUVObservability;
    this.precRA = precRA;
    this.sc = sc;

    // create a new OIFits structure :
    this.oiFitsFile = new OIFitsFile();
  }

  /**
   * Create the OIFits structure with OI_ARRAY, OI_TARGET, OI_WAVELENGTH, OI_VIS tables
   * @return OIFits structure
   */
  protected OIFitsFile createOIFits() {

    // Start the computations :
    final long start = System.nanoTime();

    this.arrayName = this.observation.getInterferometerConfiguration().getName();
    this.instrumentName = this.observation.getInstrumentConfiguration().getName();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("arrName : " + this.arrayName);
      logger.fine("insName : " + this.instrumentName);
    }

    this.beamMapping = createBeamMapping(this.beams);

    // OI_ARRAY :
    this.createOIArray();

    // OI_TARGET :
    this.createOITarget();

    // OI_WAVELENGTH :
    this.createOIWaveLength();

    // OI_VIS :
    this.createOIVis();

    // OI_VIS2 :
    this.createOIVis2();

    // TODO (VIS2, T3)

    // TODO : compute errors + noise models


    if (logger.isLoggable(Level.INFO)) {
      logger.info("createOIFits : duration = " + 1e-6d * (System.nanoTime() - start) + " ms.");
    }

    if (DO_VALIDATE) {
      final OIFitsChecker checker = new OIFitsChecker();
      this.oiFitsFile.check(checker);

      // validation results
      if (logger.isLoggable(Level.INFO)) {
        logger.info("createOIFits : validation results\n" + checker.getCheckReport());
      }
    }

    // fast interrupt :
    if (Thread.currentThread().isInterrupted()) {
      return null;
    }

    return this.oiFitsFile;
  }

  /**
   * Create the OI_ARRAY table
   *
   * Note : station indexes are given according to the beam list ordering starting from 1
   */
  protected void createOIArray() {

    final OIArray oiArray = new OIArray(this.oiFitsFile, this.beams.size());

    // Array Name :
    oiArray.setArrName(this.arrayName);

    // Position :
    oiArray.setFrame(OIFitsConstants.KEYWORD_FRAME_GEOCENTRIC);

    final InterferometerConfiguration ic = this.observation.getInterferometerConfiguration().getInterferometerConfiguration();
    if (ic != null) {
      final Position3D position = ic.getInterferometer().getPosition();

      oiArray.setArrayXYZ(new double[]{position.getPosX(), position.getPosY(), position.getPosZ()});
    }

    // Stations :
    int i = 0;
    Telescope tel;
    Station station;
    for (Beam b : this.beams) {
      station = b.getStation();

      tel = station.getTelescope();
      oiArray.getTelName()[i] = tel.getName();
      oiArray.getDiameter()[i] = (float) tel.getDiameter();

      oiArray.getStaName()[i] = station.getName();
      oiArray.getStaIndex()[i] = (short) (i + 1);

      // TODO : rotate the horizontal position to geocentric :
      final Position3D position = station.getRelativePosition();

      oiArray.getStaXYZ()[i] = new double[]{position.getPosX(), position.getPosY(), position.getPosZ()};

      i++;
    }

    this.oiFitsFile.addOiTable(oiArray);
  }

  /**
   * Create the OI_TARGET table
   *
   * Note : target index is 1
   */
  protected void createOITarget() {
    final OITarget oiTarget = new OITarget(this.oiFitsFile, 1);
    oiTarget.getTargetId()[0] = TARGET_ID;
    oiTarget.getTarget()[0] = this.target.getName();

    // Coordinates RA/DEC :
    oiTarget.getRaEp0()[0] = this.target.getRADeg();
    oiTarget.getDecEp0()[0] = this.target.getDECDeg();
    oiTarget.getEquinox()[0] = this.target.getEQUINOX();

    // Missing RA/DEC errors :
    oiTarget.getRaErr()[0] = 0d;
    oiTarget.getDecErr()[0] = 0d;

    // Radial velocity :
    if (this.target.getSYSVEL() != null) {
      // convert km/s in m/s :
      oiTarget.getSysVel()[0] = this.target.getSYSVEL().doubleValue() * 1e3;
    }
    oiTarget.getVelTyp()[0] = OIFitsConstants.UNKNOWN_VALUE;
    // Use VELTYP (mostly undefined) :
    oiTarget.getVelDef()[0] = OIFitsConstants.COLUMN_VELDEF_OPTICAL;

    // Proper motion :
    if (this.target.getPMRA() != null && this.target.getPMDEC() != null) {
      // convert mas/year in deg/year :
      oiTarget.getPmRa()[0] = this.target.getPMRA().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES;
      oiTarget.getPmDec()[0] = this.target.getPMDEC().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES;
    }

    // Missing PM RA/DEC errors :
    oiTarget.getPmRaErr()[0] = 0d;
    oiTarget.getPmDecErr()[0] = 0d;

    // Parallax :
    if (this.target.getPARALLAX() != null && this.target.getPARAERR() != null) {
      // convert mas in deg :
      oiTarget.getParallax()[0] = (float) (this.target.getPARALLAX().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES);
      oiTarget.getParaErr()[0] = (float) (this.target.getPARAERR().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES);
    }

    // Spectral type :
    oiTarget.getSpecTyp()[0] = this.target.getSPECTYP();

    this.oiFitsFile.addOiTable(oiTarget);
  }

  /**
   * Create the OI_WAVELENGTH table
   */
  protected void createOIWaveLength() {

    final OIWavelength waves = new OIWavelength(this.oiFitsFile, this.nSpectralChannels);
    waves.setInsName(this.instrumentName);

    final double step = (this.lambdaMax - this.lambdaMin) / this.nSpectralChannels;

    final float[] effWave = waves.getEffWave();
    final float[] effBand = waves.getEffBand();

    double waveLength = this.lambdaMin;
    for (int i = 0; i < this.nSpectralChannels; i++) {
      effWave[i] = (float) waveLength;
      effBand[i] = (float) step;

      waveLength += step;
    }

    this.oiFitsFile.addOiTable(waves);
  }

  /**
   * Create the OI_VIS table
   */
  protected void createOIVis() {

    // Create station - base line mapping :
    final Map<BaseLine, short[]> baseLineIndexes = createBaseLineMapping(this.beamMapping, this.baseLines);

    // Has models ?
    final List<Model> models = this.target.getModels();
    final boolean hasModels = models != null && !models.isEmpty();

    final int nBl = this.targetUVObservability.size();

    // Suppose that number of points is consistent :
    final int nPoints = this.targetUVObservability.get(0).getNPoints();

    final OIVis vis = new OIVis(this.oiFitsFile, this.instrumentName, nPoints * nBl);
    vis.setArrName(this.arrayName);

    // Compute UTC start date from first HA :
    final Calendar calObs = this.sc.toCalendar(this.sc.convertHAToJD(this.obsHa[0], this.precRA), false);

    final String dateObs = calendarToString(calObs);
    vis.setDateObs(dateObs);

    // Columns :
    final short[] targetIds = vis.getTargetId();
    final double[] times = vis.getTime();
    final double[] mjds = vis.getMjd();
    // skip int_time (unknown)

    final float[][][] visData = vis.getVisData();
    // skip visErr (unknown)

    final double[][] visAmp = vis.getVisAmp();
    // skip visAmpErr (unknown)

    final double[][] visPhi = vis.getVisPhi();
    // skip visPhiErr (unknown)

    final double[] uCoords = vis.getUCoord();
    final double[] vCoords = vis.getVCoord();

    final short[][] staIndexes = vis.getStaIndex();
    // skip flag (default to false means that values are considered as valid)

    // Get WaveLengths from related OI table :
    final float[] effWave = vis.getOiWavelength().getEffWave();
    final int nWave = effWave.length;

    // vars:
    double jd;
    double u, v;

    final double[] ufreq = new double[nWave];
    final double[] vfreq = new double[nWave];
    Complex[] visComplex;

    // iterate on HA points :
    for (int i = 0, j = 0, k = 0; i < nPoints; i++) {

      j = 0;

      // iterate on baselines :
      for (UVRangeBaseLineData uvBL : this.targetUVObservability) {

        k = i * nBl + j;

        // target id
        targetIds[k] = TARGET_ID;

        // jd from HA :
        jd = this.sc.convertHAToJD(this.obsHa[i], this.precRA);

        // UTC :
        times[k] = calendarToTime(this.sc.toCalendar(jd, false), calObs);

        // modified julian day :
        mjds[k] = AstroSkyCalc.mjd(jd);

        // UV coords (m) :
        u = uvBL.getU()[i];
        v = uvBL.getV()[i];

        uCoords[k] = u;
        vCoords[k] = v;

        // Compute complex visibility for the given models :
        if (hasModels) {

          // prepare spatial frequencies :
          for (int l = 0; l < nWave; l++) {
            ufreq[l] = u / effWave[l];
            vfreq[l] = v / effWave[l];
          }

          // compute complex visibilities :
          visComplex = ModelManager.getInstance().computeModels(ufreq, vfreq, models);

          if (visComplex == null) {
            // fast interrupt :
            return;
          }

          for (int l = 0; l < nWave; l++) {
            // complex data :
            visData[k][l][0] = (float) visComplex[l].getReal();
            visData[k][l][1] = (float) visComplex[l].getImaginary();

            // amplitude (not normalized) :
            visAmp[k][l] = visComplex[l].abs();

            // phase [-PI;PI] in degrees :
            visPhi[k][l] = Math.toDegrees(visComplex[l].getArgument());
          }
        }

        staIndexes[k] = baseLineIndexes.get(uvBL.getBaseLine());

        j++;
      }
    }

    this.oiFitsFile.addOiTable(vis);
  }

  /**
   * Create the OI_VIS table
   */
  protected void createOIVis2() {
    // Get OI_VIS table :
    final OIVis vis = this.oiFitsFile.getOiVis()[0];
    final int nRows = vis.getNbRows();
    final int nWave = vis.getNWave();

    final OIVis2 vis2 = new OIVis2(this.oiFitsFile, this.instrumentName, nRows);
    vis2.setArrName(this.arrayName);
    vis2.setDateObs(vis.getDateObs());

    // Columns :
    System.arraycopy(vis.getTargetId(), 0, vis2.getTargetId(), 0, nRows);
    System.arraycopy(vis.getTime(), 0, vis2.getTime(), 0, nRows);
    System.arraycopy(vis.getMjd(), 0, vis2.getMjd(), 0, nRows);
    System.arraycopy(vis.getIntTime(), 0, vis2.getIntTime(), 0, nRows);

    final double[][] visAmp = vis.getVisAmp();
    final double[][] vis2Data = vis2.getVis2Data();
    // skip vis2Err (unknown)

    for (int k = 0, l = 0; k < nRows; k++) {
      for (l = 0; l < nWave; l++) {
        // square visibility (not normalized) :
        vis2Data[k][l] = visAmp[k][l] * visAmp[k][l];
      }
    }

    System.arraycopy(vis.getUCoord(), 0, vis2.getUCoord(), 0, nRows);
    System.arraycopy(vis.getVCoord(), 0, vis2.getVCoord(), 0, nRows);

    System.arraycopy(vis.getStaIndex(), 0, vis2.getStaIndex(), 0, nRows);
    // skip flag (default to false means that values are considered as valid)

    this.oiFitsFile.addOiTable(vis2);
  }

  /**
   * Return the beam mapping
   * @param beams beam list
   * @return beam mapping
   */
  private static Map<Beam, Short> createBeamMapping(final List<Beam> beams) {
    // Create Beam - index mapping :
    final Map<Beam, Short> beamMapping = new HashMap<Beam, Short>();

    // Note : as Beam.hashCode is not implemented, the map acts as an IdentityMap (pointer equality)
    int i = 0;
    for (Beam b : beams) {
      beamMapping.put(b, Short.valueOf((short) (i + 1)));
      i++;
    }

    return beamMapping;
  }

  /**
   * Return the station indexes (2) per base line mapping
   * @param beamMapping beam mapping
   * @param baseLines base line list
   * @return baseline station indexes
   */
  private static Map<BaseLine, short[]> createBaseLineMapping(final Map<Beam, Short> beamMapping, final List<BaseLine> baseLines) {

    // Create BaseLine - indexes mapping :
    final Map<BaseLine, short[]> baseLineIndexes = new HashMap<BaseLine, short[]>();

    for (BaseLine bl : baseLines) {
      baseLineIndexes.put(bl, new short[]{
                beamMapping.get(bl.getBeam1()).shortValue(),
                beamMapping.get(bl.getBeam2()).shortValue()
              });
    }

    return baseLineIndexes;
  }

  /**
   * Convert UTC time in seconds
   * @param cal UTC time
   * @param calObs UTC start date of observation
   * @return UTC time in seconds
   */
  private static double calendarToTime(final Calendar cal, final Calendar calObs) {
    final double time = 3600d * cal.get(Calendar.HOUR_OF_DAY)
            + 60d * cal.get(Calendar.MINUTE)
            + cal.get(Calendar.SECOND);

    if (cal.get(Calendar.DAY_OF_MONTH) != calObs.get(Calendar.DAY_OF_MONTH)) {
      // observation is over 2 UTC days = observation starts before midnight and ends after :
      return 86400d + time;
    }
    return time;
  }

  /**
   * Convert UTC date to string
   * @param cal UTC date
   * @return string representation
   */
  private static String calendarToString(final Calendar cal) {
    final StringBuilder sb = new StringBuilder();
    sb.append(cal.get(Calendar.YEAR)).append('-');
    sb.append(cal.get(Calendar.MONTH) + 1).append('-');
    sb.append(cal.get(Calendar.DAY_OF_MONTH));
    return sb.toString();
  }
}
