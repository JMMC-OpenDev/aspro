/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: OIFitsCreatorService.java,v 1.6 2010-06-29 12:13:41 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OITarget;
import fr.jmmc.oitools.model.OIVis;
import fr.jmmc.oitools.model.OIWavelength;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.apache.commons.math.complex.Complex;

/**
 * This stateless class contains the code to create OIFits structure from the current observation
 * @author bourgesl
 */
public class OIFitsCreatorService {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.service.OIFitsCreatorService";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** target Id */
  private final static short TARGET_ID = (short) 1;

  /**
   * Forbidden constructor
   */
  private OIFitsCreatorService() {
    // no-op
  }

  /**
   * Create the OI_ARRAY table for the given observation and beams and add it to the given oiFits structure
   *
   * Note : station indexes are given according to the beam list ordering starting from 1
   *
   * @param oiFitsFile OIFits structure
   * @param observation observation settings
   * @param arrayName interferometer name
   * @param beams beam list
   */
  protected static void createOIArray(final OIFitsFile oiFitsFile, final ObservationSetting observation,
                                      final String arrayName, final List<Beam> beams) {

    final OIArray oiArray = new OIArray(oiFitsFile, beams.size());

    // Array Name :
    oiArray.setArrName(arrayName);

    // Position :
    oiArray.setFrame(OIFitsConstants.KEYWORD_FRAME_GEOCENTRIC);

    final InterferometerConfiguration ic = observation.getInterferometerConfiguration().getInterferometerConfiguration();
    if (ic != null) {
      final Position3D position = ic.getInterferometer().getPosition();

      oiArray.setArrayXYZ(new double[]{position.getPosX(), position.getPosY(), position.getPosZ()});
    }

    // Stations :
    int i = 0;
    Telescope tel;
    Station station;
    for (Beam b : beams) {
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

    oiFitsFile.addOiTable(oiArray);
  }

  /**
   * Create the OI_TARGET table for the given target and add it to the given oiFits structure
   *
   * Note : target index is 1
   *
   * @param oiFitsFile OIFits structure
   * @param target target to use
   */
  protected static void createOITarget(final OIFitsFile oiFitsFile, final Target target) {
    final OITarget oiTarget = new OITarget(oiFitsFile, 1);
    oiTarget.getTargetId()[0] = TARGET_ID;
    oiTarget.getTarget()[0] = target.getName();

    // Coordinates RA/DEC :
    oiTarget.getRaEp0()[0] = target.getRADeg();
    oiTarget.getDecEp0()[0] = target.getDECDeg();
    oiTarget.getEquinox()[0] = target.getEQUINOX();

    // Missing RA/DEC errors :
    oiTarget.getRaErr()[0] = 0d;
    oiTarget.getDecErr()[0] = 0d;

    // Radial velocity :
    if (target.getSYSVEL() != null) {
      // convert km/s in m/s :
      oiTarget.getSysVel()[0] = target.getSYSVEL().doubleValue() * 1e3;
    }
    oiTarget.getVelTyp()[0] = OIFitsConstants.UNKNOWN_VALUE;
    // Use VELTYP (mostly undefined) :
    oiTarget.getVelDef()[0] = OIFitsConstants.COLUMN_VELDEF_OPTICAL;

    // Proper motion :
    if (target.getPMRA() != null && target.getPMDEC() != null) {
      // convert mas/year in deg/year :
      oiTarget.getPmRa()[0] = target.getPMRA().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES;
      oiTarget.getPmDec()[0] = target.getPMDEC().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES;
    }

    // Missing PM RA/DEC errors :
    oiTarget.getPmRaErr()[0] = 0d;
    oiTarget.getPmDecErr()[0] = 0d;

    // Parallax :
    if (target.getPARALLAX() != null && target.getPARAERR() != null) {
      // convert mas in deg :
      oiTarget.getParallax()[0] = (float) (target.getPARALLAX().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES);
      oiTarget.getParaErr()[0] = (float) (target.getPARAERR().doubleValue() * ALX.MILLI_ARCSEC_IN_DEGREES);
    }

    // Spectral type :
    oiTarget.getSpecTyp()[0] = target.getSPECTYP();

    oiFitsFile.addOiTable(oiTarget);
  }

  /**
   * Create the OI_WAVELENGTH table for the given observation and add it to the given oiFits structure
   * @param oiFitsFile OIFits structure
   * @param instrumentName instrument name
   * @param lambdaMin minimal wavelength (m)
   * @param lambdaMax maximal wavelength (m)
   * @param nSpectralChannels number of spectral channels
   */
  protected static void createOIWaveLength(final OIFitsFile oiFitsFile, final String instrumentName,
                                           final double lambdaMin, final double lambdaMax, final int nSpectralChannels) {

    final OIWavelength waves = new OIWavelength(oiFitsFile, nSpectralChannels);
    waves.setInsName(instrumentName);

    final double step = (lambdaMax - lambdaMin) / nSpectralChannels;

    final float[] effWave = waves.getEffWave();
    final float[] effBand = waves.getEffBand();

    double waveLength = lambdaMin;
    for (int i = 0; i < nSpectralChannels; i++) {
      effWave[i] = (float) waveLength;
      effBand[i] = (float) step;

      waveLength += step;
    }

    oiFitsFile.addOiTable(waves);
  }

  /**
   * Create the OI_VIS table for the given target and add it to the given oiFits structure
   * @param oiFitsFile OIFits structure
   * @param arrayName interferometer name
   * @param instrumentName instrument name
   * @param beams beam list
   * @param baseLines base line list
   * @param obsHa observable decimal hour angles
   * @param targetUVObservability list of UV coordinates per baseline
   * @param models list of models to compute complex visibilities
   * @param precRA precessed target right ascension in decimal hours
   * @param sc sky calc instance
   */
  protected static void createOIVis(final OIFitsFile oiFitsFile,
                                    final String arrayName,
                                    final String instrumentName,
                                    final double[] obsHa,
                                    final List<Beam> beams,
                                    final List<BaseLine> baseLines,
                                    final List<UVRangeBaseLineData> targetUVObservability,
                                    final List<Model> models,
                                    final double precRA,
                                    final AstroSkyCalc sc) {

    // Create station - base line mapping :
    final Map<BaseLine, short[]> baseLineIndexes = createBaseLineIndexes(beams, baseLines);

    // Has models ?
    final boolean hasModels = models != null && !models.isEmpty();

    final int nBl = targetUVObservability.size();

    // Suppose that number of points is consistent :
    final int nRows = targetUVObservability.get(0).getNPoints();

    final OIVis vis = new OIVis(oiFitsFile, instrumentName, nRows * nBl);
    vis.setArrName(arrayName);

    // Compute UTC start date from first HA :
    final Calendar calObs = sc.toCalendar(sc.convertHAToJD(obsHa[0], precRA), false);

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
    for (int i = 0, j = 0, k = 0; i < nRows; i++) {

      j = 0;

      // iterate on baselines :
      for (UVRangeBaseLineData uvBL : targetUVObservability) {

        k = i * nBl + j;

        // target id
        targetIds[k] = TARGET_ID;

        // jd from HA :
        jd = sc.convertHAToJD(obsHa[i], precRA);

        // UTC :
        times[k] = calendarToTime(sc.toCalendar(jd, false), calObs);

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

    oiFitsFile.addOiTable(vis);
  }

  /**
   * Return the station indexes (2) per base line mapping
   * @param beams beam list
   * @param baseLines base line list
   * @return baseline station indexes
   */
  private static Map<BaseLine, short[]> createBaseLineIndexes(final List<Beam> beams, final List<BaseLine> baseLines) {
    // Create Beam - index mapping :
    final Map<Beam, Short> beamIndexes = new HashMap<Beam, Short>();

    // Note : as Beam.hashCode is not implemented, the map acts as an IdentityMap (pointer equality)
    int i = 0;
    for (Beam b : beams) {
      beamIndexes.put(b, Short.valueOf((short) (i + 1)));
      i++;
    }

    // Create BaseLine - indexes mapping :
    final Map<BaseLine, short[]> baseLineIndexes = new HashMap<BaseLine, short[]>();

    for (BaseLine bl : baseLines) {
      baseLineIndexes.put(bl, new short[]{
                beamIndexes.get(bl.getBeam1()).shortValue(),
                beamIndexes.get(bl.getBeam2()).shortValue()
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
