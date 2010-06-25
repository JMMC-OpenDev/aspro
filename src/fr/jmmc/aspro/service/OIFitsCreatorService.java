/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: OIFitsCreatorService.java,v 1.3 2010-06-25 15:16:27 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/06/23 15:44:06  bourgesl
 * added computed OI_WAVELENGTH table in OIFits
 *
 * Revision 1.1  2010/06/23 12:56:13  bourgesl
 * added OIFits structure generation with OI_ARRAY and OI_TARGET tables
 *
 */
package fr.jmmc.aspro.service;

import edu.dartmouth.AstroSkyCalc;
import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Position3D;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData;
import fr.jmmc.mcs.astro.ALX;
import fr.jmmc.oitools.OIFitsConstants;
import fr.jmmc.oitools.model.OIArray;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OITarget;
import fr.jmmc.oitools.model.OIVis;
import fr.jmmc.oitools.model.OIWavelength;
import java.util.Calendar;
import java.util.List;

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
   * and return the array of wave lengths (min to max)
   * @param oiFitsFile OIFits structure
   * @param instrumentName instrument name
   * @param lambdaMin minimal wavelength (m)
   * @param lambdaMax maximal wavelength (m)
   * @param nSpectralChannels number of spectral channels
   * @return array of wave lengths (min to max)
   */
  protected static float[] createOIWaveLength(final OIFitsFile oiFitsFile, final String instrumentName,
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

    return effWave;
  }

  /**
   * Create the OI_VIS table for the given target and add it to the given oiFits structure
   * @param oiFitsFile OIFits structure
   * @param arrayName interferometer name
   * @param instrumentName instrument name
   * @param dateObs observation date
   * @param beams beam list
   * @param targetUVObservability list of HA/UV coordinates per baseline
   * @param effWave array of wave lengths (min to max)
   * @param precRA precessed target right ascension in decimal hours
   * @param sc sky calc instance
   */
  protected static void createOIVis(final OIFitsFile oiFitsFile,
                                    final String arrayName,
                                    final String instrumentName,
                                    final String dateObs,
                                    final List<Beam> beams,
                                    final List<UVRangeBaseLineData> targetUVObservability,
                                    final float[] effWave,
                                    final double precRA,
                                    final AstroSkyCalc sc) {

    final int nBl = targetUVObservability.size();

    // Suppose that number of points is consistent :
    final int nRows = targetUVObservability.get(0).getNPoints();

    final OIVis vis = new OIVis(oiFitsFile, instrumentName, nRows * nBl);
    vis.setArrName(arrayName);
    vis.setDateObs(dateObs);

    // Columns :
    final short[] targetIds = vis.getTargetId();
    final double[] times = vis.getTime();
    final double[] mjds = vis.getMjd();
    // skip int_time (unknown)

    // TODO : vis columns

    final double[] uCoords = vis.getUCoord();
    final double[] vCoords = vis.getVCoord();

    final short[][] staIndexes = vis.getStaIndex();
    // skip flag (default to false means values are valid)

    // vars:
    double ha, jd;

    for (int i = 0, j = 0, k = 0; i < nRows; i++) {
      targetIds[i] = TARGET_ID;

      j = 0;
      for (UVRangeBaseLineData uvBL : targetUVObservability) {

        logger.severe("i = " + i + ", j = " + j + ", k = " + k);

        k = i * nBl + j;

        ha = uvBL.getHA()[i];
        jd = sc.convertHAToJD(ha, precRA);

        // UTC :
        // todo manage date change : (day + 1)
        times[k] = calendarToTime(sc.toCalendar(jd, false));

        // TODO : convert jd to mjd :
        mjds[k] = jd;

        uCoords[k] = uvBL.getU()[i];
        vCoords[k] = uvBL.getV()[i];

        j++;
      }

//      staIndexes[i][0] = sta1Id;
//      staIndexes[i][1] = sta2Id;
    }


    // TODO : compute visiblities

    oiFitsFile.addOiTable(vis);
  }

  /**
   * Convert UTC time in seconds
   * @param cal UTC time
   * @return UTC time in seconds
   */
  private static double calendarToTime(final Calendar cal) {
    return 3600d * cal.get(Calendar.HOUR_OF_DAY)
            + 60d * cal.get(Calendar.MINUTE)
            + cal.get(Calendar.SECOND);
  }
}
