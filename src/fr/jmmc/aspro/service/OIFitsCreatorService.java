/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: OIFitsCreatorService.java,v 1.1 2010-06-23 12:56:13 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.model.Beam;
import fr.jmmc.aspro.model.oi.InterferometerConfiguration;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Position3D;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.mcs.astro.ALX;
import fr.jmmc.oitools.OIFitsConstants;
import fr.jmmc.oitools.model.OIArray;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OITarget;
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
   * @param beams beam list
   */
  protected static void createOIArray(final OIFitsFile oiFitsFile, final ObservationSetting observation, final List<Beam> beams) {

    final OIArray oiArray = new OIArray(oiFitsFile, beams.size());

    // Array Name :
    oiArray.setArrName(observation.getInterferometerConfiguration().getName());

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
    oiTarget.getTargetId()[0] = (short) 1;
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
}
