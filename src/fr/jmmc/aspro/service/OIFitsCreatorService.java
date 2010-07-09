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
import fr.jmmc.aspro.util.CombUtils;
import fr.jmmc.aspro.util.ComplexUtils;
import fr.jmmc.mcs.astro.ALX;
import fr.jmmc.mcs.model.ModelManager;
import fr.jmmc.mcs.model.targetmodel.Model;
import fr.jmmc.oitools.OIFitsConstants;
import fr.jmmc.oitools.model.OIArray;
import fr.jmmc.oitools.model.OIFitsChecker;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIT3;
import fr.jmmc.oitools.model.OITarget;
import fr.jmmc.oitools.model.OIVis;
import fr.jmmc.oitools.model.OIVis2;
import fr.jmmc.oitools.model.OIWavelength;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.LinkedHashMap;
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
  private final static boolean DO_VALIDATE = false;

  /* members */
  /* input */
  /** observation settings */
  private final ObservationSetting observation;
  /** selected target */
  private final Target target;

  /* reused observability data */
  /** beam list */
  private final List<Beam> beams;
  /** number of beams */
  private final int nBeams;
  /** base line list */
  private final List<BaseLine> baseLines;
  /** number of baselines */
  private final int nBaseLines;
  /** precessed target right ascension in decimal hours */
  private final double precRA;
  /** sky calc instance */
  private final AstroSkyCalc sc;

  /* reused uv coverage data */
  /** minimal wavelength */
  private final double lambdaMin;
  /** maximal wavelength */
  private final double lambdaMax;
  /** number of wavelengths = number of spectral channels */
  private int nWaveLengths;
  /** observable decimal hour angles */
  private final double[] obsHa;
  /** number of points = number of observable hour angles */
  private final int nHAPoints;
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
  /** wavelengths */
  private double[] waveLengths;
  /** beam mapping */
  private Map<Beam, Short> beamMapping = null;
  /** baseline mapping */
  private Map<BaseLine, short[]> baseLineMapping = null;

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
    this.nBeams = this.beams.size();
    this.baseLines = baseLines;
    this.nBaseLines = this.baseLines.size();
    this.lambdaMin = lambdaMin;
    this.lambdaMax = lambdaMax;
    this.nWaveLengths = nSpectralChannels;
    this.obsHa = obsHa;
    this.nHAPoints = this.obsHa.length;
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

    // Create beams and base line mappings :
    this.beamMapping = createBeamMapping(this.beams);
    this.baseLineMapping = createBaseLineMapping(this.beamMapping, this.baseLines);

    // OI_ARRAY :
    this.createOIArray();

    // OI_TARGET :
    this.createOITarget();

    // OI_WAVELENGTH :
    this.createOIWaveLength();

    // OI_VIS :
    this.createOIVis();

    // fast interrupt :
    if (Thread.currentThread().isInterrupted()) {
      return null;
    }

    // OI_VIS2 :
    this.createOIVis2();

    // OI_VIS2 :
    if (true) {
      // Work in progress :
      this.createOIT3();
    }

    // fast interrupt :
    if (Thread.currentThread().isInterrupted()) {
      return null;
    }

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

    // Create OI_ARRAY table :
    final OIArray oiArray = new OIArray(this.oiFitsFile, this.nBeams);

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

    // Create OI_TARGET table :
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

    // Create OI_WAVELENGTH table :
    final OIWavelength waves = new OIWavelength(this.oiFitsFile, this.nWaveLengths);
    waves.setInsName(this.instrumentName);

    final double step = (this.lambdaMax - this.lambdaMin) / this.nWaveLengths;

    this.waveLengths = new double[this.nWaveLengths];

    final float[] effWave = waves.getEffWave();
    final float[] effBand = waves.getEffBand();

    double waveLength = this.lambdaMin;
    for (int i = 0; i < this.nWaveLengths; i++) {
      this.waveLengths[i] = waveLength;

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

    // Has models ?
    final List<Model> models = this.target.getModels();
    final boolean hasModels = models != null && !models.isEmpty();

    // Create OI_VIS table :
    final OIVis vis = new OIVis(this.oiFitsFile, this.instrumentName, this.nHAPoints * this.nBaseLines);
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

    // vars:
    double jd;
    double u, v;

    final double[] ufreq = new double[this.nWaveLengths];
    final double[] vfreq = new double[this.nWaveLengths];
    Complex[] visComplex;

    // Iterate on HA points :
    for (int i = 0, j = 0, k = 0, l = 0; i < this.nHAPoints; i++) {

      j = 0;

      // Iterate on baselines :
      for (UVRangeBaseLineData uvBL : this.targetUVObservability) {

        k = i * this.nBaseLines + j;

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
          for (l = 0; l < this.nWaveLengths; l++) {
            ufreq[l] = u / this.waveLengths[l];
            vfreq[l] = v / this.waveLengths[l];
          }

          // compute complex visibilities :
          visComplex = ModelManager.getInstance().computeModels(ufreq, vfreq, models);

          if (visComplex == null) {
            // fast interrupt :
            return;
          }

          // Iterate on wave lengths :
          for (l = 0; l < this.nWaveLengths; l++) {
            // complex data :
            visData[k][l][0] = (float) visComplex[l].getReal();
            visData[k][l][1] = (float) visComplex[l].getImaginary();

            // amplitude (not normalized) :
            visAmp[k][l] = visComplex[l].abs();

            // phase [-PI;PI] in degrees :
            visPhi[k][l] = Math.toDegrees(visComplex[l].getArgument());
          }
        }

        // station indexes :
        staIndexes[k] = this.baseLineMapping.get(uvBL.getBaseLine());

        j++;
      }
    }

    this.oiFitsFile.addOiTable(vis);
  }

  /**
   * Create the OI_VIS2 table
   */
  protected void createOIVis2() {
    // Get OI_VIS table :
    final OIVis vis = this.oiFitsFile.getOiVis()[0];
    final int nRows = vis.getNbRows();

    // Create OI_VIS2 table :
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
      // Iterate on wave lengths :
      for (l = 0; l < this.nWaveLengths; l++) {
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
   * Create the OI_T3 table
   */
  protected void createOIT3() {

    if (this.nBeams < 3) {
      return;
    }

    // number of triplets :
    final List<int[]> iTriplets = CombUtils.generateCombinations(this.nBeams, 3);

    final int nTriplets = iTriplets.size();
    if (nTriplets == 0) {
      return;
    }

    final List<Triplet> triplets = new ArrayList<Triplet>(nTriplets);
    for (int[] idx : iTriplets) {
      triplets.add(Triplet.create(idx, this.baseLineMapping));
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("triplets  = " + triplets);
    }

    // Get OI_VIS table :
    final OIVis vis = this.oiFitsFile.getOiVis()[0];

    // Create OI_T3 table :
    final OIT3 t3 = new OIT3(this.oiFitsFile, this.instrumentName, this.nHAPoints * nTriplets);
    t3.setArrName(this.arrayName);
    t3.setDateObs(vis.getDateObs());

    // OI_VIS Columns :
    final double[] visTimes = vis.getTime();
    final double[] visMjds = vis.getMjd();
    // skip int_time (unknown)

    final float[][][] visData = vis.getVisData();
    // skip visErr (unknown)

    final double[] visUCoords = vis.getUCoord();
    final double[] visVCoords = vis.getVCoord();

    final short[][] visStaIndexes = vis.getStaIndex();

    // OI_T3 Columns :
    final short[] t3TargetIds = t3.getTargetId();
    final double[] t3Times = t3.getTime();
    final double[] t3Mjds = t3.getMjd();
    // skip int_time (unknown)

    final double[][] t3Amp = t3.getT3Amp();
    // skip t3AmpErr (unknown)

    final double[][] t3Phi = t3.getT3Phi();
    // skip t3PhiErr (unknown)

    final double[] t3U1Coords = t3.getU1Coord();
    final double[] t3V1Coords = t3.getV1Coord();

    final double[] t3U2Coords = t3.getU2Coord();
    final double[] t3V2Coords = t3.getV2Coord();

    final short[][] t3StaIndexes = t3.getStaIndex();
    // skip flag (default to false means that values are considered as valid)


    // The following code use some hypothesis on the OI_VIS table as defined in createOIVis()

    // 1 - the number of rows per HA point corresponds to the number of baselines.
    // 2 - OI_VIS rows have the same ordering than the list of baselines per HA points.

    float[][] visData12, visData23, visData13;
    Complex vis12, vis23, vis31, t3Data;
    double u12, v12, u23, v23;

    int[] relPos;
    int pos;

    // Iterate on HA points :
    for (int i = 0, j = 0, k = 0, l = 0, vp = 0; i < this.nHAPoints; i++) {

      // position in OI_VIS HA row group :
      vp = this.nBaseLines * i;

      j = 0;

      // Iterate on baselines :
      for (Triplet triplet : triplets) {

        k = nTriplets * i + j;

        // target id
        t3TargetIds[k] = TARGET_ID;

        // UTC :
        t3Times[k] = visTimes[vp];

        // modified julian day :
        t3Mjds[k] = visMjds[vp];

        // Use relative positions to get the 3 complex vectors (AB, BC, AC)
        relPos = triplet.getRelativePosition();

        // Find baseline AB = 12 :
        pos = relPos[0];

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("vis baseline = " + Arrays.toString(visStaIndexes[vp + pos]));
          logger.fine("T3  baseline = " + Arrays.toString(triplet.getBaselineIndexes()[0]));
        }

        visData12 = visData[vp + pos];
        u12 = visUCoords[vp + pos];
        v12 = visVCoords[vp + pos];

        // Find baseline BC = 23 :
        pos = relPos[1];

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("vis baseline = " + Arrays.toString(visStaIndexes[vp + pos]));
          logger.fine("T3  baseline = " + Arrays.toString(triplet.getBaselineIndexes()[1]));
        }

        visData23 = visData[vp + pos];
        u23 = visUCoords[vp + pos];
        v23 = visVCoords[vp + pos];

        // Find baseline AC = 13 :
        pos = relPos[2];

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("vis baseline = " + Arrays.toString(visStaIndexes[vp + pos]));
          logger.fine("T3  baseline = " + Arrays.toString(triplet.getBaselineIndexes()[2]));
        }

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("UV 13    = " + visUCoords[vp + pos] + ", " + visVCoords[vp + pos]);
          logger.fine("UV 12+23 = " + (u12 + u23) + ", " + (v12 + v23));
        }

        visData13 = visData[vp + pos];

        // Iterate on wave lengths :

        for (l = 0; l < this.nWaveLengths; l++) {

          // baseline AB = 12 :
          vis12 = new Complex(visData12[l][0], visData12[l][1]);

          // baseline BC = 23
          vis23 = new Complex(visData23[l][0], visData23[l][1]);

          // baseline AC = 13 => conjuguate 31 (im = -im)
          vis31 = new Complex(visData13[l][0], -visData13[l][1]);

          // Compute RE/IM bispectrum with C12*C23*~C13 :
          t3Data = ComplexUtils.bispectrum(vis12, vis23, vis31);

          // amplitude :
          t3Amp[k][l] = t3Data.abs();

          // phase [-PI;PI] in degrees :
          t3Phi[k][l] = Math.toDegrees(t3Data.getArgument());
        }

        // UV 1 coords (m) :
        t3U1Coords[k] = u12;
        t3V1Coords[k] = v12;

        // UV 2 coords (m) :
        t3U2Coords[k] = u23;
        t3V2Coords[k] = v23;

        // station indexes :
        t3StaIndexes[k] = triplet.getTripletIndexes();

        j++;
      }
    }

    this.oiFitsFile.addOiTable(t3);
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
   * Return the station indexes (2) per base line mapping (ordered)
   * @param beamMapping beam mapping
   * @param baseLines base line list
   * @return baseline station indexes
   */
  private static Map<BaseLine, short[]> createBaseLineMapping(final Map<Beam, Short> beamMapping, final List<BaseLine> baseLines) {
    // Create BaseLine - indexes mapping :
    final Map<BaseLine, short[]> baseLineIndexes = new LinkedHashMap<BaseLine, short[]>();

    for (BaseLine bl : baseLines) {
      baseLineIndexes.put(bl, new short[]{
                beamMapping.get(bl.getBeam1()).shortValue(),
                beamMapping.get(bl.getBeam2()).shortValue()
              });
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("BaseLine indexes = ");
      for (short[] idx : baseLineIndexes.values()) {
        logger.fine(" " + idx[0] + " " + idx[1]);
      }
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
   * Convert UTC date to string 'YYYY-MM-DD'
   * @param cal UTC date
   * @return string representation
   */
  private static String calendarToString(final Calendar cal) {
    final StringBuilder sb = new StringBuilder();
    sb.append(cal.get(Calendar.YEAR)).append('-');

    final int month = cal.get(Calendar.MONTH) + 1;
    if (month < 10) {
      sb.append('0');
    }
    sb.append(month).append('-');

    final int day = cal.get(Calendar.DAY_OF_MONTH);
    if (day < 10) {
      sb.append('0');
    }
    sb.append(day);

    return sb.toString();
  }

  /**
   * Simple object representing a triplet (3 beams A, B, C) with corresponding baselines and table indexes
   */
  private final static class Triplet {

    /** station indexes (1 .. nBeams) */
    private final short[] tripletIndexes;
    /** baseline indexes (3 couples) */
    private final short[][] baselineIndexes;
    /** relative row positions in OI_VIS table (3) */
    private final int[] relativePosition;

    /**
     * Triplet factory method
     * @param idx 0-based indexes
     * @param baseLineMapping baseline mapping
     * @return triplet instance
     */
    protected static Triplet create(final int[] idx, final Map<BaseLine, short[]> baseLineMapping) {

      final short[] tIndexes = new short[3];
      for (int i = 0; i < 3; i++) {
        tIndexes[i] = (short) (idx[i] + 1);
      }

      // for tIndexes = [123] i.e. ABC
      // couples gives { 12 13 23 } i.e. AB AC BC

      // 3 couples :
      final short[][] bIndexes = new short[3][2];

      for (int i = 0, n = 0; i < 3; i++) {
        for (int j = i + 1; j < 3; j++) {
          bIndexes[n][0] = tIndexes[i];
          bIndexes[n][1] = tIndexes[j];
          n++;
        }
      }

      // Permutations to have { 12 23 13 } i.e. AB BC AC
      // i.e. exchange 2 and 3
      final short[] tmp = bIndexes[1];
      bIndexes[1] = bIndexes[2];
      bIndexes[2] = tmp;

      // Find relative positions in baseline ordering :
      final int[] pos = new int[3];

      final short[][] orderedbaseLineIndexes = baseLineMapping.values().toArray(new short[baseLineMapping.size()][2]);
      final int size = orderedbaseLineIndexes.length;

      short[] find, other;
      for (int n = 0; n < 3; n++) {
        find = bIndexes[n];
        pos[n] = -1;
        for (int i = 0; i < size; i++) {
          other = orderedbaseLineIndexes[i];

          if (Arrays.equals(find, other)) {
            pos[n] = i;
            break;
          }
        }
        if (pos[n] == -1) {
          throw new IllegalStateException("impossible to find couple [" + find[0] + find[1] + "]");
        }
      }

      return new Triplet(tIndexes, bIndexes, pos);
    }

    /**
     * Protected constructor
     * @param tIndexes station indexes
     * @param bIndexes baseline indexes
     * @param pos relative row positions
     */
    private Triplet(final short[] tIndexes, final short[][] bIndexes, final int[] pos) {
      this.tripletIndexes = tIndexes;

      // 3 couples :
      this.baselineIndexes = bIndexes;

      // 3 positions :
      this.relativePosition = pos;
    }

    /**
     * Return the station indexes
     * @return station indexes (3)
     */
    public short[] getTripletIndexes() {
      return tripletIndexes;
    }

    /**
     * Return the baseline indexes (3 couples)
     * @return baseline indexes
     */
    public short[][] getBaselineIndexes() {
      return baselineIndexes;
    }

    /**
     * Return the relative row positions in OI_VIS table (3)
     * @return relative row positions
     */
    public int[] getRelativePosition() {
      return relativePosition;
    }

    /**
     * Return a string representation for this triplet
     * @return string representation
     */
    @Override
    public String toString() {
      final StringBuilder sb = new StringBuilder(32);
      sb.append("Triplet[");

      for (short s : this.tripletIndexes) {
        sb.append(s);
      }
      sb.append("]{ ");

      for (short[] b : this.baselineIndexes) {
        sb.append(b[0]).append(b[1]).append(" ");
      }

      sb.append("} = [ ");

      for (int i : this.relativePosition) {
        sb.append(i).append(" ");
      }

      sb.append("]");
      return sb.toString();
    }
  }
}
