/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.ob;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.OBManager;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.ob.ExtraInformations;
import fr.jmmc.aspro.model.ob.InstrumentConfiguration;
import fr.jmmc.aspro.model.ob.InterferometerConfiguration;
import fr.jmmc.aspro.model.ob.OBItem;
import fr.jmmc.aspro.model.ob.ObservationConfiguration;
import fr.jmmc.aspro.model.ob.ObservationConstraints;
import fr.jmmc.aspro.model.ob.ObservationSchedule;
import fr.jmmc.aspro.model.ob.ObservationType;
import fr.jmmc.aspro.model.ob.ObservingBlockDefinition;
import fr.jmmc.aspro.model.observability.DateTimeInterval;
import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.observability.StarData;
import fr.jmmc.aspro.model.oi.AtmosphereQuality;
import fr.jmmc.aspro.model.oi.BaseValue;
import fr.jmmc.aspro.model.oi.BooleanValue;
import fr.jmmc.aspro.model.oi.CalibratorInformations;
import fr.jmmc.aspro.model.oi.Channel;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.FringeTracker;
import fr.jmmc.aspro.model.oi.NumberValue;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.aspro.model.oi.StringValue;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetConfiguration;
import fr.jmmc.aspro.model.oi.TargetGroup;
import fr.jmmc.aspro.model.oi.TargetInformation;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.model.util.AtmosphereQualityUtils;
import fr.jmmc.aspro.model.util.SpectralBandUtils;
import fr.jmmc.aspro.service.ObservabilityService;
import fr.jmmc.jmal.Band;
import fr.jmmc.jmcs.data.MimeType;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.StringUtils;
import fr.jmmc.oitools.model.range.Range;
import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class generates the common part of observing blocks for the VLTI
 * @author bourgesl
 */
public class ExportOBXml {

    /** Class logger */
    protected static final Logger logger = LoggerFactory.getLogger(ExportOBXml.class.getName());

    /** OB manager */
    private final static OBManager obm = OBManager.getInstance();

    /** file prefix for science targets */
    public static final String OB_SCIENCE = "SCI";
    /** file prefix for calibrator targets */
    public static final String OB_CALIBRATOR = "CAL";
    /** NONE value */
    public static final String VALUE_NONE = "NONE";

    /** time separator */
    public static final char SEP_TIME = '/';
    /** P2 minimal duration = 10 minutes in seconds */
    public static final int P2_LST_MIN_DURATION = 10 * 60;
    /** double formatter for HA values */
    private final static NumberFormat df2 = new DecimalFormat("0.00");
    /** time formatter */
    private final static DateFormat timeFormatter = new SimpleDateFormat("HH:mm");

    /**
     * Forbidden constructor
     */
    protected ExportOBXml() {
        // no-op
    }

    /**
     * Compute observability for the complete observation (no night limits).
     * @param observation observation to use
     * @return observability service with computed data
     */
    public static ObservabilityService processObservability(final ObservationSetting observation) {
        // Compute Observability data using astronomical night (-18 deg) without night restrictions :
        final ObservabilityService os = new ObservabilityService(observation, true);

        // compute observability data:
        os.compute();

        return os;
    }

    /**
     * Generate the OB XML file for the given target.
     * @param file file to save
     * @param observation observation to use
     * @param os observability service with computed data
     * @param target target to process
     *
     * @throws IOException if an I/O exception occured while writing the observing block
     */
    public static void process(final File file,
            final ObservationSetting observation,
            final ObservabilityService os,
            final Target target) throws IOException {

        if (logger.isDebugEnabled()) {
            logger.debug("process target {} to file: ", target.getName(), file);
        }

        if (observation != null && target != null) {
            final ObservingBlockDefinition obd = new ObservingBlockDefinition();

            fill(obd, observation, os, target);

            OBManager.getInstance().saveObject(file, obd);
        }
    }

    /**
     * Fill the OB for the given target
     * @param obd OB to fill
     * @param observation observation settings
     * @param os observability service with computed data
     * @param target target to process
     *
     * @throws IllegalStateException if the template file is not found or can not be read
     * @throws IOException if an I/O exception occurred while writing the observing block
     */
    private static void fill(final ObservingBlockDefinition obd,
            final ObservationSetting observation,
            final ObservabilityService os,
            final Target target) throws IllegalStateException, IOException {

        final TargetConfiguration targetConf = target.getConfiguration();

        // Interferometer configuration
        obd.setInterferometerConfiguration(getInterferometerConfiguration(observation, targetConf));

        // Instrument configuration
        obd.setInstrumentConfiguration(getInstrumentConfiguration(observation, targetConf));

        // Observation configuration - SCI
        final ObservationConfiguration obSCI = getObservationConfiguration(observation, os, target);
        obd.getObservationConfigurations().add(obSCI);

        // Observation configuration - CAL  (store first cal only for schedule) 
        ObservationConfiguration obCAL = null;

        final TargetUserInformations targetUserInfos = observation.getOrCreateTargetUserInfos();

        if (targetUserInfos != null && !targetUserInfos.isCalibrator(target)) {
            // use first calibrator in calibrator list for later reuse in schedule:
            final TargetInformation targetInfo = targetUserInfos.getTargetInformation(target);
            if (targetInfo != null) {
                final List<Target> calibrators = targetInfo.getCalibrators();
                if (!calibrators.isEmpty()) {
                    for (Target calibrator : calibrators) {

                        final ObservationConfiguration ob = getObservationConfiguration(observation, os, calibrator);
                        obd.getObservationConfigurations().add(ob);
                        // keep first cal for schedule below
                        obCAL = (obCAL != null) ? obCAL : ob;
                    }
                }
            }
        }

        // Schedule
        final ObservationSchedule obsSch = new ObservationSchedule();
        final OBItem sci = new OBItem(obSCI);
        final OBItem cal = (obCAL != null) ? new OBItem(obCAL) : null;

        if (cal != null) {
            obsSch.getOBS().add(cal);
        }
        obsSch.getOBS().add(sci);
        if (cal != null) {
            obsSch.getOBS().add(cal);
        }
        obd.setObservationSchedule(obsSch);
    }

    private static InterferometerConfiguration getInterferometerConfiguration(final ObservationSetting observation,
            final TargetConfiguration targetConf) {

        final InterferometerConfiguration intConf = new InterferometerConfiguration();

        final fr.jmmc.aspro.model.oi.InterferometerConfiguration obsIntConf = observation.getInterferometerConfiguration().getInterferometerConfiguration();
        intConf.setName(obsIntConf.getInterferometer().getName());
        intConf.setVersion(obsIntConf.getVersion());

        intConf.setStations(observation.getInstrumentConfiguration().getStations());

        final String confAltBaselines = ConfigurationManager.getInstance().getInstrumentConfigurationAltBaselines(
                observation.getInterferometerConfiguration().getName(),
                observation.getInstrumentConfiguration().getName(),
                observation.getInstrumentConfiguration().getStations());

        if (confAltBaselines != null) {
            intConf.setConfAltName(confAltBaselines);
        }

        // CHARA Pops:
        final String pops = observation.getInstrumentConfiguration().getPops();
        if (pops != null) {
            intConf.setPops(pops);
        }

        // find the optional channels associated to the stations in the instrument configuration :
        // CHARA : predefined channel per station for a specific base line :
        final List<Channel> relatedChannels = ConfigurationManager.getInstance().getInstrumentConfigurationChannels(
                observation.getInterferometerConfiguration().getName(),
                observation.getInstrumentConfiguration().getName(),
                observation.getInstrumentConfiguration().getStations());
        if (relatedChannels != null) {
            intConf.setChannels(Channel.toString(relatedChannels));
        }

        // set AOsetup (from given target):
        if (targetConf != null && targetConf.getAoSetup() != null) {
            intConf.setAoSetup(targetConf.getAoSetup());
        }

        return intConf;
    }

    private static InstrumentConfiguration getInstrumentConfiguration(final ObservationSetting observation,
            final TargetConfiguration targetConf) {

        final InstrumentConfiguration insConf = new InstrumentConfiguration();

        insConf.setName(observation.getInstrumentConfiguration().getName());
        insConf.setInstrumentMode(observation.getInstrumentConfiguration().getInstrumentMode());
        insConf.setAcquisitionTime(observation.getInstrumentConfiguration().getAcquisitionTime());

        String ftMode = VALUE_NONE;
        if (targetConf != null && targetConf.getFringeTrackerMode() != null) {
            ftMode = targetConf.getFringeTrackerMode();
        }
        insConf.setFringeTrackerMode(ftMode); // "NONE" if undefined

        final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();

        // use target's wavelength ref instead of default value:
        double wlRef = insMode.getEffWaveLengthRef();

        if (targetConf != null && targetConf.getInstrumentWaveLengthRef() != null) {
            wlRef = targetConf.getInstrumentWaveLengthRef();
        }

        final double dlRef = insMode.getEffWaveLengthBandRef();

        // DIT:
        double dit = insMode.getSetupRef().getDit();
        if (insMode.getDit() != null) {
            dit = insMode.getDit();
        }

        // MATISSE:
        if (!VALUE_NONE.equals(insConf.getFringeTrackerMode())) {
            if (insMode.getFtDit() != null) {
                dit = insMode.getFtDit();
            } else {
                final FringeTracker ft = observation.getInstrumentConfiguration().getInstrumentConfiguration().getFocalInstrument().getFringeTracker();;
                if (ft != null) {
                    // TODO: handle FT modes properly: GroupTrack is hard coded !
                    if (!ftMode.startsWith(AsproConstants.FT_GROUP_TRACK)) {
                        dit = ft.getMaxIntegration();
                    }
                }
            }
        }
        insConf.setDit(dit);
        insConf.setInstrumentWaveLengthRef(NumberUtils.trimTo5Digits(wlRef));
        insConf.setInstrumentWaveBandRef(NumberUtils.trimTo5Digits(dlRef));

        return insConf;
    }

    private static ObservationConfiguration getObservationConfiguration(final ObservationSetting observation,
            final ObservabilityService os,
            final Target target) {

        final TargetUserInformations targetUserInfos = observation.getTargetUserInfos();
        final boolean isCalibrator = (targetUserInfos != null && targetUserInfos.isCalibrator(target));

        final ObservationConfiguration obsConf = new ObservationConfiguration();
        obsConf.setId(target.getId());
        obsConf.setType(isCalibrator ? ObservationType.CALIBRATION : ObservationType.SCIENCE);

        final fr.jmmc.aspro.model.ob.Target obTarget = getTarget(target);
        obsConf.setSCTarget(obTarget);

        // Define the calibrator Angular diameter (mas)
        Double diameter = null;
        if (isCalibrator) {
            final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();

            final SpectralBand insBand = SpectralBandUtils.findBand(Band.findBand(insMode.getWaveLength()));

            diameter = target.getDiameter(insBand);
        }
        obTarget.setDIAMETER(diameter);

        if (targetUserInfos != null) {
            // Handle OB targets (AO / FT / Guide)
            final TargetInformation targetInfo = targetUserInfos.getOrCreateTargetInformation(target);

            // AO
            final Target aoTarget = TargetUserInformations.getFirstTargetForGroup(targetUserInfos, targetInfo,
                    TargetGroup.GROUP_AO);
            if (aoTarget != null) {
                obsConf.setAOTarget(getTarget(aoTarget));
            }
            // FT
            final Target ftTarget = TargetUserInformations.getFirstTargetForGroup(targetUserInfos, targetInfo,
                    TargetGroup.GROUP_FT);
            if (ftTarget != null) {
                obsConf.setFTTarget(getTarget(ftTarget));
            }
            // GUIDE
            final Target gsTarget = TargetUserInformations.getFirstTargetForGroup(targetUserInfos, targetInfo,
                    TargetGroup.GROUP_GUIDE);
            if (gsTarget != null) {
                obsConf.setGSTarget(getTarget(gsTarget));
            }
        }

        // constraints
        final ObservationConstraints obsCons = new ObservationConstraints();

        // Atmosphere:
        final AtmosphereQuality atmQual = observation.getWhen().getAtmosphereQuality();
        obsCons.setAtmosphereQuality(atmQual.value());
        obsCons.setSeeing(AtmosphereQualityUtils.getSeeing(atmQual)); // as
        obsCons.setTau0(AtmosphereQualityUtils.getCoherenceTime(atmQual) * 0.001); // s

        // Compute Observability data:
        final ObservabilityData obsData = os.getData();

        final StarData starData = obsData.getStarData(target.getName());
        if (starData != null) {
            final List<Range> obsRangesHA = starData.getObsRangesHA();

            if (logger.isDebugEnabled()) {
                logger.debug("obsRangesHA: {}", obsRangesHA);
            }

            if (obsRangesHA != null) {
                // target is observable :
                processHARanges(obsCons.getHAintervals(), obsRangesHA);

                final List<DateTimeInterval> lstRanges = os.convertHARangesToDateInterval(obsRangesHA, starData.getPrecRA());
                if (logger.isDebugEnabled()) {
                    logger.debug("lst ranges: {}", lstRanges);
                }

                if (lstRanges != null) {
                    processLSTRanges(obsCons.getLSTintervals(), lstRanges);
                }
            }
        }

        if (!obsCons.getHAintervals().isEmpty()) {
            obsConf.setObservationConstraints(obsCons);
        }

        return obsConf;
    }

    private static fr.jmmc.aspro.model.ob.Target getTarget(final Target target) {

        final fr.jmmc.aspro.model.ob.Target t = new fr.jmmc.aspro.model.ob.Target();
        t.setName(target.getName());
        t.setRA(target.getRA());
        t.setDEC(target.getDEC());
        t.setEQUINOX(target.getEQUINOX());
        t.setSYSVEL(target.getSYSVEL());
        t.setVELTYP(target.getVELTYP());
        t.setPMRA(target.getPMRA());
        t.setPMDEC(target.getPMDEC());
        t.setPARALLAX(target.getPARALLAX());
        t.setPARAERR(target.getPARAERR());
        t.setIDS(target.getIDS());
        t.setOBJTYP(target.getOBJTYP());
        t.setSPECTYP(target.getSPECTYP());

        if (target.getFLUXB() != null) {
            t.setFLUXB(NumberUtils.trimTo3Digits(target.getFLUXB()));
        }
        if (target.getFLUXV() != null) {
            t.setFLUXV(NumberUtils.trimTo3Digits(target.getFLUXV()));
        }
        if (target.getFLUXG() != null) {
            t.setFLUXG(NumberUtils.trimTo3Digits(target.getFLUXG()));
        }

        if (target.getFLUXR() != null) {
            t.setFLUXR(NumberUtils.trimTo3Digits(target.getFLUXR()));
        }
        if (target.getFLUXI() != null) {
            t.setFLUXI(NumberUtils.trimTo3Digits(target.getFLUXI()));
        }

        if (target.getFLUXJ() != null) {
            t.setFLUXJ(NumberUtils.trimTo3Digits(target.getFLUXJ()));
        }
        if (target.getFLUXH() != null) {
            t.setFLUXH(NumberUtils.trimTo3Digits(target.getFLUXH()));
        }
        if (target.getFLUXK() != null) {
            t.setFLUXK(NumberUtils.trimTo3Digits(target.getFLUXK()));
        }

        if (target.getFLUXL() != null) {
            t.setFLUXL(NumberUtils.trimTo3Digits(target.getFLUXL()));
        }
        if (target.getFLUXM() != null) {
            t.setFLUXM(NumberUtils.trimTo3Digits(target.getFLUXM()));
        }
        if (target.getFLUXN() != null) {
            t.setFLUXN(NumberUtils.trimTo3Digits(target.getFLUXN()));
        }

        // convert to Jansky:
        if (target.getFLUXL() != null) {
            t.setFLUXLJY(NumberUtils.trimTo3Digits(Band.L.magToJy(target.getFLUXL())));
        }
        if (target.getFLUXM() != null) {
            t.setFLUXMJY(NumberUtils.trimTo3Digits(Band.M.magToJy(target.getFLUXM())));
        }
        if (target.getFLUXN() != null) {
            t.setFLUXNJY(NumberUtils.trimTo3Digits(Band.N.magToJy(target.getFLUXN())));
        }

        // Copy calInfo into extraInfos if not null
        final CalibratorInformations calInfos = target.getCalibratorInfos();
        if (calInfos != null) {
            final ExtraInformations extraInfos = new ExtraInformations();
            for (BaseValue oiValue : calInfos.getFields()) {
                extraInfos.getFields().add(getOBValue(oiValue));
            }
            for (BaseValue oiValue : calInfos.getParameters()) {
                extraInfos.getParameters().add(getOBValue(oiValue));
            }
            t.setEXTRAINFORMATIONS(extraInfos);
        }

        return t;
    }

    private static fr.jmmc.aspro.model.ob.BaseValue getOBValue(BaseValue oiValue) {

        if (oiValue instanceof BooleanValue) {
            fr.jmmc.aspro.model.ob.BooleanValue b = new fr.jmmc.aspro.model.ob.BooleanValue();
            b.setValue(oiValue.getBoolean());
            b.setName(oiValue.getName());
            b.setUnit(oiValue.getUnit());
            return b;
        }

        if (oiValue instanceof NumberValue) {
            fr.jmmc.aspro.model.ob.NumberValue n = new fr.jmmc.aspro.model.ob.NumberValue();
            n.setValue(oiValue.getNumber());
            n.setName(oiValue.getName());
            n.setUnit(oiValue.getUnit());
            return n;
        }

        if (oiValue instanceof StringValue) {
            fr.jmmc.aspro.model.ob.StringValue s = new fr.jmmc.aspro.model.ob.StringValue();
            s.setValue(oiValue.getString());
            s.setName(oiValue.getName());
            s.setUnit(oiValue.getUnit());
            return s;
        }

        return null;
    }

    private static void processHARanges(final List<String> haIntervals, final List<Range> obsRangesHA) {
        final StringBuilder sb = new StringBuilder(12);

        for (Range range : obsRangesHA) {
            sb.setLength(0);

            sb.append(df2.format(range.getMin()));
            sb.append(SEP_TIME);
            sb.append(df2.format(range.getMax()));

            haIntervals.add(sb.toString());
        }
    }

    private static void processLSTRanges(final List<String> lstIntervals, final List<DateTimeInterval> lstRanges) {
        final StringBuilder sb = new StringBuilder(12);

        final Calendar cal = new GregorianCalendar();

        for (DateTimeInterval interval : lstRanges) {
            sb.setLength(0);

            final Date start = interval.getStartDate();
            final Date end = interval.getEndDate();

            cal.setTime(start);
            final int secStart = convertDateToSeconds(cal);

            cal.setTime(end);
            final int secEnd = convertDateToSeconds(cal);

            boolean valid = false;

            if (end.before(start)) {
                // single interval over midnight:
                if (((86400 + secEnd) - secStart) > P2_LST_MIN_DURATION) {
                    valid = true;
                }
            } else {
                if ((secEnd - secStart) > P2_LST_MIN_DURATION) {
                    valid = true;
                }
            }

            if (valid) {
                sb.append(timeFormatter.format(interval.getStartDate()));
                sb.append(SEP_TIME);
                sb.append(timeFormatter.format(interval.getEndDate()));

                lstIntervals.add(sb.toString());
            } else {
                logger.info("invalid interval: {}", interval);
            }
        }
    }

    /**
     * Convert a calendar time to seconds rounding to the lower or upper minute
     * according to the given sign
     * @param cal calendar time
     * @return seconds
     */
    private static int convertDateToSeconds(final Calendar cal) {
        final int h = cal.get(Calendar.HOUR_OF_DAY);
        final int m = cal.get(Calendar.MINUTE);
        final int s = cal.get(Calendar.SECOND);

        return h * 3600 + m * 60 + s;
    }

    /**
     * Generate the Observing block file name using the given target
     * @param target target to use
     * @return Observing block file name
     */
    public static String generateOBFileName(final Target target) {
        return generateOBFileName(target, null);
    }

    /**
     * Generate the Observing block file name using the given target
     * @param target target to use
     * @param customPrefix custom prefix or null
     * @return Observing block file name
     */
    public static String generateOBFileName(final Target target, final String customPrefix) {

        final ObservationManager om = ObservationManager.getInstance();

        // use main observation :
        final ObservationSetting observation = om.getMainObservation();

        // get instrument band :
        final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();
        if (insMode == null) {
            throw new IllegalStateException("The instrumentMode is empty !");
        }

        final TargetConfiguration targetConf = target.getConfiguration();

        final boolean useFT = (targetConf != null && targetConf.getFringeTrackerMode() != null);

        final String suffix = StringUtils.removeUnderscores(insMode.getName()) + '_' + (useFT ? "FT" : "noFT");
        final String prefix = (customPrefix != null) ? customPrefix
                : (om.isCalibrator(target) ? OB_CALIBRATOR : OB_SCIENCE);

        return generateFileName(observation, target.getName(), prefix, suffix, MimeType.ASPRO_OB_XML.getExtension());
    }

    /**
     * Generate a standard file name for the selected target using the format :
     * [<prefix>_<TARGET>_<INSTRUMENT>_<CONFIGURATION>_<suffix>.<extension>]
     *
     * @param observation main observation
     * @param targetName target name
     * @param prefix prefix (optional) for the file name
     * @param suffix suffix (optional) before the date
     * @param extension file extension
     * @return standard file name
     */
    private static String generateFileName(final ObservationSetting observation, final String targetName,
            final String prefix, final String suffix,
            final String extension) {

        final StringBuilder sb = new StringBuilder(32);
        if (prefix != null) {
            sb.append(prefix).append('_');
        }

        // replace invalid characters :
        final String altTargetName = StringUtils.removeNonAlphaNumericChars(targetName);
        sb.append(altTargetName).append('_');

        final String instrumentName = observation.getInstrumentConfiguration().getName();
        sb.append(instrumentName).append('_');

        final String baseLine = StringUtils.removeWhiteSpaces(observation.getInstrumentConfiguration().getStations());
        sb.append(baseLine);

        if (suffix != null) {
            sb.append('_').append(suffix);
        }

        final String date = observation.getWhen().getDate().toString();
        sb.append('_').append(date);

        // maximum length of both the OB name and file name:
        if (sb.length() > 64) {
            sb.setLength(64);
        }

        sb.append('.').append(extension);

        return sb.toString();
    }
}
