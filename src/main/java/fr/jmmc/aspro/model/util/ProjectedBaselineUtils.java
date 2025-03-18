/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData;
import fr.jmmc.jmcs.util.StringUtils;
import java.util.ArrayList;
import java.util.List;
import org.json.JSONException;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Class to convert ProjectedBaselines from the JSON dictionary (obs portal) into UVRangeBaseLineData instances
 */
public final class ProjectedBaselineUtils {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(ProjectedBaselineUtils.class.getName());

    // json keys:
    private final static String KEY_ANGLE = "angle";
    private final static String KEY_LENGTH = "length";
    private final static String KEY_START = "start";
    private final static String KEY_END = "end";

    private final static double PREC_ANGLE = 0.01; // no better than 0.01 deg
    private final static double PREC_LENGTH = 0.001 / 2.0; // half 1 mm

    private ProjectedBaselineUtils() {
        super(); // forbidden
    }

    /*
        Normal case:
        {
            'U1-U2': {
                'length': {'start': '55.9040', 'end': '55.8970'},
                'angle': {'start': '3.3000', 'end': '4.0000'}
            },
            ...
        }
        Bad case (None in values):
        {
            'U3-U1': {
                'length': {'start': '102.2030', 'end': '102.2500'}, 
                'angle': {'start': None, 'end': None}
            }
        }
     */
    public static List<UVRangeBaseLineData> parse(final String projectedBaselines,
                                                  final FocalInstrumentMode insMode) {
        List<UVRangeBaseLineData> result = null;

        if (insMode != null && !StringUtils.isEmpty(projectedBaselines)) {
            final JSONObject jsObj = new JSONObject(projectedBaselines);

            result = new ArrayList<UVRangeBaseLineData>(jsObj.length());

            final boolean isDebug = logger.isDebugEnabled();

            for (final String baseline : jsObj.keySet()) {
                final JSONObject blObj = jsObj.getJSONObject(baseline);

                if (isDebug) {
                    logger.debug("baseline: {}", baseline);
                    logger.debug("blObj: {}", blObj.toMap());
                }

                try {
                    double angle_start = Double.NaN;
                    double angle_end = Double.NaN;

                    if (blObj.has(KEY_ANGLE)) {
                        final JSONObject angObj = blObj.getJSONObject(KEY_ANGLE);

                        angle_start = angObj.getDouble(KEY_START);
                        angle_end = angObj.getDouble(KEY_END);
                    }

                    double length_start = Double.NaN;
                    double length_end = Double.NaN;

                    if (blObj.has(KEY_LENGTH)) {
                        final JSONObject lenObj = blObj.getJSONObject(KEY_LENGTH);

                        length_start = lenObj.getDouble(KEY_START);
                        length_end = lenObj.getDouble(KEY_END);
                    }

                    if (!Double.isNaN(angle_start) && !Double.isNaN(angle_end)
                            && !Double.isNaN(length_start) && !Double.isNaN(length_end)) {
                        // Ensure length > 0
                        if (length_start > 0.0 && length_end > 0.0) {
                            result.add(convert(baseline,
                                    angle_start - PREC_ANGLE,
                                    length_start - PREC_LENGTH,
                                    angle_end + PREC_ANGLE,
                                    length_end + PREC_LENGTH,
                                    insMode, isDebug));
                        } else {
                            if (isDebug) {
                                logger.debug("Bad length values: {} or {} in '{}'", length_start, length_end, blObj.toMap());
                            }
                        }
                    }
                } catch (JSONException je) {
                    if (isDebug) {
                        logger.debug("JSON exception: {} in '{}'", je.getMessage(), blObj.toMap());
                    }
                }
            }
            if (result.isEmpty()) {
                result = null;
            }
        }
        return result;
    }

    private static UVRangeBaseLineData convert(final String baseline,
                                               final double angle_start, final double length_start,
                                               final double angle_end, final double length_end,
                                               final FocalInstrumentMode insMode,
                                               final boolean isDebug) {

        // See UVCoverageService.computeUVPoints()
        if (insMode == null) {
            throw new IllegalStateException("The instrumentMode is empty !");
        }

        if (isDebug) {
            logger.debug("instrumentMode: {}", insMode.getName());
        }

        // Get wavelength range for the selected instrument mode :
        final double lambdaMin = AsproConstants.MICRO_METER * insMode.getWaveLengthMin();
        final double lambdaMax = AsproConstants.MICRO_METER * insMode.getWaveLengthMax();

        if (isDebug) {
            logger.debug("lambdaMin: {}", lambdaMin);
            logger.debug("lambdaMax: {}", lambdaMax);
        }

        final double invLambdaMin = 1.0 / lambdaMin;
        final double invLambdaMax = 1.0 / lambdaMax;

        /* pure U,V coordinates (m) */
        final double[] u = new double[2];
        final double[] v = new double[2];
        /* U,V coordinates corrected with minimal wavelength */
        final double[] uWMin = new double[2];
        final double[] vWMin = new double[2];
        /* U,V coordinates corrected with maximal wavelength */
        final double[] uWMax = new double[2];
        final double[] vWMax = new double[2];

        double angle;
        // start:
        angle = Math.toRadians(90.0 - angle_start);

        // Baseline projected vector (m) :
        u[0] = length_start * Math.cos(angle);
        v[0] = length_start * Math.sin(angle);

        // wavelength correction :
        // Spatial frequency (rad-1) :
        uWMin[0] = u[0] * invLambdaMin;
        vWMin[0] = v[0] * invLambdaMin;

        uWMax[0] = u[0] * invLambdaMax;
        vWMax[0] = v[0] * invLambdaMax;

        // end:
        angle = Math.toRadians(90.0 - angle_end);

        // Baseline projected vector (m) :
        u[1] = length_end * Math.cos(angle);
        v[1] = length_end * Math.sin(angle);

        // wavelength correction :
        // Spatial frequency (rad-1) :
        uWMin[1] = u[1] * invLambdaMin;
        vWMin[1] = v[1] * invLambdaMin;

        uWMax[1] = u[1] * invLambdaMax;
        vWMax[1] = v[1] * invLambdaMax;

        return new UVRangeBaseLineData(baseline, 2, u, v, uWMin, vWMin, uWMax, vWMax);
    }

    public static void main(String[] unused) {
        final FocalInstrumentMode insMode = new FocalInstrumentMode();
        insMode.setWaveLengthMin(1.0);
        insMode.setWaveLengthMax(2.0);

        logger.info("(KO): {}",
                parse("{"
                        + "  'U3-U1': {"
                        + "      'length': {'start': '102.2030', 'end': '102.2500'}, "
                        + "      'angle': {'start': None, 'end': None} "
                        + "  }"
                        + "}",
                        insMode)
        );

        logger.info("(GRAV): {}",
                parse("{'U1-U2': {'length': {'start': '55.9040', 'end': '55.8970'}, 'angle': {'start': '3.3000', 'end': '4.0000'}},"
                        + " 'U1-U3': {'length': {'start': '102.0940', 'end': '102.0770'}, 'angle': {'start': '8.2000', 'end': '8.9000'}},"
                        + " 'U1-U4': {'length': {'start': '126.7160', 'end': '126.9170'}, 'angle': {'start': '29.9210', 'end': '30.8160'}},"
                        + " 'U2-U3': {'length': {'start': '46.6400', 'end': '46.6410'}, 'angle': {'start': '14.1000', 'end': '14.9000'}},"
                        + " 'U2-U4': {'length': {'start': '80.6940', 'end': '81.0780'}, 'angle': {'start': '47.9770', 'end': '48.9590'}},"
                        + " 'U3-U4': {'length': {'start': '49.3540', 'end': '49.8580'}, 'angle': {'start': '79.7460', 'end': '80.5870'}}}",
                        insMode)
        );
    }
}
