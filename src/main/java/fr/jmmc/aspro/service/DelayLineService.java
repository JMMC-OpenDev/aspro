/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.oitools.model.range.Range;
import fr.jmmc.oitools.model.range.RangeFactory;
import fr.jmmc.aspro.util.AngleUtils;
import fr.jmmc.jmal.util.MathUtils;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import net.jafama.FastMath;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class manages the computation to find the base line limits
 * @author bourgesl
 */
public final class DelayLineService {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(DelayLineService.class.getName());
    /** cached log debug enabled */
    private static final boolean DEBUG = false;
    /** Range List containing a single full range (-12; +12) */
    public static final List<Range> FULL_RANGE_LIST = Arrays.asList(new Range(AsproConstants.HA_MIN, AsproConstants.HA_MAX));
    /** Empty Range list */
    public static final List<Range> EMPTY_RANGE_LIST = Collections.emptyList();
    /** PI */
    private static final double PI = Math.PI;
    /** -PI */
    private static final double MINUS_PI = -Math.PI;
    /** 2 x PI */
    private static final double TWO_PI = 2d * Math.PI;

    /**
     * Forbidden contructor
     */
    private DelayLineService() {
        super();
    }

    /**
     * Return the intervals (hour angles) for all base lines compatible with wMin < w(h) < wMax,
     * wMin and wMax are given by wRanges.
     *
     * see astro_delayline.DL_INTERVAL_LIST(...)
     *
     * @param dec target declination (rad)
     * @param baseLines base line list
     * @param wRanges [wMin - wMax] ranges per base line
     * @param ha double[2] array to avoid array allocations
     * @param haValues double[6] array to avoid array allocations
     * @param w double[2] array to avoid array allocations
     * @param rangeFactory Factory used to create Range and List[Range] instances
     * @return intervals (hour angles)
     */
    public static List<List<Range>> findHAIntervals(final double dec, final List<BaseLine> baseLines, final List<Range> wRanges,
                                                    final double[] ha, final double[] haValues, final double[] w,
                                                    final RangeFactory rangeFactory) {

        final double cosDec = FastMath.cos(dec);
        final double sinDec = FastMath.sin(dec);

        final int size = baseLines.size();
        if (size == 0) {
            return Collections.EMPTY_LIST;
        }

        final List<List<Range>> rangesBL = new ArrayList<List<Range>>(size);

        BaseLine bl;
        Range wRange;
        double[] wExtrema;
        for (int i = 0; i < size; i++) {
            bl = baseLines.get(i);
            wRange = wRanges.get(i);

            // First check the W limits :
            wExtrema = findWExtrema(cosDec, sinDec, bl, w);

            rangesBL.add(findHAIntervalsForBaseLine(cosDec, sinDec, bl, wExtrema, wRange, ha, haValues, rangeFactory));
        }

        return rangesBL;
    }

    /**
     * -----------------------------------------------------------------------
     * Finds the interval in hour angle where w(h) is .GE.WMIN AND .LE.WMAX
     * -----------------------------------------------------------------------
     *
     * see astro_delayline.DELAYS_INTERVAL(WMIN,WMAX,D,X,Y,Z,HLIST,N)
     *
     * @param cosDec cosinus of target declination
     * @param sinDec sinus of target declination
     * @param baseLine base line
     * @param wExtrema W extrema values for the given baseline
     * @param wRange [wMin - wMax] range
     * @param ha double[2] array to avoid array allocations
     * @param haValues double[6] array to avoid array allocations
     * @param rangeFactory Factory used to create Range and List[Range] instances
     * @return intervals (hour angles) in dec hours.
     */
    public static List<Range> findHAIntervalsForBaseLine(final double cosDec, final double sinDec, final BaseLine baseLine,
                                                         final double[] wExtrema, final Range wRange,
                                                         final double[] ha, final double[] haValues,
                                                         final RangeFactory rangeFactory) {

        if (wExtrema == null) {
            // no solution :
            return EMPTY_RANGE_LIST;
        }

        // test if [WMIN,WMAX] has intersection with MAXW[1-2] :
        final double wLower = wExtrema[0];
        final double wUpper = wExtrema[1];

        final double wMin = wRange.getMin();
        final double wMax = wRange.getMax();

        if (wMax < wLower || wMin > wUpper) {
            // outside range, no solution :
            if (DEBUG) {
                logger.info("W outside range: {} : {} / W extrema = [{}, {}]", baseLine.getName(), wRange, wLower, wUpper);
            }
            return EMPTY_RANGE_LIST;
        }
        if (wLower > wMin && wUpper < wMax) {
            // always inside range = full interval [-12h;12h]:
            if (DEBUG) {
                logger.info("W inside range: {} : {} / W extrema = [{}, {}]", baseLine.getName(), wRange, wLower, wUpper);
            }
            return FULL_RANGE_LIST;
        }

        if (DEBUG) {
            logger.info("W extrema = [{}, {}]", wLower, wUpper);
        }

        // haValues[6] = list of hour angles (rad) in [-PI;PI] range :
        // define ha limits :
        haValues[0] = MINUS_PI;
        haValues[1] = PI;

        int nHA = 2;

        if (solveDelays(cosDec, sinDec, baseLine, wMin, ha)) {
            haValues[nHA++] = ha[0];
            haValues[nHA++] = ha[1];
        }

        if (solveDelays(cosDec, sinDec, baseLine, wMax, ha)) {
            haValues[nHA++] = ha[0];
            haValues[nHA++] = ha[1];
        }

        // We have then the (at max 6) peculiar hour angles.
        // Sort them by ascending order
        Arrays.sort(haValues, 0, nHA);

        if (DEBUG) {
            logger.info("haList ({} first values): {}", nHA, Arrays.toString(haValues));
        }

        // output :
        // size / 2 because only half intervals are inside [wMin; wMax]:
        final List<Range> ranges = rangeFactory.getList(); // new ArrayList<Range>(nHA / 2);

        // Look if midpoints values are or not in the HMAX-HMIN interval
        // to find which intervals are correct :
        double haLo, haUp, haMid, wMid;

        for (int i = 0, size = nHA - 1; i < size; i++) {
            haLo = haValues[i];
            haUp = haValues[i + 1];

            haMid = 0.5d * (haLo + haUp);

            wMid = CalcUVW.computeW(cosDec, sinDec, baseLine, haMid);

            if (DEBUG) {
                logger.info("W({}) = {}", haMid, wMid);
            }

            if ((wMid >= wMin) && (wMid <= wMax)) {
                // this ha interval is valid :
                ranges.add(rangeFactory.valueOf(AngleUtils.rad2hours(haLo), AngleUtils.rad2hours(haUp)));
            }
        }

        if (DEBUG) {
            logger.info("valid intervals (dec hours): {}", ranges);
        }

        return ranges;
    }

    /**
     * see astro_delayline.SOLVE_DELAYS(THROW,D,X,Y,Z,H0,N)
     * 
     * -----------------------------------------------------------------------
     * Solves the equation W(H)=cte, where W(H) is the delay on the baseline B
     * of components X,Y and Z, seen as a function of H, the hour angle, for
     * an object at declination D:
     * the transformation from the equatorial coordinates Bx,By,Bz
     * of a Baseline B to u,v,w is given by the matrix:
     * u       / sin(H)       , cos(H)       ,      0 \   Bx
     * v  =    |-sin(D)*cos(H), sin(D)*sin(H), cos(D) | * By
     * w       \ cos(D)*cos(H),-cos(D)*sin(H), sin(D) /   Bz
     * where H is the hour angle and D the declination of the source
     * To insure that w=cos(D)(cos(H)*X-sin(H)*Y)+sin(D)*Z is between 0 and
     * THROW,
     * we have to solve w=throw, i.e. w-throw=0 ,i.e.,
     * cos(D)(cos(H)*X-sin(H)*Y)+sin(D)*Z-THROW = 0
     *
     * if cos(D)=0 this equation becomes Z=THROW, that is,
     * H=[-PI,PI] if Z=THROW, H nonexistent if not.
     *
     * if not we have: 
     * cos(H)*X-sin(H)*Y=(THROW-sin(D)*Z)/cos(D)
     *
     * if we put: TAN(H/2)=t, and (THROW-sin(D)*Z)/cos(D)=C
     * we have
     * cos(H) --> (1-t**2)/(1+t**2)
     * sin(H) --> (2*t)/(1+t**2)
     * and the equation becomes:
     * C(1+t^2)=Xt-Xt^2-2Yt => (C+X)t^2+(2Y)t+(C-X)=0
     * so the solutions are (if any)
     * t(1,2) = - Y/(C+X) +/- 1/(C+X) SQRT(Y**2+X**2-C**2)
     * and H = 2 ATAN(t)
     * -----------------------------------------------------------------------
     *
     * @param cosDec cosinus of target declination
     * @param sinDec sinus of target declination
     * @param baseLine used base line
     * @param wThrow w limit value (throw)
     * @param ha double[2] array to store ha solutions (rad) in [-PI;PI] range and avoid array allocations
     * @return true if ha solutions found; false otherwise
     */
    private static boolean solveDelays(final double cosDec, final double sinDec, final BaseLine baseLine, final double wThrow, final double[] ha) {
        if (cosDec == 0d) {
            final double w = sinDec * baseLine.getZ();
            if (w == wThrow) {
                // define results:
                ha[0] = MINUS_PI;
                ha[1] = PI;
                return true;
            }
            // impossible case : sinDec = 1 <=> cosDec = 0 !
            return false;
        }
        final double coeff = (wThrow - sinDec * baseLine.getZ()) / cosDec;

        // A = (C + X)
        final double a = coeff + baseLine.getX();
        // B = (2 Y)
        final double b = 2d * baseLine.getY();
        // C = (C - X)
        final double c = coeff - baseLine.getX();

        return solveWEquation(a, b, c, ha);
    }

    /**
     * Find extrema for w(h)
     *
     * see astro_delayline.FIND_THROW_LIMS(D,X,Y,Z,H0,W0,N)
     * 
     * -----------------------------------------------------------------------
     *  we have to solve w=extrema, i.e. d(w)/dh=0 ,i.e.,
     *  -sin(H)*X-cos(H)*Y = 0
     *  if we put TAN(H/2)=t we have
     *  cos(H) --> (1-t**2)/(1+t**2)
     *  sin(H) --> (2*t)/(1+t**2)
     *  and the equation becomes:
     *  -(2*t)*X-(1-t**2)*Y=0 => -2X*t-Y+Yt**2=0
     *  a=+Y, b=-2X, c=-Y
     *  and H = 2 ATAN(t)
     * -----------------------------------------------------------------------
     * 
     * @param cosDec cosinus of target declination
     * @param sinDec sinus of target declination
     * @param baseLine used base line
     * @param w double[2] array to avoid array allocations
     * @return 2 W extrema or null
     */
    public static double[] findWExtrema(final double cosDec, final double sinDec, final BaseLine baseLine,
                                        final double[] w) {
        // A = Y
        final double a = baseLine.getY();
        // B = -2 X
        final double b = -2d * baseLine.getX();
        // C = - Y
        final double c = -baseLine.getY();

        // note: w contains ha solutions:
        if (solveWEquation(a, b, c, w)) {
            // 2 solutions :
            // w=cos(D)(cos(H)*X-sin(H)*Y)+sin(D)*Z

            w[0] = CalcUVW.computeW(cosDec, sinDec, baseLine, w[0]);
            w[1] = CalcUVW.computeW(cosDec, sinDec, baseLine, w[1]);

            // Ensure lower < higher:
            if (w[0] > w[1]) {
                // swap w values:
                final double w0 = w[0];
                w[0] = w[1];
                w[1] = w0;
            }
            return w;
        }

        return null;
    }

    /**
     * Returns the ha solutions (0, 1 or 2) for the second degree equation : a.t**2 + b.t + c = 0
     * t(1,2) = - Y/(C+X) +/- 1/(C+X) SQRT(Y**2+X**2-C**2)
     * and H = 2 ATAN(t)
     *
     * Note : ha solutions are in [-PI;PI] range
     *
     * see astro_delayline.SOLVE_2DEGR_UV_EQ(A,B,C,H0,N)
     *
     * @param a coefficient a
     * @param b coefficient b
     * @param c coefficient c
     * @param ha double[2] array to store ha solutions (rad) in [-PI;PI] range and avoid array allocations
     * @return true if ha solutions found; false otherwise
     * 
     */
    private static boolean solveWEquation(final double a, final double b, final double c, final double[] ha) {
        // output :
        if (a == 0d) {
            if (b == 0d) {
                // no solution :
                return false;
            }
            // 1 solution :
            ha[0] = FastMath.atan(-c / b);
            ha[1] = ha[0];
        } else {
            final double disc = b * b - 4d * a * c;
            if (disc <= 0d) {
                // no solution or only 1 point so reject it
                return false;
            }
            // 2 solutions:
            // note: sqrt uses hardware function (JIT):
            final double square = Math.sqrt(disc);

            // define solutions:
            final double two_a = 2d * a;

            ha[0] = MathUtils.getArgument(two_a, -b - square);
            ha[1] = MathUtils.getArgument(two_a, -b + square);
        }

        ha[0] *= 2d;
        ha[1] *= 2d;
        if (ha[0] > PI) {
            ha[0] -= TWO_PI;
        } else if (ha[0] < MINUS_PI) {
            ha[0] += TWO_PI;
        }
        if (ha[1] > PI) {
            ha[1] -= TWO_PI;
        } else if (ha[1] < MINUS_PI) {
            ha[1] += TWO_PI;
        }

        // arrange (useful for findWExtrema method) :
        if (ha[0] > ha[1]) {
            // swap ha values:
            final double ha0 = ha[0];
            ha[0] = ha[1];
            ha[1] = ha0;
        } else if (ha[0] == ha[1]) {
            ha[1] = ha[0] + PI;
        }

        return true;
    }
}
