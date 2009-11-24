/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: DelayLineService.java,v 1.1 2009-11-24 15:12:09 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
*/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.model.BaseLine;
import fr.jmmc.aspro.model.Range;
import java.util.ArrayList;
import java.util.List;

/**
 * This class manages the computation to find the base line limits
 * @author bourgesl
 */
public class DelayLineService {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.service.DelayLineService";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /**
   * Return the intervals (hour angles) for all base lines compatible with wMin < w(h) < wMax given by wRanges
   *
   * see astro_delayline.DL_INTERVAL_LIST(...)
   *
   * @param dec target declination (rad)
   * @param baseLines base line list
   * @param wRanges [wMin - wMax] ranges per base line
   * @return intervals (hour angles)
   */
  public static List<Range> findHAIntervals(final double dec, final List<BaseLine> baseLines, final List<Range> wRanges) {
    // output :
    final List<Range> ranges = new ArrayList<Range>();

    // Get the current thread to check if the computation is interrupted :
    final Thread currentThread = Thread.currentThread();

    final int size = baseLines.size();

    List<List<Range>> rangesBL = new ArrayList<List<Range>>();

    BaseLine bl;
    Range wRange;
    for (int i = 0; i < size; i++) {
      bl = baseLines.get(i);
      wRange = wRanges.get(i);

      // fast interrupt :
      if (currentThread.isInterrupted()) {
        return null;
      }

      rangesBL.add(findHAIntervalsForBaseLine(dec, bl, wRange));
    }

    // Intersect all intervals :


    /*

    CALL DL_INTERVAL_LIST(TTT,THROW,KKK,RIGHTA(I),DECLIN(I),LATITUD,
    &  HORIZ(1),iTw,
    &  XXX,YYY,ZZZ,SNAM,HLONGLIST,NHLIST,MEMORY(IPWH),
    &  MEMORY(IPWHI),MEMORY(IPIWH))

    SUBROUTINE DL_INTERVAL_LIST(WMIN,WMAX,M,RA,D,LAT,Elev,iTw,
    &     X,Y,Z,SNAM,
    &     HLIST,N,
    &     WH,WHI,IWH)

     * -----------------------------------------------------------------------
     * Finds the intervals in hour angle where w(h) is .GE.WMIN AND .LE.WMAX
     * for ALL the WMIN and WMAX (and corresponding X Y & Z).
     * Result is a list of start-end times in HLIST
     * WH and IWH are work arrays of size 6*M
     * -----------------------------------------------------------------------
     */
    return ranges;
  }

  /**
   * -----------------------------------------------------------------------
   * Finds the interval in hour angle where w(h) is .GE.WMIN AND .LE.WMAX
   * -----------------------------------------------------------------------
   *
   * see astro_delayline.DELAYS_INTERVAL(WMIN,WMAX,D,X,Y,Z,HLIST,N)
   *
   * @param dec target declination (rad)
   * @param baseLine base line
   * @param wRange [wMin - wMax] range
   * @return intervals (hour angles)
   */
  private static List<Range> findHAIntervalsForBaseLine(final double dec, final BaseLine baseLine, final Range wRange) {
    logger.severe("baseLine : " + baseLine);
    logger.severe("W range  : " + wRange);

    // First check the W limits :
    final double[] wExtrema = findWExtrema(dec, baseLine);

    if (wExtrema == null) {
      // no solution :
      return null;
    }

    // test if [WMIN,WMAX] has intersection with MAXW[1-2] :
    final double w1 = Math.min(wExtrema[0], wExtrema[1]);
    final double w2 = Math.max(wExtrema[0], wExtrema[1]);

    logger.severe("W extrema = " + w1 + ", " + w2);

    final double wMin = wRange.getMin();
    final double wMax = wRange.getMax();
    if (wMax < w1 || wMin > w2) {
      // outside range, no solution :
      return null;
    }
    logger.severe("there are ha solutions ...");

    // output :
    final List<Range> ranges = new ArrayList<Range>();


    /*
    !
    N=0
    NHH=0
    SIND=SIN(D)
    COSD=COS(D)
    CALL FIND_THROW_LIMS(D,X,Y,Z,MAXH,MAXW,NMAXW)
    IF (NMAXW.NE.2) RETURN    !? no solution
    !     test if [WMIN,WMAX] has intersection with MAXW[1-2]
    W1=MIN(MAXW(1),MAXW(2))
    W2=MAX(MAXW(1),MAXW(2))
    !     outside range
    IF (WMAX.LT.W1.OR.WMIN.GT.W2) RETURN ! No solution
    !     create list of peculiar points:
    !     Since the 'no way' case is out, at least there is one solution:
    NHH=2                     !1 range
    HH(1)=-PI
    HH(2)=PI
    CALL SOLVE_DELAYS(WMIN,D,X,Y,Z,HMIN,NHMIN)
    DO I=1,NHMIN
    NHH=NHH+1
    HH(NHH)=HMIN(I)
    ENDDO
    CALL SOLVE_DELAYS(WMAX,D,X,Y,Z,HMAX,NHMAX)
    DO I=1,NHMAX
    NHH=NHH+1
    HH(NHH)=HMAX(I)
    ENDDO
    !     We have then the (at max 6) peculiar hour angles.
    !     Sort them by ascending order
    CALL GR8_TRIE(HH,IH,NHH,ERROR)
    IF(ERROR) THEN
    CALL GAGOUT ('Error in Delays_Interval (1) !')
    N=0
    RETURN
    ENDIF
    !     look if midpoints values are or not in the HMAX-HMIN interval.
    !     update accordingly the output array
    DO I=1,NHH-1
    !     test value of w in-between
    H=0.5*(HH(I)+HH(I+1))
    W = COSD*(-SIN(H)*Y+COS(H)*X)+SIND*Z
    IF (W.GE.WMIN.AND.W.LE.WMAX) THEN ! H interval is OK
    N=N+1               !another valid range
    HLIST(1,N)=HH(I)
    HLIST(2,N)=HH(I+1)
    ENDIF
    ENDDO
    RETURN
    END
     */

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
   * we have to solve w =throw, i.e. w-throw=0 ,i.e.,
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
   * @param dec target declination (rad)
   * @param baseLine used base line
   * @param wThrow w limit value (throw)
   * @return ha solutions (rad) in [-PI;PI] range (0 or 2)
   */
  private static double[] solveDelays(final double dec, final BaseLine baseLine, final double wThrow) {
    // output :
    double[] haValues = null;

    // w=cos(D)(cos(H)*X-sin(H)*Y)+sin(D)*Z
    final double cosDec = Math.cos(dec);
    final double sinDec = Math.sin(dec);

    if (cosDec == 0d) {
      final double w = sinDec * baseLine.getZ();
      if (w == wThrow) {
        haValues = new double[]{-Math.PI, Math.PI};
      } else {
        // impossible case : sinDec = 1 <=> cosDec = 0 !
        haValues = null;
      }
    } else {
      final double coeff = (wThrow - sinDec * baseLine.getZ()) / cosDec;

      // A = (C + X)
      final double a = coeff + baseLine.getX();
      // B = (2 Y)
      final double b = 2 * baseLine.getY();
      // C = (C - X)
      final double c = coeff - baseLine.getX();

      haValues = solveWEquation(a, b, c);
    }

    return haValues;
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
   * @param dec target declination (rad)
   * @param baseLine used base line
   * @return wMin - wMax couple or null
   */
  private static double[] findWExtrema(final double dec, final BaseLine baseLine) {
    // output :
    double[] wValues = null;

    // A = Y
    final double a = baseLine.getY();
    // B = (2 Y)
    final double b = - 2 * baseLine.getX();
    // C = (C - X)
    final double c = -baseLine.getY();

    final double[] haValues = solveWEquation(a, b, c);

    if (haValues != null) {
      // 2 solutions :
      // w=cos(D)(cos(H)*X-sin(H)*Y)+sin(D)*Z

      wValues = new double[2];

      for (int i = 0; i < 2; i++) {
        wValues[i] = computeW(dec, baseLine, haValues[i]);
      }

    }

    return wValues;
  }

  /**
   * Compute W(ha) = cos(D)(cos(H)*X-sin(H)*Y)+sin(D)*Z
   */
  public static double computeW(final double dec, final BaseLine baseLine, final double ha) {
    return computeW(Math.cos(dec), Math.sin(dec), baseLine, ha);
  }

  /**
   * Compute W(ha) = cos(D)(cos(H)*X-sin(H)*Y)+sin(D)*Z
   */
  public static double computeW(final double cosDec, final double sinDec, final BaseLine baseLine, final double ha) {
    return cosDec * (Math.cos(ha) * baseLine.getX() - Math.sin(ha) * baseLine.getY()) + sinDec * baseLine.getZ();
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
   * @return 2 ha solutions (rad) in [-PI;PI] range or null
   */
  private static double[] solveWEquation(final double a, final double b, final double c) {
    // output :
    double[] h0;

    if (a == 0d) {
      if (b == 0d) {
        // no solution :
        return null;
      }
      // 1 solution :
      final double ha0 = Math.atan(-c / b);
      h0 = new double[]{ha0, ha0};
    } else {
      final double disc = b * b - 4 * a * c;
      if (disc <= 0) {
        // no solution or only 1 point so reject it
        return null;
      }
      // 2 solutions :
      final double square = Math.sqrt(disc);
      h0 = new double[]{Math.atan2(-b - square, 2 * a), Math.atan2(-b + square, 2 * a)};
    }

    for (int i = 0; i < 2; i++) {
      h0[i] *= 2;
      if (h0[i] > Math.PI) {
        h0[i] -= 2 * Math.PI;
      } else if (h0[i] < -Math.PI) {
        h0[i] += 2 * Math.PI;
      }
    }
    // arrange (useful for findWExtrema method) :
    if (h0[0] > h0[1]) {
      final double h = h0[0];
      h0[0] = h0[1];
      h0[1] = h;
    } else if (h0[0] == h0[1]) {
      h0[1] = h0[0] + Math.PI;
    }

    return h0;
  }
}
