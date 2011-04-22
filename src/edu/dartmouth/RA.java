package edu.dartmouth;

import java.util.Locale;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */

/* Now come the classes that handle celestial coordinates ...  */
public final class RA implements Cloneable {

  /** Right Ascension.  */
  double value;    // decimal hours
  Sexagesimal sex;

  static double adjra(double inval) {
    // System.out.printf("input inval %f ... ",inval);
    inval = inval % 24d;
    while (inval < 0d) {
      inval += 24d;
    }
    while (inval > 24d) {
      inval -= 24d;
    }
    // System.out.printf("returning inval = %f\n",inval);
    return inval;
  }

  // overloaded constructors are simply wrappers
  public RA(final double inval) {
    value = adjra(inval);
    sex = new Sexagesimal(value);
  }

  public RA(final String s) {
    sex = new Sexagesimal(s);
    value = adjra(sex.value);
    sex.tosex(value);
  }

  public void setRA(final double inval) {
    value = adjra(inval);
//      System.out.printf("Setting start %d %d %5.2f  %f  -> ",
//        sex.hour,sex.minute,sex.second,value);
    sex.tosex(value);
//      System.out.printf("%d %d %5.2f ... \n",sex.hour,sex.minute,sex.second);
  }

  public void setRA(final String s) {
    sex.parseSexString(s);
    value = adjra(sex.value);
    sex.tosex(value);
  }

  @Override
  public RA clone() {
    try {
      final RA copy = (RA) super.clone();
      copy.sex = sex.clone();
      return copy;
    } catch (CloneNotSupportedException e) {
      throw new Error("This should never happen!");
    }
  }

  public double radians() {
    return (value / Const.HRS_IN_RADIAN);
  }

  public double degrees() {
    return (value * 15d);
  }

  public String RoundedRAString(final int ndigits, final String divider) {

    /** Returns a rounded Sexagesimal RA, with the cut imposed at 24 h */
    Sexagesimal rounded;
    int secfieldwidth;
    double decimalMinute;
    String raformat = "";
    String outstr = "";

    rounded = sex.roundsex(ndigits);
    if (rounded.hour == 24) {
      rounded.hour = 0;
      rounded.minute = 0;
      rounded.second = 0d;
    }
    if (ndigits >= 0) {
      if (ndigits == 0) {
        raformat =
        String.format(Locale.ENGLISH, "%%02d%s%%02d%s%%02.0f", divider, divider);
      } else {
        secfieldwidth = ndigits + 3;
        raformat =
        String.format(Locale.ENGLISH, "%%02d%s%%02d%s%%0%1d.%1df", divider, divider,
                secfieldwidth, ndigits);
      }
      outstr = String.format(Locale.ENGLISH, raformat, rounded.hour, rounded.minute, rounded.second);
    } else if (ndigits == -1) {
      decimalMinute = rounded.minute + rounded.second / 60.;
      outstr = String.format(Locale.ENGLISH, "%02d%s%04.1f", rounded.hour, divider, decimalMinute);
    } else if (ndigits == -2) {
      outstr = String.format(Locale.ENGLISH, "%02d%s%02d", rounded.hour, divider, rounded.minute);
    }
    return outstr;
  }
}
