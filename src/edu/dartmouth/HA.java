package edu.dartmouth;

import java.util.Locale;


/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */
public final class HA implements Cloneable {

  /** Hour angle, nearly the same as RA, but force -12 to +12.  */
  double value;    // decimal hours
  Sexagesimal sex;

  static double adjha(double inval) {
    inval = inval % 24d;
    while (inval < -12d) {
      inval += 24d;
    }
    while (inval > 12d) {
      inval -= 24d;
    }
    return inval;
  }

  // overloaded constructors are simply wrappers
  public HA(final double inval) {
    setHA(inval);
  }

  public HA(final String s) {
    setHA(s);
  }

  public void setHA(final double inval) {
    value = adjha(inval);
    sex = new Sexagesimal(value);
  }

  public void setHA(final String s) {
    sex = new Sexagesimal(s);
    value = adjha(sex.value);
    sex.tosex(value);
  }

  @Override
  public HA clone() {
    try {
      final HA copy = (HA) super.clone();
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

  public String RoundedHAString(final int ndigits, final String divider) {
    /** Sexagesimal dec string, with the cut at +- 12 hr */
    Sexagesimal rounded;
    int secfieldwidth;
    double decimalMinute;
    String haformat = "";
    String outstr = "";
    String signout = "+";

    rounded = sex.roundsex(ndigits);
    if (rounded.hour == 12 & rounded.sign == -1) {
      rounded.hour = 12;
      rounded.minute = 0;
      rounded.second = 0d;
      rounded.sign = 1;
    }
    // System.out.printf("rounded.sign = %d\n",rounded.sign);
    if (rounded.sign > 0) {
      signout = "+";
    } else {
      signout = "-";
    }
    if (ndigits >= 0) {
      if (ndigits == 0) {
        haformat =
        String.format(Locale.ENGLISH, "%s%%d%s%%02d%s%%02.0f", signout, divider, divider);
      } else {
        secfieldwidth = ndigits + 3;
        haformat =
        String.format(Locale.ENGLISH, "%s%%d%s%%02d%s%%0%1d.%1df", signout, divider, divider,
                secfieldwidth, ndigits);
      }
      outstr = String.format(Locale.ENGLISH, haformat, rounded.hour, rounded.minute, rounded.second);
    } else if (ndigits == -1) {
      decimalMinute = rounded.minute + rounded.second / 60.;
      outstr = String.format(Locale.ENGLISH, "%s%d%s%04.1f", signout, rounded.hour, divider, decimalMinute);
    } else if (ndigits == -2) {
      outstr = String.format(Locale.ENGLISH, "%s%d%s%02d", signout, rounded.hour, divider, rounded.minute);
    }

    return outstr;
  }
}
