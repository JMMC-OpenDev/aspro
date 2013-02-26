package edu.dartmouth;

import java.util.Locale;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */
public class DEC implements Cloneable {

  /** declination.  */
  double value;    // decimal degrees
  Sexagesimal sex;

  // overloaded constructors are simply wrappers
  public DEC(final double inval) {
    value = inval;
    sex = new Sexagesimal(value);
  }

  public DEC(final String s) {
    sex = new Sexagesimal(s);
    value = sex.value;
  }
  // no good way to adjust out-of-range decs (it
  // doesn't wrap) so this is simpler than HA and RA

  public final void setDec(final double inval) {
    value = inval;
    sex.tosex(value);
  }

  public final void setDec(final String s) {
    sex.parseSexString(s);
    value = sex.value;
  }

  @Override
  public DEC clone() {
    try {
      final DEC copy = (DEC) super.clone();
      copy.sex = sex.clone();
      return copy;
    } catch (CloneNotSupportedException e) {
      throw new Error("This should never happen!");
    }
  }

  public final double radians() {
    return (value * Const.RADIAN_IN_DEG);
  }

  public final double degrees() {
    return (value);   // duuhh - but may be good to have
  }

  public final String roundedDecString(final int ndigits, final String divider) {
    // no need to wrap, so again simpler than HA and RA
    Sexagesimal rounded;
    int secfieldwidth;
    double decimalMinute;
    String decformat = "";
    String outstr = "";
    String signout = "+";

    rounded = sex.roundsex(ndigits);

    if (rounded.sign > 0) {
      signout = "+";
    } else {
      signout = "-";
    }
    if (ndigits >= 0) {
      if (ndigits == 0) {
        decformat =
        String.format(Locale.ENGLISH, "%s%%02d%s%%02d%s%%02.0f", signout, divider, divider);
      } else {
        secfieldwidth = ndigits + 3;
        decformat =
        String.format(Locale.ENGLISH, "%s%%02d%s%%02d%s%%0%1d.%1df", signout, divider, divider,
                secfieldwidth, ndigits);
      }
      outstr = String.format(Locale.ENGLISH, decformat, rounded.hour, rounded.minute, rounded.second);
    } else if (ndigits == -1) {
      decimalMinute = rounded.minute + rounded.second / 60.;
      outstr = String.format(Locale.ENGLISH, "%s%02d%s%04.1f", signout, rounded.hour, divider, decimalMinute);
    } else if (ndigits == -2) {
      outstr = String.format(Locale.ENGLISH, "%s%02d%s%02d", signout, rounded.hour, divider, rounded.minute);
    }
    return outstr;
  }
}
