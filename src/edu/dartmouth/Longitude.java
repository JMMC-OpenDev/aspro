package edu.dartmouth;

import java.util.Locale;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */
public final class Longitude implements Cloneable {

  /** Longitude, +- 12 hours.  Includes some special hooks for
  interpreting inmput strings .*/
  double value;   // internally, hours west.
  Sexagesimal sex;

  static double adjlongit(double inval) {
    inval = inval % 24d;
    while (inval < -12d) {
      inval += 24d;
    }
    while (inval > 12d) {
      inval -= 24d;
    }
    return inval;
  }

  Longitude(final double x) {
    setFromDouble(x);
  }

  Longitude(final String s) {
    setFromString(s);
  }

  public Longitude clone() {
    try {
      final Longitude copy = (Longitude) super.clone();
      copy.sex = sex.clone();
      return copy;
    } catch (CloneNotSupportedException e) {
      throw new Error("This should never happen!");
    }
  }

  void setFromDouble(final double x) {
    value = adjlongit(x);  // force value to +- 12 h on input.
    sex = new Sexagesimal(value);
  }

  void setFromString(final String s) {
    String[] fields;
    String unitfield;
    String directionfield;
    boolean indegrees = false, positiveEast = false;

    int nf, i;

// THIS NEEDS WORK to avoid colliding with rules used in the
// interpretation of sexagesimals.  Need to test for letters in last
// couple of pieces of the string, then feed the Sexagesimal converter
// with only the remaining pieces.

    // check for a label at the end ...
    fields = s.split("\\s+");  // whitespace
    nf = fields.length;
    unitfield = fields[nf - 2].toLowerCase();
    // System.out.printf("last field %s%n",lastfield);
    i = unitfield.indexOf("h");
    if (i > -1) {
      indegrees = false;
      // System.out.println("h found ... ");
    }
    i = unitfield.indexOf("d");
    if (i > -1) {
      indegrees = true;
      // System.out.println("d found ... ");
    }
    directionfield = fields[nf - 1].toLowerCase();
    i = directionfield.indexOf("e");
    if (i > -1) {
      positiveEast = true;
      // System.out.println("e found ... ");
    }
    i = directionfield.indexOf("w");
    if (i > -1) {
      positiveEast = false;
      // System.out.println("w found ... ");
    }
    sex = new Sexagesimal(s);
    value = sex.value;
    if (indegrees) {
      value /= 15d;
    }
    if (positiveEast) {
      value *= -1d;
    }
    // System.out.printf("Value = %f%n",value);
    value = adjlongit(value);
    sex.tosex(value);
  }

  double radiansWest() {
    return (value* Const.RADIAN_IN_HRS);
  }

  double hoursWest() {
    return value;
  }

  double degreesEast() {
    return value * -15d;
  }

  public String roundedLongitString(final int ndigits, final String divider,
                                    final boolean inDegrees) {

    Sexagesimal outvalsex, rounded;
    double outval;
    int secfieldwidth;
    double decimalMinute;
    String raformat = "";
    String outstr = "";

    if (inDegrees) {
      outval = value * 15d;
    } else {
      outval = value;
    }

    outvalsex = new Sexagesimal(outval);
    rounded = outvalsex.roundsex(ndigits);

    // System.out.printf("rounded value is %f%n",rounded.value);

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

    if (inDegrees) {
      outstr += " D";
    } else {
      outstr += " H";
    }
    if (rounded.sign == 1) {
      outstr += " W";
    } else {
      outstr += " E";
    }

    return outstr;
  }
}
