package edu.dartmouth;

import java.util.Locale;


/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */

/* The Java calendar API is pretty nasty, and the DST rules in
particular are buried very deeply.  The application I have in
mind requires me to have explicit control over these rules in the
past and the future.  Given that I've already
written my own date handlers in other languages, I'm thinking
it may be be easier and more robust for me to just go ahead and
reproduce that functionality from scratch.  */
/** GenericCalDat - handles conversions back and forth to JD, and
formatting and rounding of dates.  Rounding of times is handled in the
Sexagesimal class, and then continued rounding of dates is handled
here.  GenericCalDat is blind to whether the date represented is 
UT or local - that's handled by InstantInTime. */
public final class GenericCalDat implements Cloneable {

  final static String[] months = {"", "January", "February", "March", "April",
    "May", "June", "July", "August", "September", "October", "November",
    "December"};
  final static String monthtest = "janfebmaraprmayjunjulaugsepoctnovdec";
  final static String[] dayname = {"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"};

  /* Members */
  int year, month, day;
  Sexagesimal timeofday;

  // Polymorphism! Overload constructor class to
  // accept either a string or a double.
  public GenericCalDat(final String s) {
    // "yyyy mm dd hh mm ss" ... or JD as character string.
    calFromString(s);
  }
  // overload the constructor method to take a JD number.

  public GenericCalDat(final double jdin) {
    calFromJD(jdin);
  }

  @Override
  public GenericCalDat clone() {
    try {
      final GenericCalDat copy = (GenericCalDat) super.clone();
      copy.timeofday = timeofday.clone();
      return copy;
    } catch (CloneNotSupportedException e) {
      throw new Error("This should never happen!");
    }
  }

  void calFromJD(final double jd) {
    /* sets the calendar date using the current values of jd -- can
    be either a local or UT date */

    /* Adapted from J. Meeus,  Astronomical Formulae for Calculators,
    published by Willman-Bell Inc.
    Avoids a copyrighted routine from Numerical Recipes.
    Tested and works properly from the beginning of the
    calendar era (1583) to beyond 3000 AD. */

    double tmp;
    long alpha;
    long Z, A, B, C, D, E;
    double F;
    boolean rounded_ok = false;

    double jdin = jd;

    while (!rounded_ok) {
      tmp = jdin + 0.5d;
      Z = (long) tmp;

      F = tmp - Z;
      if (Z < 2299161d) {
        A = Z;
      } else {
        alpha = (long) ((Z - 1867216.25d) / 36524.25d);
        A = Z + 1l + alpha - (long) (alpha / 4d);
      }

      B = A + 1524l;
      C = (long) ((B - 122.1d) / 365.25d);
      D = (long) (365.25d * C);
      E = (long) ((B - D) / 30.6001d);

      day = (int) (B - D - (long) (30.6001d * E));
      if (E < 13.5d) {
        month = (int) (E - 1l);
      } else {
        month = (int) (E - 13l);
      }
      if (month > 2.5d) {
        year = (int) (C - 4716l);
      } else {
        year = (int) (C - 4715l);
      }
      timeofday = new Sexagesimal(24d * F);

      if (timeofday.hour == 24) {
        jdin += 1.0e-7d; // near to the resolution of the double
      } else {
        rounded_ok = true;
      }
    }
  }

  void calFromString(final String s) {
    // "yyyy mm dd hh mm ss" ... or just a string "JD".
    // "yyyy mm dd" or "yyyy mm dd hh mm" work ok too.
    // "2006 Jul 14" works, as does "2006 July 14 18:00"
    final String[] fields = s.split("\\s+");   // whitespace

    if (fields.length == 1) { // presumably only a JD ...
      final double jdin = Double.parseDouble(fields[0]);
      calFromJD(jdin);
      return;
    }

    year = Integer.parseInt(fields[0]);
    try {
      month = Integer.parseInt(fields[1]);
    } catch (NumberFormatException e) {
      // System.out.println("Catching exception .. ");
      String ss = fields[1].toLowerCase();
      ss = ss.substring(0, 3);
      int ind = monthtest.indexOf(ss);
      if (ind > -1) {
        month = ((ind / 3) + 1);
        // System.out.printf("ss %s ind %d month %d%n",ss,ind,month);
      } else {
        // System.out.println("Undecipherable month, set to 1000.\n");
        month = 1000;
      }
    }
    day = Integer.parseInt(fields[2]);
    if (fields.length == 4) { // colon-sep time included
      timeofday = new Sexagesimal(fields[3]);
    }
    if (fields.length == 5) {
      timeofday = new Sexagesimal(String.format(Locale.ENGLISH, "%s:%s", fields[3], fields[4]));
    }
    if (fields.length > 5) {
      timeofday =
              new Sexagesimal(String.format(Locale.ENGLISH, "%s:%s:%s", fields[3], fields[4], fields[5]));
    }
  }

  double cal2JD() {

    // System.out.printf("%d %d %d  %d %d %f%n",
    // year,month,day,timeofday.hour,timeofday.minute,timeofday.second);

    final int y;
    final int m;
    if (month <= 2) {
      y = year - 1;
      m = month + 12;
    } else {
      y = year;
      m = month;
    }

    final long A = (long) ((y + 0.00000001d) / 100d);
    final long B = 2l - A + (long) ((A + 0.000000001d) / 4d);

    final double jdout = (long) (365.25d * (double) y) + (long) (30.6001d * (m + 1)) + day
            + 1720994.5d
            + (double) timeofday.hour / 24d + (double) timeofday.minute / 1440d
            + timeofday.second / 86400d;
    //System.out.println(jd);

    if (year > 1583) {
      return (jdout + (double) B);
    }
    return jdout;
  }

  int dayOfWeek() {
    /** Adapted straight from skycalc, returns 0=Mon throuh 6=Sun **/
    int d;

    final double jd = cal2JD() + 0.5d;
    final long i = (long) jd;
    final double x = i / 7d + 0.01d;
    return (int) (7d * (x - (long) x));
  }

  void quickprint() {
    System.out.printf("%d %02d %02d  %02d %02d %f",
            year, month, day, timeofday.hour, timeofday.minute, timeofday.second);
    System.out.printf(" -> %s%n", dayname[dayOfWeek()]);
  }

  String roundedCalString(final int style, int digits) {
    /** Returns a descriptive string; rather than writing a flexible
    format I'll code a number of options.  Much sturm und drang here
    because of the need to round the day along with everything else.
    These styles follow cooclasses.py; I'll write more as needed. :
    
    style 0 -> 2006 8 12  10 11 12
    style 1 -> 2005 Aug 12  10 11
    style 2 -> Fri Aug 12  10:11
    style 3 -> 2005 Aug 12
    style 4 -> Fri Aug 12
    style 5 -> 2005 6 12
    style 6 -> 2006 Aug 12 Tue
    style 7 -> Fri 2006 Aug 12  10:11
    
    style 10 -> (time only) 10 11 12.0
    style 11 -> (time only) 10:11:12.0
    style 12 -> (time only) 10:11
    
    These are included here to force correct rounding when printing
    only the time.
    
     */
    String result;
    double jdtemp;
    int printYear, printMonth, printDay;
    int printHour, printMinute;
    double printSecond;
    int printDOW;


    if (style == 0) {
      digits = 0;
    }
    if (style == 1 | style == 2 | style == 12) {
      digits = -2;
    }

    Sexagesimal rounded = timeofday.roundsex(digits);
    // round the date upward ...
    if (rounded.hour == 24) {
      // System.out.println("Oops, gotta round day upward.\n");
      jdtemp = cal2JD() + 0.4; // this will always round upward
      // and never screw the day of week
      GenericCalDat tempcal = new GenericCalDat(jdtemp);
      printYear = tempcal.year;
      printMonth = tempcal.month;
      printDay = tempcal.day;
      printHour = 0;
      printMinute = 0;
      printSecond = 0d;
      printDOW = tempcal.dayOfWeek();
    } else {
      printYear = year;
      printMonth = month;
      printDay = day;
      printHour = rounded.hour;
      printMinute = rounded.minute;
      printSecond = rounded.second;
      printDOW = dayOfWeek();
    }
    String monthAbr = months[printMonth].substring(0, 3);
    switch (style) {
      case 0:
        // System.out.println("*** 0 ***");
        result = String.format(Locale.ENGLISH, "%4d %02d %02d  %02d %02d %02.0f",
                printYear, printMonth, printDay, printHour, printMinute,
                printSecond);
        break;
      case 1:
        // System.out.println("*** 1 ***");
        result = String.format(Locale.ENGLISH, "%4d %s %02d  %02d %02d",
                printYear, monthAbr, printDay, printHour, printMinute);
        break;
      case 2:
        // System.out.println("*** 2 ***");
        result = String.format(Locale.ENGLISH, "%s %s %02d  %02d:%02d",
                dayname[printDOW], monthAbr, printDay, printHour, printMinute);
        break;
      case 3:
        // System.out.println("*** 3 ***");
        result = String.format(Locale.ENGLISH, "%4d %s %02d", printYear, monthAbr,
                printDay);
        break;
      case 4:
        // System.out.println("*** 4 ***");
        result = String.format(Locale.ENGLISH, "%s %s %02d", dayname[printDOW],
                monthAbr, printDay);
        break;
      case 5:
        // System.out.println("*** 5 ***");
        result = String.format(Locale.ENGLISH, "%4d %02d %02d", printYear, printMonth,
                printDay);
        break;
      case 6:
        // System.out.println("*** 6 ***");
        result = String.format(Locale.ENGLISH, "%4d %s %02d  %s", printYear, monthAbr,
                printDay, dayname[printDOW]);
        break;
      case 7:
        // System.out.println("*** 2 ***");
        result = String.format(Locale.ENGLISH, "%s  %4d %s %02d  %02d:%02d",
                dayname[printDOW], printYear, monthAbr, printDay, printHour, printMinute);
        break;

      case 11:
        // System.out.println("*** 11 ***");
        result = String.format(Locale.ENGLISH, "%02d %02d %04.1f", printHour, printMinute,
                printSecond);
        break;
      case 12:
        result = String.format(Locale.ENGLISH, "%02d:%02d", printHour, printMinute);
        break;
      default:
        // System.out.println("*** Default ***");
        result = String.format(Locale.ENGLISH, "%4d %s %02d  %02d:%02d:%04.1f",
                printYear, monthAbr, printDay, printHour, printMinute, printSecond);
    }
    return result;
  }
}
