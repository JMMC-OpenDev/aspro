package edu.dartmouth;

import java.util.Locale;
import java.util.Scanner;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
modify it for their own purposes, provided that credit is given to the author
in a prominent place.  For the present program that means that the green
title and author banner appearing on the main window must not be removed,
and may not be altered without premission of the author. */
public final class Sexagesimal implements Cloneable {

  int sign;
  int hour, minute;
  double second;
  double value;

  // Overloaded constructor takes either a string or a double argument.
  // little public classes wrap the ones that do all the work.
  public Sexagesimal(final String s) {
    parseSexString(s);
  }

  public Sexagesimal(final double v) {
    tosex(v);
    value = v;
  }

  @Override
  public Sexagesimal clone() {
    try {
      return (Sexagesimal) super.clone();
    } catch (CloneNotSupportedException e) {
      throw new Error("This should never happen!");
    }
  }

  void tosex(double h) {  // decimal hour converter ...
    // System.out.printf("h = %f%n",h);
    if (h >= 0d) {
      sign = 1;
    } else {
      sign = -1;
    }
    h *= sign;
    hour = (int) h;
    final double m = 60d * (h - (double) hour);
    minute = (int) m;
    second = 60d * (m - (double) minute);
    // System.out.printf("sgn h m s %d  %d %d %f%n",sign,hour,minute,second);
  }

  void parseSexString(final String s) {
    // converts a string, either colon or space separated, into a
    // Sexagesimal. "minus-zero" problem is handled.
    final String[] fields;
    if (s.contains(":")) {
      fields = s.split(":");      // colon-delimited
    } else {
      fields = s.split("\\s+");  // whitespace
    }
    if (fields.length == 1) {  // a string, but just a single number
      value = Double.parseDouble(fields[0]);
      tosex(value);
    } else {   // it's a triplet (or possibly doublet)
      // for (String ss : fields) {
      // System.out.printf("%s ... ",ss);  // colon-delimited
      // }
      if (fields[0].contains("-")) {
        sign = -1;
      } else {
        sign = 1;
      }
      fields[0] = fields[0].replace("+", "");  // parseInt chokes on explicit "+"
      hour = Integer.parseInt(fields[0]);
      if (hour < 0) {
        hour *= -1;   // positive definite
      }
      try {
        minute = Integer.parseInt(fields[1]);
      } catch (NumberFormatException e) { // decimal minute input
        final double doubleMinute = Double.parseDouble(fields[1]);
        minute = (int) doubleMinute;
        second = 60d * (doubleMinute - (double) minute);
      }
      if (fields.length > 2) {  // seconds are there ...
        second = Double.parseDouble(fields[2]);
      }
    }
    // System.out.printf("%d  %d %d %f%n",sign,hour,minute,second);
    value = (double) hour + minute / 60d + second / 3600d;
    value *= sign;
    // System.out.printf("value: %f%n",value);
  }

  public Sexagesimal roundsex(final int ndigits) {
    // returns a Sexagesimal rounded off to ndigits
    // so that seconds and minutes are never 60.  Higher overflows
    // (e.g. 24 00 00 for a time of day) will be handled elsewhere.

    String teststr = "";
    String testformat = "";
    int hourback, minuteback;
    double secondback;
    int secondswidth;
    int tenthMinutes;
    double decimalMinute, decimalMinuteback;

    if (ndigits >= 0) {  // including seconds ...
      if (ndigits == 0) {
        testformat =
        String.format(Locale.ENGLISH, "%%d %%d %%02.0f");
      }
      if (ndigits > 0) {
        secondswidth = 3 + ndigits;
        testformat = String.format(Locale.ENGLISH, "%%d %%d %%0%1d.%1df",
                secondswidth, ndigits);
        // System.out.println(testformat);
      }

      // System.out.printf("In roundsex, testformat = %s%n",testformat);
      teststr = String.format(Locale.ENGLISH, testformat, hour, minute, second);
      Scanner readback = new Scanner(teststr).useDelimiter("\\s");
      readback.useLocale(Locale.ENGLISH);
      // read back the result ...
      // System.out.printf("In roundsex, teststr = %s%n",teststr);
      hourback = readback.nextInt();
      if (hourback < 0d) {
        hourback *= -1d;
      }
      minuteback = readback.nextInt();
      secondback = readback.nextDouble();
      // System.out.printf("read back: %d %d %f%n",hourback,minuteback,secondback);
      if (secondback > 59.999999999) {  // klugy, but should be very safe
        secondback = 0d;
        minuteback++;
        if (minuteback == 60) {
          minuteback = 0;
          hourback++; // overflows to 24, etc need to be handled
          // at the next level
        }
        teststr = String.format(Locale.ENGLISH, testformat, hourback, minuteback, secondback);
      }
    } else {  // -1 -> tenths of a minute, -2 -> whole minutes
      decimalMinute = minute + second / 60.;
      if (ndigits == -1) {
        teststr = String.format(Locale.ENGLISH, "%d %4.1f", hour, decimalMinute);
      }
      if (ndigits <= -2) {
        teststr = String.format(Locale.ENGLISH, "%d %02.0f", hour, decimalMinute);
      }
      Scanner readback = new Scanner(teststr);
      readback.useLocale(Locale.ENGLISH);
      hourback = readback.nextInt();
      decimalMinuteback = readback.nextDouble();
      if (decimalMinuteback > 59.99) {  // limited precision - this will be safe
        decimalMinuteback = 0.00001;
        hourback++;
      }
      minuteback = (int) decimalMinuteback;
      if (ndigits == -1) {
        tenthMinutes = (int) ((10. * (decimalMinuteback - minuteback) + 0.0001));
        teststr = String.format(Locale.ENGLISH, "%d %02d.%1d", hourback, minuteback, tenthMinutes);
      } else {
        teststr = String.format(Locale.ENGLISH, "%d %02d", hourback, minuteback);
      }
    }
    Sexagesimal roundedsex = new Sexagesimal(teststr);
    roundedsex.sign = sign;
    return roundedsex;
  }
}
