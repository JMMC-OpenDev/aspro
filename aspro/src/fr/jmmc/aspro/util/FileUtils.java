/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: FileUtils.java,v 1.5 2010-06-17 10:02:51 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.4  2010/05/26 15:26:02  bourgesl
 * line separator is public
 *
 * Revision 1.3  2010/04/06 08:31:44  bourgesl
 * fixed classloader issue with JNLP
 *
 * Revision 1.2  2010/04/02 14:40:16  bourgesl
 * added writer methods for text files
 *
 * Revision 1.1  2010/01/13 16:12:31  bourgesl
 * added export to PDF button
 *
 */
package fr.jmmc.aspro.util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Writer;
import java.net.URL;

import java.util.logging.Level;

/**
 * Several File utility methods
 * @author bourgesl
 */
public final class FileUtils {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.util.FileUtils";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** platform dependent line separator */
  public static final String LINE_SEPARATOR = System.getProperty("line.separator");

  /**
   * Forbidden constructor
   */
  private FileUtils() {
    // no-op
  }

  /**
   * Get the extension of a file in lower case
   * @param file file to use
   * @return the extension of the file (without the dot char) or null
   */
  public static String getExtension(final File file) {
    final String fileName = file.getName();
    final int i = fileName.lastIndexOf('.');

    if (i > 0 && i < fileName.length() - 1) {
      return fileName.substring(i + 1).toLowerCase();
    }
    return null;
  }

  /**
   * Find a file in the current classloader (application class Loader)
   *
   * Accepts filename like fr/jmmc/aspro/fileName.ext
   *
   * @param fileName file name only no path included
   * @return URL to the file or null
   */
  public static URL getResource(final String fileName) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("getResource : " + fileName);
    }
    // Find properties in the classpath
    final URL url = FileUtils.class.getClassLoader().getResource(fileName);

    if (url == null) {
      throw new RuntimeException("Unable to find the file in classpath : " + fileName);
    }

    return url;
  }

  /**
   * Read the text file into a string
   * @param fileName file to load
   * @return text file content
   */
  public static String readFile(final String fileName) {

    String result = null;

    // buffer used for both script and result :
    final StringBuilder sb = new StringBuilder(512);

    InputStream inputStream = null;

    try {
      final URL url = getResource(fileName);
      inputStream = url.openStream();

      final BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream));

      // Read incoming data line by line
      String currentLine = null;

      while ((currentLine = bufferedReader.readLine()) != null) {
        if (sb.length() > 0) {
          sb.append(LINE_SEPARATOR);
        }

        sb.append(currentLine);
      }

      result = sb.toString();

    } catch (Exception ex) {
      logger.log(Level.SEVERE, "read failure.", ex);

    } finally {
      if (inputStream != null) {
        try {
          inputStream.close();
        } catch (IOException ex) {
          logger.log(Level.SEVERE, "close failure.", ex);
        }
      }
    }
    return result;
  }

  public static void writeFile(final File file, final String content) {
    final Writer w = openFile(file);
    if (w != null) {
      try {
        w.write(content);
      } catch (final IOException ioe) {
        logger.log(Level.SEVERE, "IO failure.", ioe);
      } finally {
        closeFile(w);
      }

    }
  }

  /**
   * Returns a Writer for the given file
   *
   * @param file file to write
   * @return Writer (buffered) or null
   */
  public static Writer openFile(final File file) {
    try {
      return new BufferedWriter(new FileWriter(file));
    } catch (final IOException ioe) {
      logger.log(Level.SEVERE, "IO failure.", ioe);
    }

    return null;
  }

  /**
   * Close the given writer
   *
   * @param w writer to close
   * @return null
   */
  public static Writer closeFile(final Writer w) {
    if (w != null) {
      try {
        w.close();
      } catch (final IOException ioe) {
        logger.log(Level.SEVERE, "IO close failure.", ioe);
      }
    }

    return null;
  }
}
