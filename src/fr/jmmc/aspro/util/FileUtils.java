/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: FileUtils.java,v 1.1 2010-01-13 16:12:31 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.util;

import java.io.File;

/**
 * Several File utility methods
 * @author bourgesl
 */
public class FileUtils {

  /**
   * Forbidden constructor
   */
  private FileUtils() {
    // no-op
  }

  /**
   * Get the extension of a file.
   */
  public static String getExtension(final File file) {
    final String fileName = file.getName();
    final int i = fileName.lastIndexOf('.');

    if (i > 0 && i < fileName.length() - 1) {
      return fileName.substring(i + 1).toLowerCase();
    }
    return null;
  }

}
