/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.util;

import java.util.logging.Level;

/**
 * This class provides several utility methods dedicated to tests and development.
 * @author bourgesl
 */
public final class TestUtils {

  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          TestUtils.class.getName());

  /**
   * Forbidden constructor : utility class
   */
  private TestUtils() {
    // no-op
  }

  /**
   * Causes the currently executing thread to wait but not sleep (stay busy)
   *
   * @param milliseconds milliseconds to wait
   */
  public static void busyWait(final long milliseconds) {
    long now;

    final long start = System.nanoTime();

    final long future = start + 1000000l * milliseconds;

    do {
      now = System.nanoTime();

      if (Thread.currentThread().isInterrupted()) {
        break;
      }

    } while (now <= future);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("busyWait : " + 1e-6d * (now - start) + " ms.");
    }
  }
}
