/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import java.awt.Shape;

/**
 * Simple Horizon profile holder to use the java shape API to check if a point is inside a polygon
 * @author bourgesl
 */
public final class HorizonShape {

  /** profile name */
  private final String name;
  /** internal shape */
  private final Shape shape;

  /**
   * Protected constructor
   * @param name profile name
   * @param shape shape (horizon)
   */
  public HorizonShape(final String name, final Shape shape) {
    this.name = name;
    this.shape = shape;
  }

  /**
   * Check if the given azimuth and elevation is inside the observable shape
   * @param az azimuth
   * @param elev elevation
   * @return true if the given coordinates are inside the observable shape
   */
  public boolean check(final double az, final double elev) {
    return this.shape.contains(az, elev);
  }

  /**
   * Return the profile name
   * @return profile name
   */
  public String getName() {
    return this.name;
  }
}
