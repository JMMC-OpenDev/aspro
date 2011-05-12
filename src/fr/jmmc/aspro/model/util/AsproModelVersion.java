/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AsproModelVersion.java,v 1.2 2011-02-24 17:13:18 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/11/30 15:55:57  bourgesl
 * new model revision history as an enumeration
 *
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

/**
 * This enumeration associates a float value (YYYY.MM) to a version label
 * @author bourgesl
 */
public enum AsproModelVersion {

  /** initial revision */
  InitialRevision(2010.9f),
  /** december 2010 revision : target IDs and user informations (calibrators) */
  Dec2010Revision(2010.12f),
  /** february 2011 revision : added observation variants and observation collection */
  Feb2011Revision(2011.02f);

  /* members */
  /** version as a float value */
  private final float version;

  /**
   * Protected constructor
   * @param version as a float value
   */
  AsproModelVersion(final float version) {
    this.version = version;
  }

  /**
   * Return the version as a float value
   * @return version as a float value
   */
  public float getVersion() {
    return this.version;
  }

  /**
   * Return the AsproModelVersion corresponding to the given version.
   * It returns the initial revision if there is no matching revision
   * @param version version as a float value
   * @return AsproModelVersion
   */
  public static AsproModelVersion valueOf(final float version) {
    for (AsproModelVersion rev : AsproModelVersion.values()) {
      if (rev.getVersion() == version) {
        return rev;
      }
    }
    return InitialRevision;
  }
}
