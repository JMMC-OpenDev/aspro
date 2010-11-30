/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservationFileProcessor.java,v 1.1 2010-11-30 15:52:13 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.util.AsproModelVersion;
import java.util.logging.Level;

/**
 * This class handles Aspro model conversion between model revisions
 * @author bourgesl
 */
public final class ObservationFileProcessor {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.model.util.ObservationFileProcessor";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** Current revision of the Aspro DM */
  public final static AsproModelVersion CURRENT_REVISION = AsproModelVersion.Dec2010Revision;

  /**
   * Forbidden constructor
   */
  private ObservationFileProcessor() {
    super();
  }

  /**
   * Perform the onLoad event : check the schema version and convert the observation if needed
   * @param observation observation to process
   */
  public static void onLoad(final ObservationSetting observation) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("onLoad : " + observation);
    }

    // Check version :
    final float SchemaVersion = observation.getSchemaVersion();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("SchemaVersion = " + SchemaVersion);
    }

    final AsproModelVersion revision = AsproModelVersion.valueOf(SchemaVersion);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("revision = " + revision);
    }

    if (revision != CURRENT_REVISION) {
      // model conversion is needed :
      convertModel(observation, revision);
    }
    // convert ?

    // check other things :
    observation.checkReferences();
  }

  /**
   * Perform the onSave event : set the schema version
   * @param observation observation to process
   */
  public static void onSave(final ObservationSetting observation) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("onSave : " + observation);
    }

    observation.setSchemaVersion(CURRENT_REVISION.getVersion());

    // check other things :
    observation.checkReferences();
  }

  /**
   * Convert the observation to the latest model
   * @param observation observation to convert
   * @param revision observation revision
   */
  private static void convertModel(final ObservationSetting observation, final AsproModelVersion revision) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("convertModel = " + revision);
    }

    if (revision == AsproModelVersion.InitialRevision) {
      // force to generate target identifiers :
      String id;
      for (Target target : observation.getTargets()) {
        id = target.getIdentifier();
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("generate target ID '" + target.getName() + "' = '" + id + "'");
        }
      }
    }
  }
}
