/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.ObservationVariant;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.util.AsproModelVersion;
import java.util.logging.Level;

/**
 * This class handles Aspro model conversion between model revisions
 * @author bourgesl
 */
public final class ObservationFileProcessor {

  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(ObservationFileProcessor.class.getName());
  /** Current revision of the Aspro DM */
  public final static AsproModelVersion CURRENT_REVISION = AsproModelVersion.Feb2012Revision;

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

    // check and update target references :
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

    // check and update target references :
    observation.checkReferences();
  }

  /**
   * Convert the observation to the latest model
   * @param observation observation to convert
   * @param revision observation revision
   */
  private static void convertModel(final ObservationSetting observation, final AsproModelVersion revision) {
    if (logger.isLoggable(Level.INFO)) {
      logger.info("convert observation model from " + revision + " to " + CURRENT_REVISION);
    }

    if (revision.getVersion() < AsproModelVersion.Dec2010Revision.getVersion()) {
      // update model to Dec2010Revision :
      // force to generate target identifiers :
      String id;
      for (Target target : observation.getTargets()) {
        id = target.getIdentifier();
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("generate target ID '" + target.getName() + "' = '" + id + "'");
        }
      }
    }

    if (revision.getVersion() < AsproModelVersion.Feb2011Revision.getVersion()) {
      // update model to Feb2011Revision :

      // note : scvot2AsproObservation.xsl generates only a partial observation :
      if (observation.getInstrumentConfiguration() != null && observation.getVariants().isEmpty()) {
        // create a new variant having the same configuration (stations only) :
        final ObservationVariant obsVariant = new ObservationVariant();

        // Note : stations can not be null :
        obsVariant.setStations(observation.getInstrumentConfiguration().getStations());

        // create a new collection :
        observation.getVariants().add(obsVariant);
      }
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("convertModel done : " + revision);
    }
  }
}
