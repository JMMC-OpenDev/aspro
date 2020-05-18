/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.ObservationVariant;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.util.AsproModelVersion;
import fr.jmmc.aspro.model.util.TargetUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class handles Aspro model conversion between model revisions
 * @author bourgesl
 */
public final class ObservationFileProcessor {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(ObservationFileProcessor.class.getName());
    /** Current revision of the Aspro DM */
    public final static AsproModelVersion CURRENT_REVISION = AsproModelVersion.Apr2018Revision;

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
        logger.debug("onLoad: {}", observation);

        // Check version :
        final float schemaVersion = observation.getSchemaVersion();

        logger.debug("SchemaVersion = {}", schemaVersion);

        final AsproModelVersion revision = AsproModelVersion.valueOf(schemaVersion);

        logger.debug("revision = {}", revision);

        // convert ?
        if (revision != CURRENT_REVISION) {
            // model conversion is needed :
            convertModel(observation, revision);
        }

        // check and update target references in TargetUserInformations :
        observation.checkReferences();
    }

    /**
     * Perform the onSave event : set the schema version
     * @param observation observation to process
     */
    public static void onSave(final ObservationSetting observation) {
        logger.debug("onSave: {}", observation);

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
        logger.info("convert observation model from {} to {}", revision, CURRENT_REVISION);

        if (revision.getVersion() < AsproModelVersion.Dec2010Revision.getVersion()) {
            // update model to Dec2010Revision :
            // force to generate target identifiers :
            String id;
            for (Target target : observation.getTargets()) {
                id = target.getIdentifier();
                logger.debug("generate target ID '{}' = '{}'", target.getName(), id);
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

        if (revision.getVersion() < AsproModelVersion.Feb2012Revision.getVersion()) {
            // update model to Feb2012Revision :

            // define the target.useAnalyticalModel = true :
            for (Target target : observation.getTargets()) {
                target.setUseAnalyticalModel(Boolean.TRUE);
            }
        }

        logger.debug("convertModel done : {}", revision);
    }
}
