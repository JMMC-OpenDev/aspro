/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.interop;

import fr.jmmc.aspro.model.TargetImporter;
import fr.jmmc.jmcs.service.XslTransform;
import java.io.IOException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class handles generic VOTables (targets) and PIVOT VOTables
 * 
 * @author bourgesl
 */
public final class AnyVOTableHandler {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(AnyVOTableHandler.class.getName());
    /** XSLT file path */
    private final static String XSLT_FILE = "fr/jmmc/aspro/interop/vot2AsproObservation.xsl";

    /**
     * Private constructor
     */
    private AnyVOTableHandler() {
        super();
    }

    /**
     * Process the given votable
     *
     * @param votable votable to process
     * @param forceAddTargets true to add targets only
     * 
     * @throws IOException if an I/O exception occured
     * @throws IllegalArgumentException if the file is not an Observation
     */
    public static void processVOTable(final String votable, final boolean forceAddTargets) throws IOException {
        // use an XSLT to transform the votable document to an Aspro 2 Observation:
        final long start = System.nanoTime();

        final String document = XslTransform.transform(votable, XSLT_FILE);

        logger.info("VOTable transformation (XSLT): {} ms.", 1e-6d * (System.nanoTime() - start));

        logger.debug("document:\n{}", document);

        try {
            TargetImporter.processObservation(document, forceAddTargets);
        } catch (IllegalArgumentException iae) {
            // Report both Observation and VOTable in a new IllegalArgumentException to get them in the feedback report:
            throw new IllegalArgumentException("Invalid generated Aspro2 Observation:\n\n" + document + "\n\nVOTable:\n\n" + votable, iae);
        }
    }

}
