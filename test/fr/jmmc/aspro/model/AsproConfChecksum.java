/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.jmcs.Bootstrapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class only provides checksum generation for the configuration files (in current classloader)
 * 
 * Requires: jMCS and aspro-conf modules
 * 
 * @author bourgesl
 */
public final class AsproConfChecksum {

    /** Class logger */
    private static final Logger _logger = LoggerFactory.getLogger(AsproConfChecksum.class.getName());

    /**
     * Generates MD5 checksum for configuration files
     * @param args unused
     */
    public static void main(final String[] args) {

        // invoke Bootstrapper method to initialize logback now:
        Bootstrapper.getState();

        _logger.info("\n\nUpdate checksums into AsproConfig file: AsproOIConfigurations.xml\n");

        // hard coded here to avoid using ConfigurationManager:
        final String[] files = new String[]{"VLTI.xml", "CHARA.xml", "SUSI.xml", "NPOI.xml", "DEMO.xml"};

        for (String fileName : files) {
            final long checksum = ConfigurationManager.checksum(fileName);
            
            if (checksum == 0l) {
                _logger.warn("checksum = 0 (should not happen): please modify your file to have a checksum <> 0 !");
            }

            _logger.info("File[{}]:\n<interferometerFile>\n    <file>{}</file>\n"
                    + "    <checksum>{}</checksum>\n</interferometerFile>\n", fileName, fileName, checksum);
        } // files
    }
}
