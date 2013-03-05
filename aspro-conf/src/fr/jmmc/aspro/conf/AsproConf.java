/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.conf;

import fr.jmmc.jmcs.util.FileUtils;
import java.io.IOException;
import java.io.InputStream;

/**
 * This class only provides checksum generation a configuration as input stream
 * @author bourgesl
 */
public final class AsproConf {

    /**
     * Private constructor (utility class)
     */
    private AsproConf() {
        // no-op
    }

    /**
     * Internal: Computes the checksum for the given input stream
     * @param in input stream to process
     * @return checksum value as long integer
     * @throws IOException if any I/O error occurs.
     */
    public static long checksum(final InputStream in) throws IOException {
        long checksum = 1l;

        final byte[] bytes = FileUtils.checksum(in);

        for (byte b : bytes) {
            checksum *= (b & 0xFF);
        }

        return Math.abs(checksum);
    }
}
