/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.jmcs.util.ResourceUtils;
import fr.jmmc.oitools.fits.FitsUtils;
import fr.nom.tam.fits.BasicHDU;
import fr.nom.tam.fits.BinaryTableHDU;
import fr.nom.tam.fits.Fits;
import fr.nom.tam.fits.FitsException;
import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Arrays;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author bourgesl
 */
public final class AtmosphereSpectrumService {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(AtmosphereSpectrumService.class.getName());
    /** classloader path to configuration files */
    public static final String CONF_CLASSLOADER_PATH = "fr/jmmc/aspro/model/";

    /** Fits file name for mean atmospheric conditions */
    public static final String ATM_MEAN = "skytable_mean_atm.fits";

    private static final boolean DEBUG = false;
    private static final boolean DUMP_VALUES = false;
    private static final boolean MOCK = false;

    /** FITS column lambda 'lam' (micrometer) */
    private static final String COL_LAMBDA = "lam";
    /** FITS column transmission 'trans' (micrometer) */
    private static final String COL_TRANSMISSION = "trans";

    private final static AtmosphereSpectrumService instance = new AtmosphereSpectrumService();

    public static AtmosphereSpectrumService getInstance() {
        return instance;
    }

    /* members */
    private AtmTransmission cached = null;

    private AtmosphereSpectrumService() {
        super();

        init();
    }

    public double[] getTransmission(final double[] waveLengths, final double[] waveBands) {
        final int nWLen = waveLengths.length;

        final double[] trans = new double[nWLen];

        if (MOCK) {
            Arrays.fill(trans, 1d);
            return trans;
        }

        // TODO: add several transmission curves for paranal !
        final AtmTransmission atm = this.cached;

        for (int i = 0; i < nWLen; i++) {
            final double lambdaMin = waveLengths[i] - 0.5 * waveBands[i];
            final double lambdaMax = waveLengths[i] + 0.5 * waveBands[i];

            final int first = findIndex(atm.lambda, lambdaMin, true);
            final int last = findIndex(atm.lambda, lambdaMax, false);

            if (logger.isDebugEnabled()) {
                logger.debug("lambdaMin: {} vs lambda(first): {}", lambdaMin, atm.lambda[first]);
                logger.debug("lambdaMax: {} vs lambda(last) : {}", lambdaMax, atm.lambda[last]);
            }

            if (first < last) {
                // simple mean:
                // should perform convolution of gaussian filter on each spectral channels (varying width):
                double total = 0.0;

                for (int j = first; j <= last; j++) {
                    total += atm.transmission[j]; // x weight !
                }

                trans[i] = total / (last + 1 - first);

            } else {
                // Lower = Upper index side case:
                // only happen if lambda < min(atm.lambda) or lambda > max(atm.lambda)
                trans[i] = atm.transmission[first];
            }
        }

        return trans;
    }

    private static int findIndex(final double[] array, final double value, final boolean smaller) {
        int idx = Arrays.binarySearch(array, value);
        if (idx < 0) {
            // insertion point = index of the first element greater than the key (>= 0)
            idx = -idx - 1;
            // adjust to get the index of the first element smaller than the key
            if (smaller) {
                idx--;
            }
        }

        // Range check:
        if (idx <= 0) {
            return 0; // inclusive
        }
        if (idx >= array.length) {
            return array.length - 1; // inclusive
        }
        return idx;
    }

    private void init() {
        final BinaryTableHDU table = loadFits(ATM_MEAN);

        try {
            final double[] lambda = (double[]) table.getColumn(COL_LAMBDA);
            final double[] trans = (double[]) table.getColumn(COL_TRANSMISSION);

            // convert lambda to meters:
            for (int i = 0; i < lambda.length; i++) {
                lambda[i] *= AsproConstants.MICRO_METER;
            }

            cached = new AtmTransmission(lambda, trans);

        } catch (FitsException fe) {
            throw new IllegalStateException("Missing columns", fe);
        }
    }

    private final BinaryTableHDU loadFits(final String uri)
            throws IllegalStateException, IllegalArgumentException {

        InputStream is = null;
        try {
            // use the class loader resource resolver
            final URL url = ResourceUtils.getResource(CONF_CLASSLOADER_PATH + uri);

            logger.info("AtmosphereSpectrumService.loadFits: {}", url);

            is = new BufferedInputStream(url.openStream());

            final Fits fits = new Fits(is);

            final BasicHDU[] hdus = fits.read();

            final int len = hdus.length;

            if (len != 2) {
                throw new IllegalStateException("Invalid format: 1 binary table expected !");
            }

            if (DEBUG) {
                BasicHDU hdu;
                for (int i = 0; i < len; i++) {
                    hdu = hdus[i];

                    // dump HDU:
                    logger.info("hdu[{}]:\n{}", i, FitsUtils.dumpHDU(hdu, DUMP_VALUES));
                }
            }

            return (BinaryTableHDU) hdus[1];

        } catch (IOException ioe) {
            throw new IllegalStateException("Load failure on " + uri, ioe);
        } catch (FitsException fe) {
            throw new IllegalStateException("Load failure on " + uri, fe);
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (IOException ioe) {
                    logger.warn("Closing Fits file:", ioe);
                }
            }
        }
    }

    static final class AtmTransmission {

        /** lambda in meters */
        final double[] lambda;
        /** transmission [0;1] */
        final double[] transmission;

        AtmTransmission(final double[] lambda, final double[] transmission) {
            this.lambda = lambda;
            this.transmission = transmission;
        }
    }

    public static void main(String[] args) {
        final AtmosphereSpectrumService atmService = AtmosphereSpectrumService.getInstance();

        final double[] lambda = new double[]{1.4e-6, 1.5e-6, 1.8e-6};
        final double[] delta_lambda = new double[]{1e-7, 1e-7, 2e-6};

        final double[] trans = atmService.getTransmission(lambda, delta_lambda);

        logger.info("trans: " + Arrays.toString(trans));
    }
}
