/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package test;

import fr.jmmc.jmal.Band;

/**
 *
 * @author bourgesl
 */
public class MatisseBiasTest {

    public static void main(String[] args) {
        // MATISSE systematic visibility errpr:
        final double rel_e_vis_L = 3E-3;  // 0.3 %
        final double rel_e_vis_M = 3E-3;  // 0.3 %
        final double rel_e_vis_N = 2E-3;  // 0.2 %

        // MATISSE AT:
        final double e_phot_L = 0.11; // Jy
        final double e_phot_M = 0.19; // Jy
        final double e_phot_N = 2.3; // Jy

        testBias(Band.L, rel_e_vis_L, e_phot_L);
        testBias(Band.M, rel_e_vis_M, e_phot_M);
        testBias(Band.N, rel_e_vis_N, e_phot_N);
    }

    private static void testBias(final Band band, final double rel_e_vis, final double err_phot_tel_jy) {
        System.out.println("Test MATISSE bias  (" + band + ", zero_point = " + band.getZeroPoint() + ") err_phot_tel = " + err_phot_tel_jy + " Jy, err_vis = " + (100.0 * rel_e_vis) + " %");
        System.out.println("mag\tflux(Jy)\te_phot(%)\te_V2(%)\tSNR(V2)\te_V(%)\tSNR(V)");

        for (int mag = -2; mag <= 10; mag++) {
            final double flux_density = band.magToJy(mag); // Jy
            final double rel_e_phot = err_phot_tel_jy / flux_density; // depends on mag and telescope (UT/AT)

            // (dV2 / V2) = 2 (dv / v)
            final double rel_e_v2 = 2.0 * rel_e_vis; // depends on atmosphere quality
            final double rel_v2_error = Math.sqrt(rel_e_v2 * rel_e_v2 + 2.0 * rel_e_phot * rel_e_phot);
            final double rel_e_v = rel_v2_error / 2.0; // back

            System.out.println("" + mag
                    + "\t" + flux_density
                    + "\t" + (100.0 * rel_e_phot)
                    + "\t" + (100.0 * rel_v2_error)
                    + "\t" + (1.0 / rel_v2_error)
                    + "\t" + (100.0 * rel_e_v)
                    + "\t" + (1.0 / rel_e_v)
            );
        }
        System.out.println();
    }
}
