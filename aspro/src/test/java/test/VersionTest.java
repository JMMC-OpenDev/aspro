/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package test;

import static fr.jmmc.aspro.model.ObservationManager.TARGET_VERSION_MUST_UPDATE;
import java.util.Calendar;

/**
 *
 */
public class VersionTest {

    /**
     * Test
     * @param args unused
     */
    public static void main(String[] args) {
        System.out.println("TARGET_VERSION_MUST_UPDATE: " + TARGET_VERSION_MUST_UPDATE);

        final Calendar cal = Calendar.getInstance();

        float version = cal.get(Calendar.YEAR) + (1f + cal.get(Calendar.MONTH)) / 100f;
        System.out.println("version: " + version);

        // See checkTargetVersion():
        boolean valid = (version - TARGET_VERSION_MUST_UPDATE) >= -0.01f;
        System.out.println("valid: " + valid);

        version = 2019.06f;
        System.out.println("version: " + version);
        valid = (version - TARGET_VERSION_MUST_UPDATE) >= -0.01f;
        System.out.println("valid: " + valid);
        
        version = 2019.05f;
        System.out.println("version: " + version);
        valid = (version - TARGET_VERSION_MUST_UPDATE) >= -0.01f;
        System.out.println("valid: " + valid);
    }
}
