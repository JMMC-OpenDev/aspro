/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package test;

import static fest.common.FestSwingCustomJUnitTestCase.getProjectFolderPath;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.RawObsManager;
import fr.jmmc.aspro.model.rawobs.Observations;
import fr.jmmc.aspro.model.rawobs.RawObservation;
import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.logging.LoggingService;
import java.io.File;
import java.io.FileReader;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Simple test on raw observations
 * @author bourgesl
 */
public class RawObservationTest {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(RawObservationTest.class.getName());

    /** absolute path to test folder to load test resources */
    protected final static String TEST_FOLDER = getProjectFolderPath() + "src/test/resources/obsportal/";

    /**
     * Private constructor
     */
    private RawObservationTest() {
        super();
    }

    /**
     * Simple GUI test
     * @param args unused
     */
    public static void main(String[] args) {
        // Set the default locale to en-US locale (for Numerical Fields "." ",")
        Locale.setDefault(Locale.US);

        // invoke App method to initialize logback now:
        Bootstrapper.getState();

        LoggingService.setLoggerLevel(logger, ch.qos.logback.classic.Level.DEBUG);

        try {
            if (true) {
                test(new File(TEST_FOLDER, "obsportal-output-10.xml"));
            }
            if (true) {
                test(new File(TEST_FOLDER, "obsportal-dev-spica.xml"));
            }
        } catch (Exception e) { // main (test)
            logger.error("runtime exception", e);
        }
    }

    private static void test(File input) throws Exception {
        logger.info("test file: {}", input);

        final ConfigurationManager cm = ConfigurationManager.getInstance();
        final RawObsManager rom = RawObsManager.getInstance();

        Observations obs = rom.loadRawObservations(new FileReader(input));

        /** shared context */
        final Map sharedContext = new HashMap(32);

        for (RawObservation o : obs.getObservations()) {
            logger.info("obs:\n{}", o);

            o.prepare(logger, sharedContext, cm);

            logger.info("obs valid: {}", o.getValid());
            logger.info("obs isValid(INVALID_META):  {}", o.isValid(RawObservation.INVALID_META));
            logger.info("obs isValid(INVALID_TIMES): {}", o.isValid(RawObservation.INVALID_TIMES));
            logger.info("obs isValid(INVALID_UV):    {}", o.isValid(RawObservation.INVALID_UV));
        }

        // RawObsManager.getInstance().analyze(getTargetRef(), getObservations()); ?
        logger.info("test done\n----------------");
    }
}
