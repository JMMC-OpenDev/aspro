/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fest;

import fest.common.JmcsFestSwingJUnitTestCase;

import java.util.Calendar;
import java.util.GregorianCalendar;

import org.fest.swing.annotation.GUITest;
import org.fest.swing.fixture.JPanelFixture;
import org.fest.swing.fixture.JSpinnerFixture;
import org.junit.BeforeClass;

import org.junit.Test;

/**
 * This simple tests changes the observation date to test night bounds
 * 
 * @author bourgesl
 */
public final class AsproObservabilityTest extends JmcsFestSwingJUnitTestCase {

    /** name of the tab pane corresponding to the observability panel */
    private static final String TAB_OBSERVABILITY = "Observability";
    /** initial year */
    private static final int YEAR = 2014;
    /** number of days to traverse (1 years) */
    private static final int DAYS = 1 * 365;

    /**
     * Initialize system properties & static variables and finally starts the application
     */
    @BeforeClass
    public static void intializeAndStartApplication() {
        
        AsproDocJUnitTest.prepareTest();
        AsproDocJUnitTest.prepareAsproTest();

        // Start application:
        JmcsFestSwingJUnitTestCase.startApplication(
                fr.jmmc.aspro.Aspro2.class,
                "-open", TEST_FOLDER + "lazareff_largeprogramHaebe.asprox");
    }


    /**
     * Test if the application started correctly
     */
    public void initTest() {
        window.textBox("starSearchField").setText("Simbad");

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        // select tab :
        window.tabbedPane().selectTab(TAB_OBSERVABILITY);
    }

    /**
     * Test observability plot when updating the observation date
     */
    @Test
    @GUITest
    public void shouldChangeDateForVLTI() {

        initTest();

        final JPanelFixture form = window.panel("observationForm");

        // select VLTI interferometer :
//        form.comboBox("jComboBoxInterferometer").selectItem("VLTI");

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        final JSpinnerFixture dateSpinner = form.spinner("jDateSpinner");

        traverseDays(dateSpinner);
    }

    /**
     * Change the observation date starting from 01/01/1980 for n days where n = DAYS
     * @param dateSpinner spinner field to update
     */
    private void traverseDays(final JSpinnerFixture dateSpinner) {
        final int year = YEAR;
        final int month = 1;
        final int day = 1;

        final Calendar cal = new GregorianCalendar(year, month - 1, day);

        dateSpinner.select(cal.getTime());

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        for (int i = 0; i < DAYS; i++) {

            dateSpinner.increment();

            // waits for computation to finish :
            AsproTestUtils.checkRunningTasks();
        }
    }

    /* 
     --- Utility methods  ---------------------------------------------------------
     */
}
