/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fest;

import fest.common.JmcsFestSwingJUnitTestCase;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.gui.SettingPanel;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.data.preference.CommonPreferences;
import fr.jmmc.jmcs.data.preference.PreferencesException;
import fr.jmmc.jmcs.data.preference.SessionSettingsPreferences;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.oiexplorer.core.gui.action.ExportDocumentAction;
import java.awt.Dimension;
import static java.awt.event.KeyEvent.VK_F;
import java.io.File;
import org.apache.commons.lang.SystemUtils;
import org.fest.swing.annotation.GUITest;
import static org.fest.swing.core.KeyPressInfo.keyCode;
import org.fest.swing.fixture.JComboBoxFixture;
import org.fest.swing.fixture.JFileChooserFixture;
import org.fest.swing.fixture.JOptionPaneFixture;
import org.fest.swing.fixture.JPanelFixture;
import org.fest.swing.fixture.JSpinnerFixture;
import org.fest.swing.fixture.JTabbedPaneFixture;
import org.fest.swing.util.Platform;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

/**
 * This simple tests exports OIFITS files to test noisy random observables (+ caliper)
 * 
 * @author bourgesl
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class AsproNoiseTest extends JmcsFestSwingJUnitTestCase {

    /**
     * Initialize system properties & static variables
     */
    public static void prepareAsproTest() {
        // Hack to reset LAF & ui scale:
        CommonPreferences.getInstance().resetToDefaultPreferences();
        try {
            CommonPreferences.getInstance().setPreference(CommonPreferences.UI_SCALE, Double.valueOf(1.5));
        } catch (PreferencesException pe) {
            logger.error("setPreference failed", pe);
        }

        // invoke Bootstrapper method to initialize logback now:
        Bootstrapper.getState();

        // Test JDK 1.8+
        if (!SystemUtils.isJavaVersionAtLeast(1.8f)) {
            MessagePane.showErrorMessage("Please use a JVM 1.8 (Oracle) before running tests (fonts and LAF may be wrong) !");
            Bootstrapper.stopApp(1);
        }

        // reset window Preferences:
        new File(SystemUtils.USER_HOME + "/.fr.jmmc.jmcs.session_settings.jmmc.aspro2.properties").delete();

        // reset Preferences:
        Preferences.getInstance().resetToDefaultPreferences();
        SessionSettingsPreferences.getInstance().resetToDefaultPreferences();
        try {
            Preferences.getInstance().setPreference(Preferences.MODEL_IMAGE_SIZE, NumberUtils.valueOf(1024));

            // do add noise:
            Preferences.getInstance().setPreference(Preferences.OIFITS_ADD_NOISE, Boolean.TRUE);

            CommonPreferences.getInstance().setPreference(CommonPreferences.SHOW_STARTUP_SPLASHSCREEN, Boolean.FALSE);
            CommonPreferences.getInstance().setPreference(CommonPreferences.FILECHOOSER_NATIVE, Boolean.FALSE);
        } catch (PreferencesException pe) {
            logger.error("setPreference failed", pe);
        }

        // define robot delays :
        defineRobotDelayBetweenEvents(SHORT_DELAY);

        // define delay before taking screenshot :
        defineScreenshotDelay(SHORT_DELAY);

        // disable tooltips :
        enableTooltips(false);

        // customize Export action to avoid use StatusBar :
        ExportDocumentAction.setAvoidUseStatusBar(true);
    }

    /**
     * Initialize system properties & static variables and finally starts the application
     */
    @BeforeClass
    public static void intializeAndStartApplication() {

        AsproDocJUnitTest.prepareTest();
        AsproNoiseTest.prepareAsproTest();

        // Start application:
        JmcsFestSwingJUnitTestCase.startApplication(
                fr.jmmc.aspro.Aspro2.class);
    }

    /**
     * Test if the application started correctly
     */
    @Test
    @GUITest
    public void m01_shouldStart() {
        // fix frame size:
        App.getFrame().setPreferredSize(new Dimension(1400, 1600));

        // let defered update:
        pauseMedium();

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        window.textBox("starSearchField").setText("Simbad");

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();
    }

    @Test
    @GUITest
    public void m10_MATISSE_N_LOW_SNR() {
        openObservationAndCaptureOIFitsFiles("TEST_UD_20mas_MAT_N_0");
    }

    /* 
     --- Utility methods  ---------------------------------------------------------
     */
    private void openObservationAndCaptureOIFitsFiles(final String obsFileName) {
        openObservation("noise/" + obsFileName + ".asprox");

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        selectTab(SettingPanel.TAB_OIFITS_VIEWER);

        final JPanelFixture oifitsView = window.panel("plotView");

        final JComboBoxFixture comboPlotDef = oifitsView.comboBox("plotDefinitionComboBox");
        final String[] plotDefNames = comboPlotDef.contents();
        // System.out.println("plotDefNames: " + Arrays.toString(plotDefNames));

        // select preset Residuals:
        comboPlotDef.selectItem(plotDefNames.length - 3); // last-3 corresponds to Residuals

        final JPanelFixture mainForm = window.panel("observationForm");
        final JSpinnerFixture dateSpinner = mainForm.spinner("jDateSpinner");

        final JPanelFixture plot = window.panel("plotChartPanel");

        int i = 1;
        do {
            // Capture plot screenshot :
            saveScreenshot(plot, obsFileName + "-SNR_" + i + ".png");

            // Export OIFits :
            exportOIFits(obsFileName + "_" + i + ".oifits");

            dateSpinner.increment();
            dateSpinner.decrement();

            // waits for computation to finish :
            AsproTestUtils.checkRunningTasks();

            // let plot update:
            pauseShort();

        } while (i++ <= 10);
    }

    private void openObservation(final String obsFileName) {
        // hack to solve focus trouble in menu items :
        window.menuItemWithPath("File").focus();
        window.menuItemWithPath("File", "Open observation").click();

        confirmDialogDontSave();

        window.fileChooser().selectFile(new File(TEST_FOLDER + obsFileName)).approve();
    }

    /**
     * Export OIFits file using default file name
     */
    private void exportOIFits(final String fileName) {
        // export OIFits :
        window.pressAndReleaseKey(keyCode(VK_F).modifiers(Platform.controlOrCommandMask()));

        // close warning on OIFITS message:
        closeOIFitsMessage();

        // use image folder to store OIFits and validate :
        final JFileChooserFixture fileChooser = window.fileChooser().setCurrentDirectory(getScreenshotFolder());
        fileChooser.fileNameTextBox().setText(fileName);
        fileChooser.approve();

        // overwrite any existing file :
        confirmDialogFileOverwrite();
    }

    /**
     * Close any option pane by clicking Yes
     * @return true if a message was closed
     */
    protected boolean closeOIFitsMessage() {
        try {
            // if a message appears :
            final JOptionPaneFixture optionPane = window.optionPane();

            if (optionPane != null) {
                // click Yes :
                optionPane.yesButton().click();

                return true;
            }

        } catch (RuntimeException re) {
            // happens when the confirm message does not occur :
            logger.debug("lookup failure : ", re);
        }
        return false;
    }

    /**
     * Show the plot tab of the given name and export PDF using default file name
     * @param tabName tab name
     * @return JTabbedPaneFixture
     */
    private JTabbedPaneFixture selectTab(final String tabName) {
        return getMainTabbedPane().selectTab(tabName);
    }

    /**
     * Return the main Aspro tabbed pane
     * @return Aspro tabbed pane
     */
    private JTabbedPaneFixture getMainTabbedPane() {
        return window.tabbedPane("AsproTab");
    }

}
