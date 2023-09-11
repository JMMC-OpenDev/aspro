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
import java.io.File;
import org.apache.commons.lang.SystemUtils;
import org.fest.swing.annotation.GUITest;
import org.fest.swing.fixture.ComponentFixture;
import org.fest.swing.fixture.JComboBoxFixture;
import org.fest.swing.fixture.JPanelFixture;
import org.fest.swing.fixture.JTabbedPaneFixture;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder; 
import org.junit.Test;
import org.junit.runners.MethodSorters;

/**
 * This simple tests takes screenshots to complete the Aspro2 / MATISSE-ETC report
 * 
 * @author bourgesl
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class AsproMatisseTest extends JmcsFestSwingJUnitTestCase {

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

            // do not add noise:
            Preferences.getInstance().setPreference(Preferences.OIFITS_ADD_NOISE, Boolean.FALSE);

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

        AsproMatisseTest.prepareAsproTest();

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
    public void m10_LOW_LM() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_LM_LOW_6.16.asprox");
    }

    @Test
    @GUITest
    public void m11_LOW_LM__FT() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_LM_LOW_6.16_FT.asprox");
    }

    @Test
    @GUITest
    public void m20_LOW_LM() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_LM_LOW_2.45.asprox");
    }

    @Test
    @GUITest
    public void m21_LOW_LM_FT() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_LM_LOW_2.45_FT.asprox");
    }

    @Test
    @GUITest
    public void m30_MED_LM() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_LM_MED_2.45.asprox");
    }

    @Test
    @GUITest
    public void m31_MED_LM_FT() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_LM_MED_2.45_FT.asprox");
    }

    @Test
    @GUITest
    public void m40_MED_LM() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_LM_MED_1.15.asprox");
    }

    @Test
    @GUITest
    public void m41_MED_LM_FT() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_LM_MED_1.15_FT.asprox");
    }

    @Test
    @GUITest
    public void m42_HIGH_L() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_L_HIGH_2.45.asprox");
    }

    @Test
    @GUITest
    public void m43_VERY_HIGH_L_FT() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_L_VERY_HIGH_2.9_FT.asprox");
    }

    @Test
    @GUITest
    public void m44_VERY_HIGH_M_FT() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_M_VERY_HIGH_2.3_FT.asprox");
    }

    @Test
    @GUITest
    public void m50_LOW_N() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_N_LOW_0.7.asprox");
    }

    @Test
    @GUITest
    public void m51_LOW_N_FT() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_N_LOW_0.7_FT.asprox");
    }

    @Test
    @GUITest
    public void m60_LOW_N() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_N_LOW_-1.asprox");
    }

    @Test
    @GUITest
    public void m61_LOW_N_FT() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_N_LOW_-1_FT.asprox");
    }

    @Test
    @GUITest
    public void m70_HIGH_N() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_N_HIGH_0.7.asprox");
    }

    @Test
    @GUITest
    public void m71_HIGH_N_FT() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_N_HIGH_0.7_FT.asprox");
    }

    @Test
    @GUITest
    public void m80_HIGH_N() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_N_HIGH_-1.asprox");
    }

    @Test
    @GUITest
    public void m81_HIGH_N_FT() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_N_HIGH_-1_FT.asprox");
    }

    /* 
     --- Utility methods  ---------------------------------------------------------
     */
    private void openObservationAndCaptureOIFitsSNRPlot(final String obsFileName) {
        openObservation(obsFileName);

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        selectTab(SettingPanel.TAB_OIFITS_VIEWER);

        final JPanelFixture form = window.panel("plotView");

        final JComboBoxFixture comboPlotDef = form.comboBox("plotDefinitionComboBox");
        final String[] plotDefNames = comboPlotDef.contents();

        // select preset 'V2_T3_SNR/EFF_WAVE':
        comboPlotDef.selectItem(plotDefNames.length - 1); // last corresponds to 'V2_T3_SNR/EFF_WAVE'

        // let plot update:
        pauseMedium();

        // Capture plot screenshot :
        final JPanelFixture plot = window.panel("plotChartPanel");
        saveScreenshot(plot, obsFileName + "-SNR.png");
        
        // select preset 'V_V2_T3_ERR/EFF_WAVE':
        comboPlotDef.selectItem(plotDefNames.length - 2); // last corresponds to 'V2_T3_SNR/EFF_WAVE'

        // let plot update:
        pauseMedium();

        // Capture plot screenshot :
        saveScreenshot(plot, obsFileName + "-ERR.png");
    }

    private void openObservation(final String obsFileName) {
        // hack to solve focus trouble in menu items :
        window.menuItemWithPath("File").focus();
        window.menuItemWithPath("File", "Open observation").click();

        confirmDialogDontSave();

        window.fileChooser().selectFile(new File(TEST_FOLDER + obsFileName)).approve();
    }

    /**
     * Show the plot tab of the given name and capture the window screenshot
     * @param tabName tab name
     * @param fileName file name of the window screenshot
     */
    private void showPlotTab(final String tabName, final String fileName) {
        // select tab :
        final ComponentFixture<?> com = selectTab(tabName);

        // Capture window screenshot :
        saveScreenshot(com, fileName);
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
