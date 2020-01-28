/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fest;

import fest.common.JmcsFestSwingJUnitTestCase;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.gui.SettingPanel;
import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.data.preference.CommonPreferences;
import fr.jmmc.jmcs.data.preference.PreferencesException;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.oiexplorer.core.gui.action.ExportDocumentAction;
import java.awt.Dimension;
import java.io.File;
import org.apache.commons.lang.SystemUtils;
import org.fest.swing.annotation.GUITest;
import org.fest.swing.fixture.ComponentFixture;
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
        try {
            Preferences.getInstance().setPreference(Preferences.MODEL_IMAGE_SIZE, NumberUtils.valueOf(1024));

            CommonPreferences.getInstance().setPreference(CommonPreferences.SHOW_STARTUP_SPLASHSCREEN, false);
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
        window.resizeTo(new Dimension(2000, 2000));
        
        window.textBox("starSearchField").setText("Simbad");

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();
    }

    /**
     * Capture the main panel
     */
    @Test
    @GUITest
    public void m10_LOW_LM_1Jy() {
        openObservationAndCaptureOIFitsSNRPlot("MATISSE_ETC/MATISSE_TEST_LM_LOW_6.16.asprox");
    }

    /* 
     --- Utility methods  ---------------------------------------------------------
     */
    private void openObservationAndCaptureOIFitsSNRPlot(final String obsFileName) {
        openObservation(obsFileName);

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();
        
        final JTabbedPaneFixture tab = selectTab(SettingPanel.TAB_OIFITS_VIEWER);
/*        
        final JPanelFixture form = window.panel("observationForm");

        // select CHARA interferometer :
        form.comboBox("jComboBoxInterferometer").selectItem("CHARA");
        
        tab.s
*/
        showPlotTab(SettingPanel.TAB_OIFITS_VIEWER, obsFileName + "-SNR.png");
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
