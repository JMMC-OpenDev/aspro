/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fest;

import fest.common.JmcsFestSwingJUnitTestCase;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.gui.SettingPanel;
import fr.jmmc.aspro.gui.TargetEditorDialog;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.data.preference.CommonPreferences;
import fr.jmmc.jmcs.data.preference.PreferencesException;
import fr.jmmc.jmcs.data.preference.SessionSettingsPreferences;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.oiexplorer.core.gui.action.ExportDocumentAction;
import java.awt.Dimension;
import java.io.File;
import javax.swing.JFormattedTextField;
import org.apache.commons.lang.SystemUtils;
import org.fest.swing.annotation.GUITest;
import static org.fest.swing.core.matcher.DialogMatcher.withTitle;
import org.fest.swing.core.matcher.JButtonMatcher;
import org.fest.swing.edt.GuiActionRunner;
import org.fest.swing.edt.GuiTask;
import org.fest.swing.fixture.DialogFixture;
import org.fest.swing.fixture.JComboBoxFixture;
import org.fest.swing.fixture.JPanelFixture;
import org.fest.swing.fixture.JTabbedPaneFixture;
import org.fest.swing.fixture.JTextComponentFixture;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

/**
 * This simple tests tests GRAVITY noise model to compare Aspro2 and GRAVITY+ report
 * 
 * @author bourgesl
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class AsproGravityTest extends JmcsFestSwingJUnitTestCase {

    /**
     * Initialize system properties & static variables
     */
    public static void prepareAsproTest() {
        System.setProperty("TRACE_SNR_FT", "true");
        
        // Hack to reset LAF & ui scale:
        CommonPreferences.getInstance().resetToDefaultPreferences();
        try {
            CommonPreferences.getInstance().setPreference(CommonPreferences.UI_SCALE, Double.valueOf(1.0));
        } catch (PreferencesException pe) {
            logger.error("setPreference failed", pe);
        }

        // invoke Bootstrapper method to initialize logback now:
        Bootstrapper.getState();

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

        AsproGravityTest.prepareAsproTest();

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
        App.getFrame().setPreferredSize(new Dimension(1900, 1000));

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
    public void testGravityFT_00_UT() {
        System.out.println("[TRACE]\tVLTI UT");
        openObservationAndMakeSNRTests("GRAV_sample_FT_SCI_1uv_UT_GPAO.asprox");
    }
/*
    @Test
    @GUITest
    public void testGravityFT_01_UT() {
        System.out.println("[TRACE]\tVLTI UT");
        openObservationAndMakeSNRTests("GRAV_sample_FT_SCI_1uv_UT.asprox");
    }

    @Test
    @GUITest
    public void testGravityFT_10_AT() {
        System.out.println("[TRACE]\tVLTI AT");
        openObservationAndMakeSNRTests("GRAV_sample_FT_SCI_1uv_AT.asprox");
    }
*/
    /* 
     --- Utility methods  ---------------------------------------------------------
     */
    private void openObservationAndMakeSNRTests(final String obsFileName) {
        openObservation(obsFileName);

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        selectTab(SettingPanel.TAB_OIFITS_VIEWER);

        final JPanelFixture form = window.panel("plotView");

        final JComboBoxFixture comboPlotDef = form.comboBox("plotDefinitionComboBox");
        final String[] plotDefNames = comboPlotDef.contents();

        // select preset 'V2_T3_SNR/EFF_WAVE':
        comboPlotDef.selectItem(plotDefNames.length - 1); // last corresponds to 'V2_T3_SNR/EFF_WAVE'

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        // Capture plot screenshot :
        final JPanelFixture plot = window.panel("plotChartPanel");

        final double step = 0.5;

        for (double k = 0.0; k <= 13.0; k += step) {
            changeMagK(k);

            // let plot update:
            pauseMedium();

            saveScreenshot(plot, obsFileName + "-K_" + k + "-SNR.png");
        }
    }

    private void changeMagK(double magK) {
        window.button("jButtonTargetEditor").click();

        final DialogFixture dialog = window.dialog(withTitle("Target Editor").andShowing());

        dialog.requireVisible();
        dialog.moveToFront();

        dialog.tabbedPane().selectTab(TargetEditorDialog.TAB_TARGETS);

        // FT target:
        dialog.tree().selectPath("Targets/Sirius A");

        // magnitude K:
        JTextComponentFixture jFieldFixture = dialog.textBox("FLUXK");
        jFieldFixture.click();
        {
            final JFormattedTextField jTextMagK = (JFormattedTextField) jFieldFixture.component();

            GuiActionRunner.execute(new GuiTask() {
                @Override
                protected void executeInEDT() {
                    // Integer field :
                    jTextMagK.setValue(Double.valueOf(magK));
                }
            });
        }
        jFieldFixture = dialog.textBox("FLUXV"); // bug mag edit
        jFieldFixture.click();

        // SCI target:
        dialog.tree().selectPath("Targets/Sirius B");

        // close dialog :
        dialog.button(JButtonMatcher.withText("OK")).click();

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();
    }

    private void openObservation(final String obsFileName) {
        // hack to solve focus trouble in menu items :
        window.menuItemWithPath("File").focus();
        window.menuItemWithPath("File", "Open observation").click();

        confirmDialogDontSave();

        window.fileChooser().selectFile(new File(TEST_FOLDER + obsFileName)).approve();
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
