/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fest;

import com.mortennobel.imagescaling.AdvancedResizeOp;
import com.mortennobel.imagescaling.ResampleOp;
import fest.common.JmcsFestSwingJUnitTestCase;
import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.gui.SettingPanel;
import fr.jmmc.aspro.gui.TargetEditorDialog;
import fr.jmmc.aspro.gui.TargetGroupForm;
import fr.jmmc.aspro.gui.util.WindWidget;
import fr.jmmc.jmal.image.ColorModels;
import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.data.preference.CommonPreferences;
import fr.jmmc.jmcs.data.preference.PreferencesException;
import fr.jmmc.jmcs.data.preference.SessionSettingsPreferences;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.oiexplorer.core.gui.action.ExportDocumentAction;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.DisplayMode;
import java.awt.Frame;
import java.awt.Point;
import static java.awt.event.KeyEvent.*;
import java.awt.image.BufferedImage;
import java.io.File;
import javax.swing.JComponent;
import javax.swing.JFormattedTextField;
import javax.swing.JList;
import org.apache.commons.lang.SystemUtils;
import org.fest.swing.annotation.GUITest;
import org.fest.swing.core.GenericTypeMatcher;
import static org.fest.swing.core.KeyPressInfo.*;
import org.fest.swing.core.matcher.DialogMatcher;
import static org.fest.swing.core.matcher.DialogMatcher.*;
import org.fest.swing.core.matcher.FrameMatcher;
import org.fest.swing.core.matcher.JButtonMatcher;
import org.fest.swing.core.matcher.JTextComponentMatcher;
import org.fest.swing.edt.GuiActionRunner;
import org.fest.swing.edt.GuiTask;
import org.fest.swing.fixture.DialogFixture;
import org.fest.swing.fixture.FrameFixture;
import org.fest.swing.fixture.JOptionPaneFixture;
import org.fest.swing.fixture.JPanelFixture;
import org.fest.swing.fixture.JTabbedPaneFixture;
import org.fest.swing.fixture.JTextComponentFixture;
import org.fest.swing.util.Platform;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

/**
 * This simple tests takes screenshots to complete the Aspro2 documentation
 * 
 * @author bourgesl
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public final class AsproDocJUnitTest extends JmcsFestSwingJUnitTestCase {

    private final static String FAKE_EMAIL = "FAKE_EMAIL";
    private static String CURRENT_EMAIL = "";

    /** robot take screenshot (top offset for gnome3 on x11) */
    private static final int M_TOP = 0; // 28 (breeze), 14 (gnome3), 0 now at font scale = 1.0 */

    private static void defineEmailPref(final String email) {
        try {
            final CommonPreferences prefs = CommonPreferences.getInstance();

            CURRENT_EMAIL = prefs.getPreference(CommonPreferences.FEEDBACK_REPORT_USER_EMAIL);

            prefs.setPreference(CommonPreferences.FEEDBACK_REPORT_USER_EMAIL, email);
        } catch (PreferencesException pe) {
            logger.error("setPreference failed", pe);
        }
    }

    /**
     * Initialize system properties & static variables
     */
    public static void prepareAsproTest() {
        prepareAsproTest(Double.valueOf(1.5));
    }

    /**
     * Initialize system properties & static variables
     */
    public static void prepareAsproTest(Double uiScale) {
        // Hack to reset LAF & ui scale:
        CommonPreferences.getInstance().resetToDefaultPreferences();
        try {
            CommonPreferences.getInstance().setPreference(CommonPreferences.UI_SCALE, uiScale);
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
        defineEmailPref(FAKE_EMAIL);

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
     * Initialize FEST test
     */
    public static void prepareTest() {
        // GNOME3: window issue:
        setScreenshotTakerMargins(M_TOP, 0, 0, 0);
        logger.warn("Using top margin = {} px", M_TOP);

        System.setProperty("DISABLE_DEFERED_COMPUTE", "true");
    }

    /**
     * Initialize system properties & static variables and finally starts the application
     */
    @BeforeClass
    public static void intializeAndStartApplication() {
        logger.info("AsproDocJUnitTest.intializeAndStartApplication()");

        AsproDocJUnitTest.prepareTest();
        AsproDocJUnitTest.prepareAsproTest(Double.valueOf(1.0));

        // Start application:
        JmcsFestSwingJUnitTestCase.startApplication(
                fr.jmmc.aspro.Aspro2.class,
                "-open", TEST_FOLDER + "Aspro2_sample.asprox");
    }

    /**
     * Test if the application started correctly
     */
    @Test
    @GUITest
    public void m001_shouldStart() {
        window.textBox("starSearchField").setText("Simbad");

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        // Capture initial state :
        showPlotTab(SettingPanel.TAB_INTERFEROMETER_MAP, "Aspro2-map.png");
    }

    /**
     * Capture the main panel
     */
    @Test
    @GUITest
    public void m002_captureMain() {
        captureMainForm("Aspro2-main.png");
    }

    /**
     * Capture the main panel with wind compass enabled
     */
    @Test
    @GUITest
    public void m003_captureWindCompass() {
        // enable wind restriction:
        window.checkBox("jCheckBoxWind").check();

        final WindWidget windWidget = robot().finder().findByName("WindWidget", WindWidget.class);

        GuiActionRunner.execute(new GuiTask() {
            @Override
            protected void executeInEDT() {
                windWidget.setValue(60d);
            }
        });

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        // Capture observability plot :
        getMainTabbedPane().selectTab(SettingPanel.TAB_OBSERVABILITY);
        saveScreenshot(window, "Aspro2-wind-obs.png");

        final JPanelFixture panel = window.panel("observabilityPanel");

        // enable scroll view (needed for scroller below):
        panel.checkBox("jCheckBoxScrollView").check();
        // enable detailed plot :
        panel.checkBox("jCheckBoxDetailedOutput").check();

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        panel.scrollBar("scroller").scrollToMaximum(); // see HD 1234

        // Capture observability plot of detailed plot :
        saveScreenshot(getMainTabbedPane(), "Aspro2-wind-obs-det.png");

        // disable detailed plot :
        panel.checkBox("jCheckBoxDetailedOutput").uncheck();
        // disable scroll view :
        panel.checkBox("jCheckBoxScrollView").uncheck();

        // disable wind restriction:
        GuiActionRunner.execute(new GuiTask() {
            @Override
            protected void executeInEDT() {
                windWidget.setValue(0d);
            }
        });
        window.checkBox("jCheckBoxWind").uncheck();

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();
    }

    /**
     * Capture the JSkyCalc windows
     */
    @Test
    @GUITest
    public void m004_captureJSkyCalc() {
        window.button("jButtonSkyCalc").click();

        final FrameFixture skyCalcWin = getFrame("JSkyCalc");

        // Capture SkyCalcWindow screenshot :
        saveScreenshot(skyCalcWin, "Aspro2-SkyCalcWindow.png");

        final FrameFixture skyDispWin = getFrame("Sky Display");

        // Capture SkyDisplay screenshot :
        saveScreenshot(skyDispWin, "Aspro2-SkyDisplay.png");

        // close all JSkyCalc windows:
        skyCalcWin.close();
    }

    /**
     * Test observability plot
     */
    @Test
    @GUITest
    public void m005_shouldShowObservability() {

        // Capture observability plot :
        getMainTabbedPane().selectTab(SettingPanel.TAB_OBSERVABILITY);

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        try {
            enableTooltips(true);

            // move the mouse on the first observability interval (top right corner):
            robot().moveMouse(window.component(), 380, 425 + M_TOP); // TODO: check position

            // let tooltip appear:
            pauseMedium();

            saveScreenshotWithMousePointer(window, "Aspro2-obs.png");
        } finally {
            enableTooltips(false);
        }

        // export PDF :
        exportPDF();

        final JPanelFixture panel = window.panel("observabilityPanel");

        // enable baseline limits :
        panel.checkBox("jCheckBoxBaseLineLimits").check();

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        // Capture observability plot of  baseline limits :
        saveScreenshot(getMainTabbedPane(), "Aspro2-obs-bl.png");

        // disable baseline limits :
        panel.checkBox("jCheckBoxBaseLineLimits").uncheck();

        // enable detailed plot :
        panel.checkBox("jCheckBoxDetailedOutput").check();

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        // Capture observability plot of detailed plot :
        saveScreenshot(getMainTabbedPane(), "Aspro2-obs-det.png");

        // disable detailed plot :
        panel.checkBox("jCheckBoxDetailedOutput").uncheck();

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();
    }

    /**
     * Test UV coverage plot
     */
    @Test
    @GUITest
    public void m006_shouldShowUVCoverage() {

        // Capture UV Coverage plot :
        getMainTabbedPane().selectTab(SettingPanel.TAB_UV_COVERAGE);

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        final BufferedImage image = takeScreenshotOf(window);
        saveImage(image, "Aspro2-screen.png");

        // TODO : refactor that code :
        // miniature for aspro web page : 350px width :
        final int width = 350;
        final int height = Math.round(1f * width * image.getHeight() / image.getWidth());

        // use Lanczos3 resampler and soft unsharp mask :
        final ResampleOp resampleOp = new ResampleOp(width, height);
        resampleOp.setUnsharpenMask(AdvancedResizeOp.UnsharpenMask.Soft);

        final BufferedImage rescaledImage = resampleOp.filter(image, null);
        saveImage(rescaledImage, "Aspro2-screen-small.png");

        // export PDF :
        exportPDF();

        try {
            enableTooltips(true);

            // move the mouse on one uv measurement:
            robot().moveMouse(window.component(), 690, 580 + M_TOP); // TODO: check position

            // let tooltip appear:
            pauseMedium();

            saveScreenshotWithMousePointer(window, "Aspro2-uv.png");

        } finally {
            enableTooltips(false);
        }

        // Export OIFits / OB :
        exportOIFits();
        exportOB();
    }

    /**
     * Test OIFits viewer plot
     */
    @Test
    @GUITest
    public void m007_shouldShowVis2Plot() {

        window.list("jListTargets").selectItem("HIP1234");

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        // Capture OIFits viewer plot :
        showPlotTab(SettingPanel.TAB_OIFITS_VIEWER, "Aspro2-vis2-noErr.png");

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        window.list("jListTargets").selectItem("HD 1234");

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        // Capture OIFits viewer plot :
        showPlotTab(SettingPanel.TAB_OIFITS_VIEWER, "Aspro2-vis2-withErr.png");

        window.list("jListTargets").selectItem("HIP1234");

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        // select UV tab like before OIFits viewer:
        getMainTabbedPane().selectTab(SettingPanel.TAB_UV_COVERAGE);
    }

    /**
     * Test Target editor
     */
    @Test
    @GUITest
    public void m008_shouldOpenTargetEditor() {

        window.button("jButtonTargetEditor").click();

        final DialogFixture dialog = window.dialog(withTitle("Target Editor").andShowing());

        dialog.requireVisible();
        dialog.moveToFront();

        dialog.tabbedPane().selectTab(TargetEditorDialog.TAB_MODELS);

        dialog.tree().selectPath("Models/HIP1234/elong_disk1");

        saveScreenshot(dialog, "Aspro2-Model.png");

        dialog.tabbedPane().selectTab(TargetEditorDialog.TAB_TARGETS);

        dialog.tree().selectPath("Targets/HD 1234");

        saveScreenshot(dialog, "Aspro2-Target.png");

        // close dialog :
        dialog.button(JButtonMatcher.withText("Cancel")).click();
    }

    /**
     * Test Preferences
     */
    @Test
    @GUITest
    public void m009_shouldOpenPreferences() {
        window.menuItemWithPath("Edit", "Preferences...").click();

        final Dialog prefDialog = robot().finder().find(DialogMatcher.withTitle("Preferences"));

        if (prefDialog != null) {
            final DialogFixture dialog = new DialogFixture(robot(), prefDialog);

            dialog.requireVisible();
            dialog.moveToFront();

            saveScreenshot(dialog, "Aspro2-prefs.png");

            dialog.close();
        }
    }

    /**
     * Test Feedback report
     */
    @Test
    @GUITest
    public void m010_shouldOpenFeedbackReport() {

        // hack to solve focus trouble in menu items :
        window.menuItemWithPath("Help").focus();

        window.menuItemWithPath("Help", "Report Feedback to JMMC...").click();

        final DialogFixture dialog = window.dialog(withTitle("Feedback Report ").andShowing());

        dialog.requireVisible();
        dialog.moveToFront();

        final JTextComponentFixture emailField = dialog.textBox(JTextComponentMatcher.withText(FAKE_EMAIL));

        // hide my email address :
        emailField.setText("type your email address here");

        saveScreenshot(dialog, "Aspro2-FeebackReport.png");

        // restore my preferences :
        emailField.setText(FAKE_EMAIL);

        // close dialog :
        dialog.close();

        // reset email preference:
        defineEmailPref(CURRENT_EMAIL);
    }

    /**
     * Test Chara PoPs
     */
    @Test
    @GUITest
    public void m020_shouldShowCharaPoPs() {
        final Dimension oldSize = window.target.getSize();
        try {
            window.resizeTo(new Dimension(1200, 950));

            openObservation("Aspro2_sample.asprox");

            // show observability plot :
            getMainTabbedPane().selectTab(SettingPanel.TAB_OBSERVABILITY);

            window.list("jListTargets").selectItem("HIP1234");

            final JPanelFixture form = window.panel("observationForm");

            // select CHARA interferometer:
            form.comboBox("jComboBoxInterferometer").selectItem("CHARA");

            // waits for computation to finish :
            AsproTestUtils.checkRunningTasks();

            captureMainForm("Aspro2-main-chara.png");

            final int left = 5 + window.panel("jPanelTargets").component().getWidth();

            final int width = 5 + window.panel("jPanelMain").component().getWidth()
                    + window.panel("jPanelConfigurations").component().getWidth();

            // Get plot's title to show PoPs in use:
            final int height = getMainFormHeight(window) + 50;

            saveCroppedScreenshotOf("popsAuto.png", left, 0, width, height);

            // set PoPs to '34' (manual):
            final JFormattedTextField jTextPoPs = (JFormattedTextField) form.textBox("jTextPoPs").component();

            GuiActionRunner.execute(new GuiTask() {
                @Override
                protected void executeInEDT() {
                    // Integer field :
                    jTextPoPs.setValue(NumberUtils.valueOf(34));
                }
            });

            // waits for computation to finish :
            AsproTestUtils.checkRunningTasks();

            // show observability plot (force plot refresh) :
            getMainTabbedPane().selectTab(SettingPanel.TAB_OBSERVABILITY);

            saveCroppedScreenshotOf("popsUser.png", left, 0, width, height);

            GuiActionRunner.execute(new GuiTask() {
                @Override
                protected void executeInEDT() {
                    // Integer field :
                    jTextPoPs.setValue(null);
                }
            });

            // select CHARA interferometer:
            form.comboBox("jComboBoxInstrument").selectItem("MIRCX-MYSTIC");

            // waits for computation to finish :
            AsproTestUtils.checkRunningTasks();

            saveCroppedScreenshotOf("popsAuto-6T.png", left, 0, width, height);

            window.button("jButtonPopsSet").click();

            // change configuration:
            window.list("jListInstrumentConfigurations").selectItem(5);

            saveCroppedScreenshotOf("popsFixed-5T.png", left, 0, width, height);

        } finally {
            // Restore size:
            window.resizeTo(oldSize);
        }
    }

    /**
     * Test Open file "Aspro2_sample_with_calibrators.asprox"
     */
    @Test
    @GUITest
    public void m021_shouldOpenSampleWithCalibrators() {

        openObservation("Aspro2_sample_with_calibrators.asprox");

        window.list("jListTargets").selectItem("HD 1583 (cal)");

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        // Capture observability plot :
        showPlotTab(SettingPanel.TAB_OBSERVABILITY, "Aspro2-calibrators-obs.png");

        // Capture UV Coverage plot :
        showPlotTab(SettingPanel.TAB_UV_COVERAGE, "Aspro2-calibrators-uv.png");

        // Export OIFits / OB :
        exportOIFits();
        exportOB();

        // target editor with calibrators :
        window.button("jButtonTargetEditor").click();

        final DialogFixture dialog = window.dialog(withTitle("Target Editor").andShowing());

        dialog.requireVisible();
        dialog.moveToFront();

        dialog.tabbedPane().selectTab(TargetEditorDialog.TAB_MODELS);

        dialog.tree().selectPath("Models/HD 1583 (cal)/disk1");

        saveScreenshot(dialog, "Aspro2-calibrators-Model.png");

        dialog.tabbedPane().selectTab(TargetEditorDialog.TAB_TARGETS);

        dialog.tree().selectPath("Targets/HIP1234/HD 1583 (cal)");

        saveScreenshot(dialog, "Aspro2-calibrators-Target.png");

        // close dialog :
        dialog.button(JButtonMatcher.withText("Cancel")).click();
    }

    /**
     * Test Open file "Aspro2_sample_multi.asprox"
     */
    @Test
    @GUITest
    public void m022_shouldOpenSampleMultiConf() {

        openObservation("Aspro2_sample_multi.asprox");

        // Capture map plot :
        showPlotTab(SettingPanel.TAB_INTERFEROMETER_MAP, "Aspro2-multiConf-map.png");

        // Capture observability plot :
        showPlotTab(SettingPanel.TAB_OBSERVABILITY, "Aspro2-multiConf-obs.png");

        // Capture UV Coverage plot :
        showPlotTab(SettingPanel.TAB_UV_COVERAGE, "Aspro2-multiConf-uv.png");

        // Capture OIFits viewer plot :
        showPlotTab(SettingPanel.TAB_OIFITS_VIEWER, "Aspro2-multiConf-oifitsViewer.png");
    }

    /**
     * Test Open file "Aspro2_models_BB.asprox" (User model)
     */
    @Test
    @GUITest
    public void m023_shouldOpenSampleWithBlackBodyModel() {
        openObservation("Aspro2_models_BB.asprox");

        window.list("jListTargets").selectItem("ring_plus_star");

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        // target editor with calibrators :
        window.button("jButtonTargetEditor").click();

        final DialogFixture dialog = window.dialog(withTitle("Target Editor").andShowing());

        dialog.requireVisible();
        dialog.moveToFront();

        dialog.tabbedPane().selectTab(TargetEditorDialog.TAB_MODELS);

        dialog.tree().selectPath("Models/ring_plus_star");

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        saveScreenshot(dialog, "Aspro2-BB_Model.png");

        // close dialog :
        dialog.button(JButtonMatcher.withText("Cancel")).click();

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        // Capture OIFits viewer plot :
        showPlotTab(SettingPanel.TAB_OIFITS_VIEWER, "Aspro2-vis2-bb.png");

        // Export OIFits / OB :
        exportOIFits();
    }

    /**
     * Test Open file "Aspro2_sample_spiral.asprox" (User model)
     */
    @Test
    @GUITest
    public void m024_shouldOpenSampleWithUserModel() {
        try {
            Preferences.getInstance().setPreference(Preferences.MODEL_IMAGE_LUT, ColorModels.COLOR_MODEL_HEAT);

            openObservation("Aspro2_sample_spiral.asprox");

            // target editor with calibrators :
            window.button("jButtonTargetEditor").click();

            final DialogFixture dialog = window.dialog(withTitle("Target Editor").andShowing());

            dialog.requireVisible();
            dialog.moveToFront();

            dialog.tabbedPane().selectTab(TargetEditorDialog.TAB_MODELS);

            dialog.tree().selectPath("Models/HD 1234");

            // waits for computation to finish :
            AsproTestUtils.checkRunningTasks();

            saveScreenshot(dialog, "Aspro2-UserModel.png");

            // close dialog :
            dialog.button(JButtonMatcher.withText("Cancel")).click();

            window.list("jListTargets").selectItem("HD 1234");

            // select UV tab:
            getMainTabbedPane().selectTab(SettingPanel.TAB_UV_COVERAGE);

            final JPanelFixture panel = window.panel("uvCoveragePanel");

            // disable Compute OIFits data :
            panel.checkBox("jCheckBoxDoOIFits").uncheck();

            // zoom uv max (trigger uv model to compute again):
            panel.textBox("jFieldUVMax").setText("40.00");

            // select UV tab to let fields lost focus (trigger change listeners):
            getMainTabbedPane().selectTab(SettingPanel.TAB_UV_COVERAGE);

            // waits for computation to finish :
            AsproTestUtils.checkRunningTasks();

            // Capture UV Coverage plot :
            showPlotTab(SettingPanel.TAB_UV_COVERAGE, "Aspro2-UserModel-uv.png");

            // reset zoom:
            panel.textBox("jFieldUVMax").setText("139");

        } catch (PreferencesException pe) {
            logger.error("setPreference failed", pe);
        } finally {
            try {
                Preferences.getInstance().setPreference(Preferences.MODEL_IMAGE_LUT, Preferences.DEFAULT_IMAGE_LUT);
            } catch (PreferencesException pe) {
                logger.error("setPreference failed", pe);
            }
        }
    }

    /**
     * Test Open file "Aspro2_sample_AO_on_SiriusB.asprox" (Target Group Form)
     */
    @Test
    @GUITest
    public void m025_shouldOpenSampleWithAOTargetGroup() {
        final Frame frame = window.target;
        final Point ref = frame.getLocation();
        final DisplayMode mode = frame.getGraphicsConfiguration().getDevice().getDisplayMode();
        try {
            window.moveTo(new Point(mode.getWidth() - frame.getWidth(), ref.y));

            Preferences.getInstance().setPreference(Preferences.MODEL_IMAGE_LUT, ColorModels.COLOR_MODEL_ASPRO);

            openObservation("Aspro2_sample_AO_on_SiriusB.asprox");

            // Capture observability plot :
            showPlotTab(SettingPanel.TAB_OBSERVABILITY, "Aspro2-GroupAO-obs.png");

            // target editor with calibrators :
            window.button("jButtonTargetEditor").click();

            final DialogFixture dialog = window.dialog(withTitle("Target Editor").andShowing());

            dialog.requireVisible();
            dialog.moveToFront();

            dialog.tabbedPane().selectTab(TargetEditorDialog.TAB_GROUPS);

            dialog.tree(TargetGroupForm.TREE_GROUPS).selectPath("Groups/AO Star/Sirius A");

            // highlight group information:
            dialog.tree(TargetGroupForm.TREE_GROUPS).selectPath("Groups/AO Star");

            // waits for computation to finish :
            AsproTestUtils.checkRunningTasks();

            saveScreenshot(dialog, "Aspro2-GroupAO.png");

            // close dialog :
            dialog.button(JButtonMatcher.withText("Cancel")).click();

            window.list("jListTargets").selectItem("Sirius B");

            // select UV tab:
            getMainTabbedPane().selectTab(SettingPanel.TAB_UV_COVERAGE);

            // waits for computation to finish :
            AsproTestUtils.checkRunningTasks();

            enableTooltips(true);

            // move the mouse on Status Warning:
            robot().moveMouse(window.component(), 760, 230 + M_TOP); // TODO: check position

            // let tooltip appear:
            pauseMedium();

            saveScreenshotWithMousePointer(window, "Aspro2-GroupAO-status.png");

        } catch (PreferencesException pe) {
            logger.error("setPreference failed", pe);
        } finally {
            enableTooltips(false);
            try {
                Preferences.getInstance().setPreference(Preferences.MODEL_IMAGE_LUT, Preferences.DEFAULT_IMAGE_LUT);
            } catch (PreferencesException pe) {
                logger.error("setPreference failed", pe);
            }
            // restore position:
            window.moveTo(ref);
        }
    }

    /**
     * Test Open file "Aspro2_sample_rawobs.asprox" (Target Group Form)
     */
    @Test
    @GUITest
    public void m026_shouldOpenSampleWithRawObs() {
        final Dimension oldSize = window.target.getSize();
        try {
            Preferences.getInstance().setPreference(Preferences.MODEL_IMAGE_LUT, ColorModels.COLOR_MODEL_GRAY);

            openObservation("Aspro2_sample_rawobs.asprox");

            window.resizeTo(new Dimension(1100, 1000));

            // Filter raw obs by selecting only the GRAVITY instrument:
            final int insIdx = AsproConstants.INS_OBS_LIST.indexOf("GRAVITY");
            window.list("jListInstruments").selectItem(insIdx);

            enableTooltips(true);

            // Capture observability plot :
            getMainTabbedPane().selectTab(SettingPanel.TAB_OBSERVABILITY);

            // move the mouse on plot:
            robot().moveMouse(window.component(), 760, 525 + M_TOP); // TODO: check position

            // let tooltip appear:
            pauseMedium();

            saveScreenshotWithMousePointer(window, "Aspro2-rawobs-obs.png");

            // Capture UV Coverage plot :
            getMainTabbedPane().selectTab(SettingPanel.TAB_UV_COVERAGE);

            // move the mouse on plot:
            robot().moveMouse(window.component(), 760, 485 + M_TOP); // TODO: check position

            // let tooltip appear:
            pauseMedium();

            saveScreenshotWithMousePointer(window, "Aspro2-rawobs-uv.png");

        } catch (PreferencesException pe) {
            logger.error("setPreference failed", pe);
        } finally {
            enableTooltips(false);
            try {
                Preferences.getInstance().setPreference(Preferences.MODEL_IMAGE_LUT, Preferences.DEFAULT_IMAGE_LUT);
            } catch (PreferencesException pe) {
                logger.error("setPreference failed", pe);
            }
            // Restore size:
            window.resizeTo(oldSize);
        }
    }

    /**
     * Capture the moon details
     */
    @Test
    @GUITest
    public void m099_captureMoon() {
        openObservation("Aspro2_sample_moon.asprox");

        // Capture observability plot :
        getMainTabbedPane().selectTab(SettingPanel.TAB_OBSERVABILITY);

        final JPanelFixture panel = window.panel("observabilityPanel");

        // enable scroll view (needed for scroller below):
        panel.checkBox("jCheckBoxScrollView").check();
        // enable detailed plot :
        panel.checkBox("jCheckBoxDetailedOutput").check();

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();

        panel.scrollBar("scroller").scrollTo(8); // see ETA TAU (moon) [0 to 14]

        // Capture observability plot of detailed plot :
        saveScreenshot(getMainTabbedPane(), "Aspro2-moon-obs-det.png");

        // disable detailed plot :
        panel.checkBox("jCheckBoxDetailedOutput").uncheck();
        // disable scroll view :
        panel.checkBox("jCheckBoxScrollView").uncheck();

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();
    }

    // --- InterOp ---
    /**
     * Test Interop menu : Start AppLauncher & A2P2 manually before this test
     */
    @Test
    @GUITest
    public void m100_showInteropMenu() {
        // hack to solve focus trouble in menu items :
        window.menuItemWithPath("Interop").click();
        captureMainForm("Aspro2-interop-menu.png");

        window.menuItemWithPath("Interop", "Show Hub Status").click();

        final Frame hubFrame = robot().finder().find(FrameMatcher.withTitle("SAMP Status"));

        if (hubFrame != null) {
            final FrameFixture frame = new FrameFixture(robot(), hubFrame);

            frame.requireVisible();
            frame.moveToFront();

            frame.list(new GenericTypeMatcher<JList>(JList.class) {
                @Override
                protected boolean isMatching(JList component) {
                    return "org.astrogrid.samp.gui.ClientListCellRenderer".equals(component.getCellRenderer().getClass().getName());
                }
            }).selectItem("Aspro2");

            saveScreenshot(frame, "Aspro2-interop-hubStatus.png");

            // close frame :
            frame.close();
        }
    }

    /**
     * Test SearchCal integration : Start AppLauncher manually before this test
     */
    @Test
    @GUITest
    public void m101_shouldCallSearchCall() {
        // hack to solve focus trouble in menu items :
        window.menuItemWithPath("Interop").focus();

        try {
            enableTooltips(true);

            window.menuItemWithPath("Interop", "Search calibrators").click();
            window.menuItemWithPath("Interop", "Search calibrators", "SearchCal").focus();

            captureMainForm("Aspro2-calibrators-StartSearchQuery.png");

        } finally {
            enableTooltips(false);
        }
    }

    /**
     * Test LITpro integration : Start AppLauncher manually before this test
     */
    @Test
    @GUITest
    public void m102_shouldCallLITpro() {
        // hack to solve focus trouble in menu items :
        window.menuItemWithPath("Interop").focus();

        try {
            enableTooltips(true);

            window.menuItemWithPath("Interop", "Perform model fitting").click();
            window.menuItemWithPath("Interop", "Perform model fitting", "LITpro").focus();

            captureMainForm("Aspro2-LITpro-send.png");

        } finally {
            enableTooltips(false);
        }
    }

    /**
     * Test A2P2 integration : Start A2P2 manually before this test
     */
    @Test
    @GUITest
    public void m103_shouldCallA2P2() {
        // hack to solve focus trouble in menu items :
        window.menuItemWithPath("Interop").focus();

        try {
            enableTooltips(true);

            window.menuItemWithPath("Interop", "Send Obs. block(s) to A2p2").click();
            window.menuItemWithPath("Interop", "Send Obs. block(s) to A2p2", "A2P2 samp relay").focus();

            captureMainForm("Aspro2-a2p2-SendOB.png");

        } finally {
            enableTooltips(false);
        }
    }

    /* 
     --- Utility methods  ---------------------------------------------------------
     */
    private void openObservation(final String obsFileName) {
        // hack to solve focus trouble in menu items :
        window.menuItemWithPath("File").focus();
        window.menuItemWithPath("File", "Open observation").click();

        confirmDialogDontSave();

        window.fileChooser().selectFile(new File(TEST_FOLDER + obsFileName)).approve();

        pauseMedium();

        // waits for computation to finish :
        AsproTestUtils.checkRunningTasks();
    }

    /**
     * Export Observing block file using default file name
     */
    private void exportOB() {
        // export OB file :
        window.pressAndReleaseKey(keyCode(VK_B).modifiers(Platform.controlOrCommandMask()));

        // close deprecation message:
        closeMessage();

        // close warning on OIFITS message:
        closeOIFitsMessage();

        // use image folder to store OB file and validate :
        window.fileChooser().setCurrentDirectory(getScreenshotFolder()).approve();

        // overwrite any existing file :
        confirmDialogFileOverwrite();
        // close report message (ob files) :
        closeMessage();
        // close check ESO magnitudes :
        closeMessage();
    }

    /**
     * Export OIFits file using default file name
     */
    private void exportOIFits() {
        // export OIFits :
        window.pressAndReleaseKey(keyCode(VK_F).modifiers(Platform.controlOrCommandMask()));

        // close warning on OIFITS message:
        closeOIFitsMessage();

        // use image folder to store OIFits and validate :
        window.fileChooser().setCurrentDirectory(getScreenshotFolder()).approve();

        // overwrite any existing file :
        confirmDialogFileOverwrite();
    }

    /**
     * Export the current plot to PDF
     */
    private void exportPDF() {
        // export PDF :
        window.pressAndReleaseKey(keyCode(VK_P).modifiers(Platform.controlOrCommandMask()));

        // close warning on OIFITS message:
        closeOIFitsMessage();

        // use image folder to store PDF and validate :
        window.fileChooser().setCurrentDirectory(getScreenshotFolder()).approve();

        // overwrite any existing file :
        confirmDialogFileOverwrite();
    }

    /**
     * Close any option pane by clicking Yes
     * @return true if a message was closed
     */
    protected final boolean closeOIFitsMessage() {
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
     * Show the plot tab of the given name and capture the window screenshot
     * @param tabName tab name
     * @param fileName file name of the window screenshot
     */
    private void showPlotTab(final String tabName, final String fileName) {
        // select tab :
        getMainTabbedPane().selectTab(tabName);

        // Capture window screenshot :
        saveScreenshot(window, fileName);

        // export PDF :
        exportPDF();
    }

    /**
     * Return the main Aspro tabbed pane
     * @return Aspro tabbed pane
     */
    private JTabbedPaneFixture getMainTabbedPane() {
        return window.tabbedPane("AsproTab");
    }

    /**
     * Capture a screenshot of the main form using the given file name
     * @param fileName the file name (including the png extension)
     */
    private void captureMainForm(final String fileName) {
        saveCroppedScreenshotOf(fileName, 0, 0, -1, getMainFormHeight(window));
    }

    /**
     * Determine the height of the main form
     * @param window window fixture
     * @return height of the main form
     */
    private static int getMainFormHeight(final FrameFixture window) {
        // window (top left corner):
        final Point pw = window.component().getLocationOnScreen();

        final JComponent com = window.panel("observationForm").component();

        final Point pf = com.getLocationOnScreen();

        // distance including window bar + menu bar (+ borders):
        int height = (pf.y - pw.y - M_TOP);

        height += com.getHeight();

        height += 10;

        return height;
    }
}
