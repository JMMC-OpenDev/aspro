/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AsproDocJUnitTest.java,v 1.4 2011-03-14 14:47:50 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.3  2011/03/11 16:02:05  bourgesl
 * updated scenario
 *
 * Revision 1.2  2011/03/11 15:04:07  bourgesl
 * added image operations (resize, crop ...)
 *
 * Revision 1.1  2011/03/11 12:55:35  bourgesl
 * added fest-swing test cases for Aspro 2
 *
 */
package fest;

import static org.fest.swing.core.matcher.DialogMatcher.*;

import com.mortennobel.imagescaling.AdvancedResizeOp;
import com.mortennobel.imagescaling.ResampleOp;

import fest.common.JmcsApplicationSetup;

import fest.common.JmcsFestSwingJUnitTestCase;
import fr.jmmc.aspro.AsproGui;
import fr.jmmc.aspro.gui.SettingPanel;
import java.awt.Frame;
import java.awt.image.BufferedImage;
import java.io.File;
import javax.swing.JComponent;
import javax.swing.JList;
import org.fest.swing.annotation.GUITest;
import org.fest.swing.core.GenericTypeMatcher;
import org.fest.swing.core.matcher.FrameMatcher;
import org.fest.swing.core.matcher.JButtonMatcher;
import org.fest.swing.core.matcher.JTextComponentMatcher;
import org.fest.swing.fixture.DialogFixture;
import org.fest.swing.fixture.FrameFixture;
import org.fest.swing.fixture.JTabbedPaneFixture;
import org.fest.swing.fixture.JTextComponentFixture;
import org.junit.Test;

/**
 * This simple tests takes screenshots to complete the Aspro2 documentation
 * 
 * @author bourgesl
 */
public final class AsproDocJUnitTest extends JmcsFestSwingJUnitTestCase {

  /**
   * Define the application
   */
  static {
    JmcsApplicationSetup.define(AsproGui.class, "-open", "/home/bourgesl/dev/aspro/test/Aspro2_sample.asprox");

    // define robot delays :
    // fast delay :
    defineRobotDelayBetweenEvents(VERY_SHORT_DELAY);
    // normal delay
    //defineRobotDelayBetweenEvents(SHORT_DELAY);

    // define delay before taking screenshot :
    // fast delay :
    defineScreenshotDelay(VERY_SHORT_DELAY);
    // normal delay
    //defineScreenshotDelay(SHORT_DELAY);

    enableTooltips(false);
  }

  /**
   * Test if the application started correctly
   */
  @Test
  @GUITest
  public void shouldStart() {
    window.tabbedPane().requireVisible();

    window.textBox("starSearchField").enterText("Simbad");

    // waits for computation to finish :
    AsproTestUtils.checkRunningTasks();

    // Capture initial state :
    saveScreenshot(window, "Aspro2-map.png");
  }

  /**
   * Capture the main panel
   */
  @Test
  @GUITest
  public void captureMain() {
    captureMainForm("Aspro2-main.png");
  }

  /**
   * Test plot tabs
   */
  @Test
  @GUITest
  public void shouldNavigateTabs() {

    final JTabbedPaneFixture plotTabs = window.tabbedPane();

    // Capture UV Coverage :
    plotTabs.selectTab(SettingPanel.TAB_OBSERVABILITY);
    saveScreenshot(window, "Aspro2-obs.png");

    // Capture UV Coverage :
    plotTabs.selectTab(SettingPanel.TAB_UV_COVERAGE);

    final BufferedImage image = takeScreenshotOf(window);

    saveImage(image, "Aspro2-uv.png");
    saveImage(image, "Aspro2-screen.png");

    // TODO : refactor that code :

    // miniature for aspro web page : 350px width :
    final int width = 350;
    final int height = Math.round(1f * width * image.getHeight() / image.getWidth());

    BufferedImage rescaledImage = null;
    ResampleOp resampleOp = null;

    // use Lanczos3 resampler and soft unsharp mask :
    resampleOp = new ResampleOp(width, height);
    resampleOp.setUnsharpenMask(AdvancedResizeOp.UnsharpenMask.Soft);
    rescaledImage = resampleOp.filter(image, null);
    saveImage(rescaledImage, "Aspro2-screen-small.png");
  }

  /**
   * Test Target editor
   */
  @Test
  @GUITest
  public void shouldOpenTargetEditor() {

    window.button("jButtonTargetEditor").click();

    final DialogFixture dialog = window.dialog(withTitle("Target Editor").andShowing());

    dialog.requireVisible();
    dialog.moveToFront();

    dialog.tabbedPane().selectTab("Models");

    dialog.tree().selectPath("Models/HIP1234/elong_disk1");
    saveScreenshot(dialog, "Aspro2-Model.png");

    dialog.tabbedPane().selectTab("Targets");

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
  public void shouldOpenPreferences() {
    window.menuItemWithPath("Edit", "Preferences...").click();

    final Frame prefFrame = robot().finder().find(FrameMatcher.withTitle("Preferences"));

    if (prefFrame != null) {
      final FrameFixture frame = new FrameFixture(robot(), prefFrame);

      frame.requireVisible();
      frame.moveToFront();

      saveScreenshot(frame, "Aspro2-prefs.png");

      // close frame :
      frame.close();
    }
  }

  /**
   * Test Interop menu : Start SearchCal and LITpro manually before this test
   */
  @Test
  @GUITest
  public void showInteropMenu() {
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
   * Test SearchCal integration : Start SearchCal manually before this test
   */
  @Test
  @GUITest
  public void shouldCallSearchCall() {
    // hack to solve focus trouble in menu items :
    window.menuItemWithPath("Interop").focus();

    enableTooltips(true);

    window.menuItemWithPath("Interop", "Search calibrators").click();
    window.menuItemWithPath("Interop", "Search calibrators", "SearchCal").focus();

    captureMainForm("Aspro2-calibrators-StartSearchQuery.png");

    enableTooltips(false);
  }

  /**
   * Test LITpro integration : Start LITpro manually before this test
   */
  @Test
  @GUITest
  public void shouldCallLITpro() {
    // hack to solve focus trouble in menu items :
    window.menuItemWithPath("Interop").focus();

    enableTooltips(true);

    window.menuItemWithPath("Interop", "Perform model fitting").click();
    window.menuItemWithPath("Interop", "Perform model fitting", "LITpro").focus();

    captureMainForm("Aspro2-LITpro-send.png");

    enableTooltips(false);
  }

  /**
   * Test Open file "/home/bourgesl/dev/aspro/test/Aspro2_sample_with_calibrators.asprox"
   */
  @Test
  @GUITest
  public void shouldOpenSampleWithCalibrators() {

    // hack to solve focus trouble in menu items :
    window.menuItemWithPath("File").focus();
    window.menuItemWithPath("File", "Open observation").click();

    window.fileChooser().selectFile(new File("/home/bourgesl/dev/aspro/test/Aspro2_sample_with_calibrators.asprox"));
    window.fileChooser().approve();

    window.list("jListTargets").selectItem("HD 3546 (cal)");

    // capture tabs :
    final JTabbedPaneFixture plotTabs = window.tabbedPane();

    // Capture UV Coverage :
    plotTabs.selectTab(SettingPanel.TAB_OBSERVABILITY);

    // waits for computation to finish :
    AsproTestUtils.checkRunningTasks();

    saveScreenshot(window, "Aspro2-calibrators-obs.png");

    // Capture UV Coverage :
    plotTabs.selectTab(SettingPanel.TAB_UV_COVERAGE);

    saveScreenshot(window, "Aspro2-calibrators-uv.png");

    // target editor with calibrators :
    window.button("jButtonTargetEditor").click();

    final DialogFixture dialog = window.dialog(withTitle("Target Editor").andShowing());

    dialog.requireVisible();
    dialog.moveToFront();

    dialog.tabbedPane().selectTab("Models");
    dialog.tree().selectPath("Models/HD 3546 (cal)/disk1");

    saveScreenshot(dialog, "Aspro2-calibrators-Model.png");

    dialog.tabbedPane().selectTab("Targets");

    saveScreenshot(dialog, "Aspro2-calibrators-Target.png");

    // close dialog :
    dialog.button(JButtonMatcher.withText("Cancel")).click();
  }

  /**
   * Test Feedback report
   */
  @Test
  @GUITest
  public void shouldOpenFeedbackReport() {

    // hack to solve focus trouble in menu items :
    window.menuItemWithPath("Help").focus();

    window.menuItemWithPath("Help", "Report Feedback to JMMC...").click();

    final DialogFixture dialog = window.dialog(withTitle("JMMC Feedback Report ").andShowing());

    dialog.requireVisible();
    dialog.moveToFront();

    final JTextComponentFixture emailField = dialog.textBox(JTextComponentMatcher.withText("bourges.laurent@obs.ujf-grenoble.fr"));

    emailField.deleteText().enterText("type your email address here");

    saveScreenshot(dialog, "Aspro2-FeebackReport.png");

    // close dialog :
    dialog.close();
  }

  /**
   * Test the application exit sequence
   */
  @Test
  @GUITest
  public void shouldExit() {
    window.close();

    confirmDialogDontSave();
  }

  /* 
  --- Utility methods  ---------------------------------------------------------
   */
  /**
   * Close Save confirm dialog clicking on "Don't Save" button
   */
  private void confirmDialogDontSave() {
    // close confirm dialog :
    window.optionPane().buttonWithText("Don't Save").click();
  }

  /**
   * Capture a screenshot of the main form using the given file name
   * @param fileName the file name (including the png extension)
   */
  private void captureMainForm(final String fileName) {

    final BufferedImage image = takeScreenshotOf(window);

    final BufferedImage croppedImage = image.getSubimage(0, 0, image.getWidth(), getMainFormHeight(window));

    saveImage(croppedImage, fileName);
  }

  /**
   * Determine the height of the main form
   * @param window window fixture
   * @return height of the main form
   */
  private static int getMainFormHeight(final FrameFixture window) {
    int height = 32 + 10;

    JComponent com;
    com = window.panel("observationForm").component();
    height += com.getHeight();

    com = window.menuItemWithPath("File").component();
    height += com.getHeight();

    return height;
  }
}
