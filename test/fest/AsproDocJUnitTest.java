/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AsproDocJUnitTest.java,v 1.2 2011-03-11 15:04:07 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2011/03/11 12:55:35  bourgesl
 * added fest-swing test cases for Aspro 2
 *
 */
package fest;

import com.mortennobel.imagescaling.AdvancedResizeOp;
import com.mortennobel.imagescaling.ResampleOp;
import static org.fest.swing.core.matcher.DialogMatcher.*;
import static org.fest.swing.core.matcher.JButtonMatcher.*;
import fest.common.JmcsApplicationSetup;

import fest.common.JmcsFestSwingJUnitTestCase;
import fr.jmmc.aspro.AsproGui;
import fr.jmmc.aspro.gui.SettingPanel;
import java.awt.image.BufferedImage;
import javax.swing.JComponent;
import org.fest.swing.annotation.GUITest;
import org.fest.swing.fixture.DialogFixture;
import org.fest.swing.fixture.JTabbedPaneFixture;
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
  }

  /**
   * Test if the application started correctly
   */
  @Test
  @GUITest
  public void shouldStart() {
    window.tabbedPane().requireVisible();

    window.textBox("starSearchField").enterText("SimBad");

    AsproTestUtils.checkRunningTasks();

    // Capture initial state :
    saveScreenshot(window, "Aspro2.png");
  }

  /**
   * Capture the main panel
   */
  @Test
  @GUITest
  public void captureMain() {

    int height = 32 + 10;

    JComponent com;
    com = window.panel("observationForm").component();
    logger.severe("observationForm height = " + com.getHeight());

    height += com.getHeight();

    com = window.menuItemWithPath("File").component();
    logger.severe("menu bar height = " + com.getHeight());

    height += com.getHeight();

    final BufferedImage image = takeScreenshotOf(window);

    final BufferedImage croppedImage = image.getSubimage(0, 0, image.getWidth(), height);

    saveImage(croppedImage, "Aspro2-main.png");
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

    logger.severe("image size = " + image.getWidth() + "x" + image.getHeight());

    // miniature for aspro web page : 350px width :
    final int width = 350;
    final int height = Math.round(1f * width * image.getHeight() / image.getWidth());

    logger.severe("miniature size = " + width + "x" + height);

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

    final DialogFixture dialog = window.dialog(withTitle("Target Editor"));

    dialog.requireVisible();

    saveScreenshot(dialog, "Aspro2-TargetEditor.png");

    window.button(withText("Cancel")).click();
  }

  /**
   * Test the application exit sequence
   */
  @Test
  @GUITest
  public void shouldExit() {
    window.close();

    window.optionPane().buttonWithText("Don't Save").click();
  }
}
