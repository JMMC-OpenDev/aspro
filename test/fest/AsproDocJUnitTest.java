/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AsproDocJUnitTest.java,v 1.1 2011-03-11 12:55:35 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fest;

import static org.fest.swing.core.matcher.DialogMatcher.*;
import static org.fest.swing.core.matcher.JButtonMatcher.*;
import fest.common.JmcsApplicationSetup;

import fest.common.JmcsFestSwingJUnitTestCase;
import fr.jmmc.aspro.AsproGui;
import fr.jmmc.aspro.gui.SettingPanel;
import fr.jmmc.mcs.astro.star.StarResolverWidget;
import javax.swing.text.JTextComponent;
import org.fest.swing.annotation.GUITest;
import org.fest.swing.core.GenericTypeMatcher;
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

    window.textBox(new GenericTypeMatcher<JTextComponent>(JTextComponent.class) {

      @Override
      protected boolean isMatching(JTextComponent c) {
        return c instanceof StarResolverWidget;
      }
    }).enterText("SimBad Star resolver");

    // Capture initial state :
    saveScreenshot(window, "Aspro2.png");
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
    saveScreenshot(window, "Aspro2-uv.png");

    /*
    window.panel(new GenericTypeMatcher<JPanel>(JPanel.class) {

    @Override
    protected boolean isMatching(JPanel panel) {
    return panel instanceof UVCoveragePanel;
    }
    }).requireNotVisible();
     */

    /*
    window.panel(new GenericTypeMatcher<JPanel>(JPanel.class) {

    @Override
    protected boolean isMatching(JPanel panel) {
    return panel instanceof BasicObservationForm;
    }
    }).requireNotVisible();
     */
  }

  /**
   * Test Target editor
   */
  @Test
  @GUITest
  public void shouldOpenTargetEditor() {

    window.button(withText("Target editor")).click();

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
//    window.optionPane().buttonWithText("Cancel").click();
  }
}
