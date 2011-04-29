/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AsproNightBoundsTest.java,v 1.1 2011-04-22 15:33:34 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.11  2011/03/30 09:00:56  bourgesl
 * use testgui module for Swing tests
 *
 * Revision 1.10  2011/03/17 15:58:32  bourgesl
 * disable dev LAF menu
 *
 * Revision 1.9  2011/03/17 15:42:40  bourgesl
 * added shouldOpenSampleMultiConf test
 *
 * Revision 1.8  2011/03/15 16:36:00  bourgesl
 * comments
 *
 * Revision 1.7  2011/03/15 16:12:19  bourgesl
 * disable statusBar updates when running fest swing tests
 *
 * Revision 1.6  2011/03/15 15:46:37  bourgesl
 * new tests : PDF / OIFits / OB
 * observability (detailed / baseline limits)
 * Pops
 *
 * Revision 1.5  2011/03/14 17:43:10  bourgesl
 * use setText instead of enterText
 *
 * Revision 1.4  2011/03/14 14:47:50  bourgesl
 * added many tests
 *
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

import fest.common.JmcsApplicationSetup;

import fest.common.JmcsFestSwingJUnitTestCase;

import java.util.Calendar;
import java.util.GregorianCalendar;
import javax.swing.JFormattedTextField;

import org.fest.swing.annotation.GUITest;
import org.fest.swing.edt.GuiActionRunner;
import org.fest.swing.edt.GuiTask;
import org.fest.swing.fixture.JPanelFixture;
import org.fest.swing.fixture.JSpinnerFixture;

import org.junit.Test;

/**
 * This simple tests changes the observation date to test night bounds
 * 
 * @author bourgesl
 */
public final class AsproNightBoundsTest extends JmcsFestSwingJUnitTestCase {

  /** name of the tab pane corresponding to the observability panel */
  private static final String TAB_OBSERVABILITY = "Observability";
  /** initial year */
  private static final int YEAR = 2005;
  /** number of days to traverse (30 years) */
  private static final int DAYS = 10 * 365;

  /**
   * Define the application
   */
  static {
    // disable dev LAF menu :
    System.setProperty("jmcs.laf.menu", "false");

    JmcsApplicationSetup.define(
            fr.jmmc.aspro.AsproGui.class,
            "-open", "/home/bourgesl/dev/aspro/test/TEST_RA_LIMITS.asprox");

    // define robot delays :
    defineRobotDelayBetweenEvents(SHORT_DELAY);

    // define delay before taking screenshot :
    defineScreenshotDelay(SHORT_DELAY);

    // disable tooltips :
    enableTooltips(false);

    // customize PDF action to avoid use StatusBar :
    fr.jmmc.aspro.gui.action.ExportPDFAction.setAvoidUseStatusBar(true);
  }

  /**
   * Test if the application started correctly
   */
  @Test
  @GUITest
  public void shouldStart() {
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

    final JPanelFixture form = window.panel("observationForm");

    // select VLTI interferometer :
    form.comboBox("jComboBoxInterferometer").selectItem("VLTI");

    // waits for computation to finish :
    AsproTestUtils.checkRunningTasks();

    final JSpinnerFixture dateSpinner = form.spinner("jDateSpinner");

    traverseDays(dateSpinner);
  }

  /**
   * Test observability plot when updating the observation date
   */
  @Test
  @GUITest
  public void shouldChangeDateForCHARA() {

    final JPanelFixture form = window.panel("observationForm");

    // select CHARA interferometer :
    form.comboBox("jComboBoxInterferometer").selectItem("CHARA");

    // waits for computation to finish :
    AsproTestUtils.checkRunningTasks();

    // set PoPs to '54' :
    final JFormattedTextField jTextPoPs = (JFormattedTextField) form.textBox("jTextPoPs").component();

    GuiActionRunner.execute(new GuiTask() {

      protected void executeInEDT() {
        // Integer field :
        jTextPoPs.setValue(Integer.valueOf(54));
      }
    });

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

  /**
   * Test the application exit sequence : ALWAYS THE LAST TEST
   */
  @Test
  @GUITest
  public void shouldExit() {
    logger.severe("shouldExit test");

    window.close();

    confirmDialogDontSave();
  }

  /* 
  --- Utility methods  ---------------------------------------------------------
   */
}
