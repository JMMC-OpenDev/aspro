/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
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

      @Override
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
