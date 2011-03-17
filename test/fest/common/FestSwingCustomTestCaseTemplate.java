/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: FestSwingCustomTestCaseTemplate.java,v 1.3 2011-03-17 15:41:49 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2011/03/14 14:46:56  bourgesl
 * added defineRobotDelayBetweenEvents(millis) to speed up or slow down the robot animation
 *
 * Revision 1.1  2011/03/11 12:55:35  bourgesl
 * added fest-swing test cases for Aspro 2
 *
 */
package fest.common;

import org.fest.swing.core.BasicRobot;
import org.fest.swing.core.Robot;
import org.fest.swing.core.Settings;

/**
 * This custom FestSwingTestCaseTemplate modifies the robot behaviour :
 * use
 * @see BasicRobot#robotWithCurrentAwtHierarchy()
 * instead of
 * @see BasicRobot#robotWithNewAwtHierarchy()
 *
 * @author bourgesl
 *
 * Original header :
 * 
 * Understands a template for test cases that use FEST-Swing.
 * @since 1.1
 *
 * @author Alex Ruiz
 *
 * Created on Jan 17, 2009
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 *
 * Copyright @2009-2010 the original author or authors.
 */
public abstract class FestSwingCustomTestCaseTemplate {

  /** millisecond count in between generated events */
  private static int DELAY_BETWEEN_EVENTS = 50;
  /** milliseconds before checking for idle */
  private static int EVENT_POSTING_DELAY = 5 * DELAY_BETWEEN_EVENTS / 3;

  /* members */
  /** robot instance */
  private Robot robot = null;

  /**
   * Public constructor required by JUnit
   */
  public FestSwingCustomTestCaseTemplate() {
    super();
  }

  /**
   * Define the robot delays (@see #DELAY_BETWEEN_EVENTS and #EVENT_POSTING_DELAY)
   * @param delay in milliseconds
   */
  protected static void defineRobotDelayBetweenEvents(final int delay) {
    int effectiveDelay = delay;
    if (effectiveDelay < 50) {
      effectiveDelay = 50;
    }
    DELAY_BETWEEN_EVENTS = effectiveDelay;
    EVENT_POSTING_DELAY = 5 * DELAY_BETWEEN_EVENTS / 3;
  }

  /**
   * Creates this test's <code>{@link Robot}</code> using the current AWT hierarchy.
   */
  protected final void setUpRobot() {
    robot = BasicRobot.robotWithCurrentAwtHierarchy();

    // changes default delays :
    final Settings settings = robot.settings();
    settings.delayBetweenEvents(DELAY_BETWEEN_EVENTS);
    settings.eventPostingDelay(EVENT_POSTING_DELAY);
  }

  /**
   * Cleans up resources used by this test's <code>{@link Robot}</code>.
   */
  protected final void cleanUp() {
    // DO NOT dispose any open windows :
    robot.cleanUpWithoutDisposingWindows();
  }

  /**
   * Returns this test's <code>{@link Robot}</code>.
   * @return this test's <code>{@link Robot}</code>
   */
  protected final Robot robot() {
    return robot;
  }
}
