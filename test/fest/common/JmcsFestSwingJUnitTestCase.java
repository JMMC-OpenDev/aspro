/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: JmcsFestSwingJUnitTestCase.java,v 1.2 2011-03-14 14:47:18 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2011/03/11 12:55:35  bourgesl
 * added fest-swing test cases for Aspro 2
 *
 */
package fest.common;

import static org.fest.swing.launcher.ApplicationLauncher.*;
import fr.jmmc.mcs.gui.App;
import java.util.Arrays;
import java.util.logging.Level;
import org.fest.swing.core.EmergencyAbortListener;
import org.fest.swing.fixture.FrameFixture;
import org.junit.AfterClass;
import org.junit.BeforeClass;

/**
 * This class extends FestSwingCustomJUnitTestCase to start / stop one Jmcs application
 * @author bourgesl
 */
public class JmcsFestSwingJUnitTestCase extends FestSwingCustomJUnitTestCase {

  /* members */
  /** application window fixture */
  protected FrameFixture window;
  /** emergency abort listener associated to 'Ctrl + Shift + A' key combination */
  private EmergencyAbortListener listener;

  /**
   * Public constructor required by JUnit
   */
  public JmcsFestSwingJUnitTestCase() {
    super();
  }

  /**
   * Set up the Swing application.
   * This method is called <strong>after</strong> executing <code>{@link #setUpOnce()}</code>.
   */
  @BeforeClass
  public static void onSetUpOnce() {

    // Use main thread to start Jmcs application using subclass.main() :
    if (App.getSharedInstance() == null) {

      // disable use of System.exit() :
      App.setAvoidSystemExit(true);

      if (JmcsApplicationSetup.applicationClass != null) {
        if (logger.isLoggable(Level.INFO)) {
          logger.info("onSetUpOnce : starting application : " + JmcsApplicationSetup.applicationClass);
        }

        if (JmcsApplicationSetup.arguments != null) {
          if (logger.isLoggable(Level.INFO)) {
            logger.info("onSetUpOnce : using arguments      : " + Arrays.toString(JmcsApplicationSetup.arguments));
          }

          application(JmcsApplicationSetup.applicationClass).withArgs(JmcsApplicationSetup.arguments).start();

        } else {

          application(JmcsApplicationSetup.applicationClass).start();
        }
      }

      if (logger.isLoggable(Level.INFO)) {
        logger.info("onSetUpOnce : started application = " + App.getSharedInstance());
      }

      if (App.getSharedInstance() == null) {
        throw new RuntimeException("unable to start application : " + JmcsApplicationSetup.applicationClass);
      }
    }
  }

  /**
   * Free other resources like the Swing application.
   * This method is called <strong>after</strong> executing <code>{@link #tearDownOnce()}</code>.
   */
  @AfterClass
  public static void onTearDownOnce() {
    final App app = App.getSharedInstance();
    if (app != null) {
      if (logger.isLoggable(Level.INFO)) {
        logger.info("onTearDownOnce : exit application = " + app);
      }

      app.onFinish();
    }
  }

  /**
   * Return the application
   * @return application
   */
  protected static App getApplication() {
    return App.getSharedInstance();
  }

  /**
   * Define the window fixture before each JUnit Test method.
   */
  @Override
  protected void onSetUp() {
    listener = EmergencyAbortListener.registerInToolkit();

    // IMPORTANT: note the call to 'robot()'
    // we must use the Robot from FestSwingCustomTestCaseTemplate
    window = new FrameFixture(robot(), App.getFrame());

    // shows the frame to test
    window.show();
    window.moveToFront();
  }

  /**
   * Clean up resources of the window fixture after each JUnit Test method.
   */
  @Override
  protected void onTearDown() {
    listener.unregister();

    window.cleanUp();
  }
}
