/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: FestSwingCustomJUnitTestCase.java,v 1.1 2011-03-11 12:55:35 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fest.common;

import static org.fest.swing.timing.Pause.*;
import org.fest.swing.timing.Timeout;
import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.util.logging.Level;
import org.fest.swing.core.Robot;
import org.fest.swing.edt.FailOnThreadViolationRepaintManager;
import org.fest.swing.fixture.ComponentFixture;
import org.fest.swing.image.ScreenshotTaker;
import org.fest.swing.junit.v4_5.runner.GUITestRunner;
import org.fest.swing.security.NoExitSecurityManagerInstaller;
import org.fest.util.Files;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.runner.RunWith;

/**
 * This custom FestSwingJUnitTestCase uses :
 * - custom FestSwingTestCaseTemplate
 * - GUITestRunner
 * - NoExitSecurityManagerInstaller
 * - define a proper way to start / stop the Swing application (not individual components)
 * 
 * @author bourgesl
 *
 * Original header :
 *
 * Understands a template for test cases that use FEST-Swing and JUnit. This template installs a
 * <code>{@link FailOnThreadViolationRepaintManager}</code> to catch violations of Swing thread rules and manages both
 * creation and clean up of a <code>{@link Robot}</code>.
 * @since 1.1
 *
 * @author Alex Ruiz
 */

/*
 * Use GUI test runner to take a screenshot of a failed GUI test
 */
@RunWith(GUITestRunner.class)
public class FestSwingCustomJUnitTestCase extends FestSwingCustomTestCaseTemplate {

  /** Class logger */
  protected static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          FestSwingCustomJUnitTestCase.class.getName());
  /** 1s timeout */
  protected static final Timeout DEF_TIMEOUT = Timeout.timeout(1000l);
  /** short delay (0.1s) */
  protected static final long SHORT_DELAY = 100l;
  /** medium delay (0.5s) */
  protected static final long MEDIUM_DELAY = 500l;
  /** long delay (5s) */
  protected static final long LONG_DELAY = 5000l;
  /** screenshot taker */
  private static ScreenshotTaker screenshotTaker;
  /** screenshot folder */
  private static String screenshotFolder;

  /**
   * Public constructor required by JUnit
   */
  public FestSwingCustomJUnitTestCase() {
    super();
  }

  /**
   * Installs a <code>{@link FailOnThreadViolationRepaintManager}</code> to catch violations of Swing threading rules.
   *
   * Prepare the screenshot taker stored in the folder ./fest-screenshots/
   */
  @BeforeClass
  public static final void setUpOnce() {
    FailOnThreadViolationRepaintManager.install();

    // does not work within netbeans (fork JVM) :
    /* NoExitSecurityManagerInstaller.installNoExitSecurityManager(); */

    screenshotTaker = new ScreenshotTaker();

    File imageFolder = null;
    try {
      final String currentFolderPath = Files.currentFolder().getCanonicalPath();

      imageFolder = new File(currentFolderPath + File.separator + "fest-screenshots");

      if (imageFolder.exists()) {
        if (logger.isLoggable(Level.INFO)) {
          logger.info("setUpOnce : delete any existing file in directory = " + imageFolder);
        }

        for (File f : imageFolder.listFiles()) {
          Files.delete(f);
        }
      } else {
        imageFolder.mkdir();
      }

      screenshotFolder = imageFolder.getCanonicalPath() + File.separator;

      if (logger.isLoggable(Level.INFO)) {
        logger.info("setUpOnce : screenshot folder = " + screenshotFolder);
      }

    } catch (IOException ioe) {
      throw new RuntimeException("unable to create screenshot folder : " + imageFolder, ioe);
    }
  }

  /**
   * Uninstalls the <code>{@link NoExitSecurityManagerInstaller}</code>.
   */
  @AfterClass
  public static void tearDownOnce() {
    if (logger.isLoggable(Level.INFO)) {
      logger.info("tearDownOnce : tests done");
    }
    pause(MEDIUM_DELAY);
  }

  /**
   * Sets up this test's fixture, starting from creation of a new <code>{@link Robot}</code>.
   * @see #setUpRobot()
   * @see #onSetUp()
   */
  @Before
  public final void setUp() {
    setUpRobot();
    onSetUp();
  }

  /**
   * Subclasses need set up their own test fixture in this method. This method is called <strong>after</strong>
   * executing <code>{@link #setUp()}</code>.
   */
  protected void onSetUp() {
  }

  /**
   * Cleans up any resources used in this test. After calling <code>{@link #onTearDown()}</code>, this method cleans up
   * resources used by this test's <code>{@link Robot}</code>.
   * @see #cleanUp()
   * @see #onTearDown()
   */
  @After
  public final void tearDown() {
    try {
      onTearDown();
    } finally {
      cleanUp();
    }
  }

  /**
   * Subclasses need to clean up resources in this method. This method is called <strong>before</strong> executing
   * <code>{@link #tearDown()}</code>.
   */
  protected void onTearDown() {
  }


  /* Utility methods */
  /**
   * Sleeps for @see #SHORT_DELAY
   */
  protected static void pauseShort() {
    pause(SHORT_DELAY);
  }

  /**
   * Sleeps for @see #MEDIUM_DELAY
   */
  protected static void pauseMedium() {
    pause(MEDIUM_DELAY);
  }

  /**
   * Sleeps for @see #LONG_DELAY
   */
  protected static void pauseLong() {
    pause(LONG_DELAY);
  }

  /**
   * Takes a screenshot of the desktop and saves it as a PNG file.
   * @param fileName the file name (including the png extension)
   */
  protected static void saveScreenshot(final String fileName) {
    saveScreenshot((Component) null, fileName);
  }

  /**
   * Takes a screenshot of the given fixture and saves it as a PNG file.
   * @param fixture the given fixture to extract its component.
   * @param fileName the file name (including the png extension)
   */
  protected static void saveScreenshot(final ComponentFixture<?> fixture, final String fileName) {
    saveScreenshot(fixture.component(), fileName);
  }

  /**
   * Takes a screenshot of the given <code>{@link java.awt.Component}</code> and saves it as a PNG file.
   * @param c the given component.
   * @param fileName the file name (including the png extension)
   */
  protected static void saveScreenshot(final Component c, final String fileName) {
    final String filePath = screenshotFolder + fileName;
    try {
      pauseMedium();

      if (c == null) {
        screenshotTaker.saveDesktopAsPng(filePath);
      } else {
        screenshotTaker.saveComponentAsPng(c, filePath);
      }

      if (logger.isLoggable(Level.INFO)) {
        logger.info("Screenshot saved as " + filePath);
      }
    } catch (Exception e) {
      if (logger.isLoggable(Level.WARNING)) {
        logger.log(Level.WARNING, "Unable to take screenshot : " + filePath, e);
      }
    }
  }
}
