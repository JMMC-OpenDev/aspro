/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AsproGui.java,v 1.10 2010-04-08 14:04:27 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.9  2010/04/02 09:21:49  bourgesl
 * updated javadoc
 *
 * Revision 1.8  2010/02/12 15:53:18  bourgesl
 * added target model editor
 *
 * Revision 1.7  2010/01/20 16:18:38  bourgesl
 * observation form refactoring
 *
 * Revision 1.6  2010/01/08 16:51:18  bourgesl
 * initial uv coverage
 *
 * Revision 1.5  2009/12/04 16:26:58  bourgesl
 * Added Load action in the menu bar (partially handled)
 *
 * Revision 1.4  2009/12/04 15:37:32  bourgesl
 * fixed application startup using the App frame instead of MainFrame
 *
 * Revision 1.3  2009/11/03 16:57:56  bourgesl
 * added observability plot with LST/UTC support containing only day/night/twilight zones
 *
 * Revision 1.2  2009/10/02 15:20:18  bourgesl
 * updated model + stupid tree model
 *
 * Revision 1.1  2009/09/21 15:38:50  bourgesl
 * initial jmcs gui + jaxb loader
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro;

import fr.jmmc.aspro.gui.SettingPanel;
import fr.jmmc.aspro.gui.action.LoadObservationAction;
import fr.jmmc.aspro.gui.action.SaveObservationAction;
import fr.jmmc.aspro.gui.util.ComponentResizeAdapter;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.mcs.gui.App;
import fr.jmmc.mcs.gui.StatusBar;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Locale;
import java.util.TimeZone;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.ToolTipManager;

/**
 * This class represents the Aspro GUI application
 * @author bourgesl
 */
public class AsproGui extends App {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.AsproGui";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* members */
  /* Swing Components */
  /** Status Bar */
  protected StatusBar statusBar;

  /**
   * Return AsproGui singleton
   * @return AsproGui singleton
   */
  public static AsproGui getInstance() {
    return (AsproGui) App.getSharedInstance();
  }

  /**
   * Public constructor with command line arguments
   * @param args command line arguments
   */
  public AsproGui(final String[] args) {
    // no splash screen during the developments :
    super(args, false, !AsproConstants.DEBUG_MODE);
  }

  /**
   * Initialize application objects
   * @param args ignored arguments
   */
  @Override
  protected void init(final String[] args) {
    logger.fine("init : enter");

    // Preload configurations :
    ConfigurationManager.getInstance();

    // Initializes the swing components with their actions :
    prepareFrame(getRootFrame());

    logger.fine("init : exit");
  }

  /**
   * Return the application frame
   * @return application frame
   */
  public JFrame getRootFrame() {
    return (JFrame) getFrame();
  }

  /** 
   * Execute application body = make the application frame visible
   */
  @Override
  protected void execute() {
    getRootFrame().setVisible(true);
  }

  /**
   * Prepare the frame widgets and define its minimum size
   * @param frame
   */
  private void prepareFrame(final JFrame frame) {
    frame.setTitle(App.getSharedApplicationDataModel().getProgramName() + " v" + App.getSharedApplicationDataModel().getProgramVersion());

    final Dimension dim = new Dimension(900, 750);
    frame.setMinimumSize(dim);
    frame.addComponentListener(new ComponentResizeAdapter(dim));

    // handle closing by mouse :
    frame.addWindowListener(new CloseFrameAdapter());

    // previous adapter manages the windowClosing(event) :
    frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

    // init the main panel :
    createContent();

    // initialize the actions :
    registerActions();

    // Handle status bar
    statusBar = new StatusBar();
    getFramePanel().add(statusBar, BorderLayout.SOUTH);

    StatusBar.show("application started.");
  }

  /**
   * Create the main content i.e. the setting panel
   */
  private void createContent() {

    // adds the panel in scrollPane
    final JScrollPane settingScrollPanel = new JScrollPane(new SettingPanel());

    getFramePanel().add(settingScrollPanel, BorderLayout.CENTER);
  }

  /**
   * Create the main actions present in the menu bar
   */
  private void registerActions() {
    // load observation :
    new LoadObservationAction();
    // save observation :
    new SaveObservationAction();
  }

  /**
   * Window adapter to handle windowClosing event.
   */
  private static final class CloseFrameAdapter extends WindowAdapter {

    @Override
    public void windowClosing(final WindowEvent e) {
      // callback on exit :
      App.quitAction().actionPerformed(null);
    }
  }

  /**
   * Main entry point : define the locale to US / GMT and then start the application
   * @param args command line arguments
   */
  public static void main(final String[] args) {
    // Set the default locale to en-US locale (for Numerical Fields "." ",")
    Locale.setDefault(Locale.US);
    // Set the default timezone to GMT to handle properly the date in UTC :
    TimeZone.setDefault(TimeZone.getTimeZone("GMT"));

    // force Locale for Swing Components :
    JComponent.setDefaultLocale(Locale.US);

    // let the tooltip stay longer (30s) :
    ToolTipManager.sharedInstance().setInitialDelay(100);
    ToolTipManager.sharedInstance().setDismissDelay(30000);

    // Start application with the command line arguments
    new AsproGui(args);
  }
}
