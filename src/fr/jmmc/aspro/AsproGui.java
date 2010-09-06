/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AsproGui.java,v 1.27 2010-09-06 13:39:57 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.26  2010/07/08 13:41:13  bourgesl
 * added logs
 * encapsulate swing ops in SwingUtilities.invoke...
 * show GUI only when application is ready
 *
 * Revision 1.25  2010/07/07 15:16:25  bourgesl
 * added 'New Observation' action
 *
 * Revision 1.24  2010/07/05 14:50:15  bourgesl
 * added a confirm dialog on exiting the application
 *
 * Revision 1.23  2010/06/29 12:13:21  bourgesl
 * added ExportToOIFits action
 *
 * Revision 1.22  2010/06/18 13:31:07  bourgesl
 * removed oitools integration test code
 *
 * Revision 1.21  2010/06/18 12:00:40  bourgesl
 * added oitools module
 *
 * Revision 1.20  2010/06/17 10:02:51  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.19  2010/06/10 08:52:53  bourgesl
 * modified minimum delay for tooltips to 250ms
 *
 * Revision 1.18  2010/06/09 12:49:03  bourgesl
 * use an hidden preference to show the splashscreen
 * added Export OB and PDF actions
 *
 * Revision 1.17  2010/06/08 14:17:15  bourgesl
 * commented the code that tells LAF to use small variant for JComponents
 *
 * Revision 1.16  2010/06/08 13:41:48  bourgesl
 * use UI defaults to change default component size to small (mac os & nimbus)
 *
 * Revision 1.15  2010/05/19 12:49:49  bourgesl
 * added standard frame icon
 * removed application version in the frame title
 *
 * Revision 1.14  2010/05/12 08:44:10  mella
 * Add one preferences window first to choose the default style of display for positions
 *
 * Revision 1.13  2010/05/11 09:48:47  bourgesl
 * removed SwingUtilities import
 *
 * Revision 1.12  2010/05/07 11:35:19  bourgesl
 * application starts not in EDT to display the splash screen (EDT wait and refresh issues) : solve later
 *
 * Revision 1.11  2010/04/13 14:20:55  bourgesl
 * the application is launched using the EDT to be compliant with Swing constraints
 *
 * Revision 1.10  2010/04/08 14:04:27  bourgesl
 * customized ToolTipManager timeouts
 *
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
import fr.jmmc.aspro.gui.action.ExportOBAction;
import fr.jmmc.aspro.gui.action.ExportOIFitsAction;
import fr.jmmc.aspro.gui.action.ExportPDFAction;
import fr.jmmc.aspro.gui.action.LoadObservationAction;
import fr.jmmc.aspro.gui.action.NewObservationAction;
import fr.jmmc.aspro.gui.action.SaveObservationAction;
import fr.jmmc.aspro.gui.action.ShowPrefAction;
import fr.jmmc.aspro.gui.util.ComponentResizeAdapter;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.mcs.gui.App;
import fr.jmmc.mcs.gui.FeedbackReport;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.util.ActionRegistrar;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Locale;
import java.util.TimeZone;
import java.util.logging.Level;
import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;

/**
 * This class represents the Aspro GUI application
 * @author bourgesl
 */
public final class AsproGui extends App {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.AsproGui";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* members */
  /** Setting Panel */
  private SettingPanel settingPanel;

  /**
   * Public constructor with command line arguments
   * @param args command line arguments
   */
  public AsproGui(final String[] args) {
    // no splash screen during the developments :
    super(args, false, Preferences.getInstance().IsShowSplashScreen());
  }

  /**
   * Initialize application objects
   * @param args ignored arguments
   */
  @Override
  protected void init(final String[] args) {
    logger.fine("AsproGui.init() handler : enter");

    // Preload configurations :
    ConfigurationManager.getInstance();

    try {
      // Using invokeAndWait to be in sync with the main thread :
      SwingUtilities.invokeAndWait(new Runnable() {

        /**
         * Initializes the swing components with their actions in EDT
         */
        public void run() {
          prepareFrame(getFrame());
        }
      });
    } catch (Exception e) {
      logger.log(Level.SEVERE, "failure : ", e);
      // report unacceptable failure :
      new FeedbackReport(null, true, e);
    }

    logger.fine("AsproGui.init() handler : exit");
  }

  /** 
   * Execute application body = nothing to do
   */
  @Override
  protected void execute() {
    logger.fine("AsproGui.execute() handler called.");
  }

  /**
   * Application is ready = make the application frame visible
   */
  @Override
  protected void ready() {
    logger.fine("AsproGui.ready() handler called.");

    SwingUtilities.invokeLater(new Runnable() {

      /**
       * Show the application frame using EDT
       */
      public void run() {
        logger.fine("AsproGui.ready : handler called.");
        getFrame().setVisible(true);
      }
    });
  }

  /**
   * Hook to handle operations before closing application.
   *
   * @return should return true if the application can exit, false otherwise
   * to cancel exit.
   */
  @Override
  protected boolean finish() {
    logger.fine("AsproGui.finish() handler called.");

    // Ask the user if he wants to save modifications
    final Object[] options = {"Save", "Cancel", "Don't Save"};
    final int result = JOptionPane.showOptionDialog(getFrame(),
            "Do you want to save changes before closing ?\nIf you don't save, your changes will be lost.\n\n",
            null, JOptionPane.DEFAULT_OPTION, JOptionPane.WARNING_MESSAGE, null, options,
            options[0]);

    // If the User clicked the "Save" button, save and quit
    if (result == 0) {
      final AbstractAction action = (SaveObservationAction) ActionRegistrar.getInstance().get(SaveObservationAction.className, SaveObservationAction.actionName);

      action.actionPerformed(null);
    }
    // If the user clicked the "Cancel" button, don't quit
    if (result == 1) {
      return false;
    }

    // If the user clicked the "Don't Save" button, quit
    return true;
  }

  /**
   * Prepare the frame widgets and define its minimum size
   * @param frame
   */
  private void prepareFrame(final JFrame frame) {
    logger.fine("prepareFrame : enter");

    frame.setTitle(App.getSharedApplicationDataModel().getProgramName());

    // handle frame icon
    frame.setIconImage(new ImageIcon(getClass().getResource("/fr/jmmc/mcs/gui/favicon.png")).getImage());

    // get screen size to adjust minimum window size :
    final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

    if (logger.isLoggable(Level.INFO)) {
      logger.info("screen size = " + screenSize.getWidth() + " x " + screenSize.getHeight());
    }

    // hack for 1024x768 screens :
    final int appWidth = 950;
    final int appHeight = (screenSize.getHeight() <= 768) ? 700 : 800;

    final Dimension dim = new Dimension(appWidth, appHeight);
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
    getFramePanel().add(new StatusBar(), BorderLayout.SOUTH);

    StatusBar.show("application started.");

    logger.fine("prepareFrame : exit");
  }

  /**
   * Create the main content i.e. the setting panel
   */
  private void createContent() {
    this.settingPanel = new SettingPanel();

    // adds the panel in scrollPane
    getFramePanel().add(new JScrollPane(this.settingPanel), BorderLayout.CENTER);
  }

  /**
   * Create the main actions present in the menu bar
   */
  private void registerActions() {
    // new observation :
    new NewObservationAction();
    // load observation :
    new LoadObservationAction();
    // save observation :
    new SaveObservationAction();
    // show preferences :
    new ShowPrefAction();
    // export OB :
    new ExportOBAction();
    // export PDF :
    new ExportPDFAction();
    // export OIFits :
    new ExportOIFitsAction();
  }

  /**
   * Return the setting panel
   * @return setting panel
   */
  public SettingPanel getSettingPanel() {
    return this.settingPanel;
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

    // Change Swing defaults :
    changeSwingDefaults();

    // Start application with the command line arguments
    new AsproGui(args);
  }

  /**
   * Change several default values for Swing rendering.
   */
  private static void changeSwingDefaults() {

    // Force Locale for Swing Components :
    JComponent.setDefaultLocale(Locale.US);

    // Let the tooltip stay longer (60s) :
    ToolTipManager.sharedInstance().setInitialDelay(250);
    ToolTipManager.sharedInstance().setDismissDelay(60000);
  }
}
