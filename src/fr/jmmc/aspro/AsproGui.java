/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro;

import fr.jmmc.aspro.gui.SettingPanel;
import fr.jmmc.aspro.interop.BroadcastToModelFittingAction;
import fr.jmmc.aspro.gui.action.ExportAllOBAction;
import fr.jmmc.aspro.gui.action.ExportOBAction;
import fr.jmmc.aspro.gui.action.ExportOIFitsAction;
import fr.jmmc.aspro.gui.action.ExportPDFAction;
import fr.jmmc.aspro.gui.action.LoadObservationAction;
import fr.jmmc.aspro.gui.action.NewObservationAction;
import fr.jmmc.aspro.interop.SearchCalQueryAction;
import fr.jmmc.aspro.gui.action.SaveObservationAction;
import fr.jmmc.aspro.gui.action.ShowPrefAction;
import fr.jmmc.aspro.gui.action.TargetEditorAction;
import fr.jmmc.aspro.gui.task.AsproTaskRegistry;
import fr.jmmc.aspro.gui.util.ComponentResizeAdapter;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.interop.SearchCalSampMessageHandler;
import fr.jmmc.mcs.gui.App;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.gui.SwingSettings;
import fr.jmmc.mcs.gui.task.TaskSwingWorkerExecutor;
import fr.jmmc.mcs.util.ActionRegistrar;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.lang.reflect.InvocationTargetException;
import java.util.logging.Level;
import javax.swing.AbstractAction;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

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
   * Main entry point : use swing setup and then start the application
   * @param args command line arguments
   */
  public static void main(final String[] args) {
    // init swing application for science
    SwingSettings.setup();

    final long start = System.nanoTime();
    try {
      // Start application with the command line arguments
      new AsproGui(args);
    } finally {
      final long time = (System.nanoTime() - start);

      if (logger.isLoggable(Level.INFO)) {
        logger.info("startup : duration = " + 1e-6d * time + " ms.");
      }
    }
  }

  /**
   * Return the Aspro Gui singleton
   * @return Aspro Gui singleton
   */
  public static AsproGui getInstance() {
    return (AsproGui) App.getSharedInstance();
  }

  /**
   * Public constructor with command line arguments
   * @param args command line arguments
   */
  public AsproGui(final String[] args) {
    super(args, false);
  }

  /**
   * Initialize application objects
   * @param args ignored arguments
   *
   * @throws RuntimeException if the AsproGui initialisation failed
   */
  @Override
  protected void init(final String[] args) throws RuntimeException {
    logger.fine("AsproGui.init() handler : enter");

    this.initServices();

    try {

      // Using invokeAndWait to be in sync with the main thread :
      SwingUtilities.invokeAndWait(new Runnable() {

        /**
         * Initializes the swing components with their actions in EDT
         */
        public void run() {
          prepareFrame(getFrame());

          // Should open a file ?
          if (!hasFileArgument()) {
            // No, create a new observation and update the GUI :
            ObservationManager.getInstance().reset();
          }
        }
      });

    } catch (InterruptedException ie) {
      // propagate the exception :
      throw new IllegalStateException("AsproGui.init : interrupted", ie);
    } catch (InvocationTargetException ite) {
      // propagate the internal exception :
      throw new IllegalStateException("AsproGui.init : exception", ite.getCause());
    }

    logger.fine("AsproGui.init() handler : exit");
  }

  /**
   * Initialize services before the GUI
   *
   * @throws IllegalStateException if the configuration files are not found or IO failure
   * @throws IllegalArgumentException if the load configuration failed
   */
  private void initServices() throws IllegalStateException, IllegalArgumentException {

    // Preload configurations :
    ConfigurationManager.getInstance();

    // Initialize tasks and the task executor :
    AsproTaskRegistry.getInstance();
    TaskSwingWorkerExecutor.start();
  }

  /**
   * Execute application body = make the application frame visible
   */
  @Override
  protected void execute() {
    logger.fine("AsproGui.execute() handler called.");

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
   * Hook to handle operations when exiting application.
   * @see App#exit(int)
   */
  @Override
  public void onFinish() {
    // stop the task executor :
    TaskSwingWorkerExecutor.stop();

    super.onFinish();
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

    // hack for screens smaller than 1152x864 screens :
    final int appWidth = 950;
    final int appHeight = (screenSize.getHeight() >= 864) ? 800 : 700;

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

    // initialize SAMP message handlers :
    declareInteroperability();

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
    this.settingPanel.setName("settingPanel");

    // adds the panel in scrollPane
    getFramePanel().add(new JScrollPane(this.settingPanel), BorderLayout.CENTER);
  }

  /**
   * Create the main actions present in the menu bar
   */
  private void registerActions() {
    // File menu :
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
    // export ALl OB :
    new ExportAllOBAction();
    // export PDF :
    new ExportPDFAction();
    // export OIFits :
    new ExportOIFitsAction();

    // Edit menu :
    // target editor :
    new TargetEditorAction();

    // Interop menu :
    // use interop with modelfitting :
    new BroadcastToModelFittingAction();
    // searchCal query (SAMP) :
    new SearchCalQueryAction();
  }

  /**
   * Create SAMP Message handlers
   */
  private void declareInteroperability() {
    // Add handler to load searchCal votable and get calibrators
    new SearchCalSampMessageHandler();
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
}
