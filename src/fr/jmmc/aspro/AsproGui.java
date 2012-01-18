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
import fr.jmmc.jmcs.gui.ComponentResizeAdapter;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.interop.VotableSampMessageHandler;
import fr.jmmc.aspro.interop.StarListSendAction;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.gui.MessagePane;
import fr.jmmc.jmcs.gui.MessagePane.ConfirmSaveChanges;
import fr.jmmc.jmcs.gui.StatusBar;
import fr.jmmc.jmcs.gui.SwingSettings;
import fr.jmmc.jmcs.gui.SwingUtils;
import fr.jmmc.jmcs.gui.task.TaskSwingWorkerExecutor;
import fr.jmmc.jmcs.util.concurrent.ParallelJobExecutor;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.logging.Level;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JScrollPane;

/**
 * This class represents the Aspro GUI application
 * @author bourgesl
 */
public final class AsproGui extends App {

  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(AsproGui.class.getName());

  /* members */
  /** Setting Panel */
  private SettingPanel settingPanel;
  /** Save action */
  private SaveObservationAction saveAction;

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
    super(args);
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

    // Using invokeAndWait to be in sync with this thread :
    // note: invokeAndWaitEDT throws an IllegalStateException if any exception occurs
    SwingUtils.invokeAndWaitEDT(new Runnable() {

      /**
       * Initializes the swing components with their actions in EDT
       */
      @Override
      public void run() {
        prepareFrame(getFrame());

        // Create a new observation and update the GUI :
        // even if opening a file in case the file can not be loaded:
        ObservationManager.getInstance().reset();
      }
    });

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

    // Initialize the parallel job executor:
    ParallelJobExecutor.getInstance();
  }

  /**
   * Execute application body = make the application frame visible
   */
  @Override
  protected void execute() {
    logger.fine("AsproGui.execute() handler called.");

    SwingUtils.invokeLaterEDT(new Runnable() {

      /**
       * Show the application frame using EDT
       */
      @Override
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
    final ConfirmSaveChanges result = MessagePane.showConfirmSaveChangesBeforeClosing();

    // Handle user choice
    switch (result) {
      // If the user clicked the "Save" button, save and exit
      case Save:
        if (this.saveAction != null) {
          return this.saveAction.save();
        }
        break;

      // If the user clicked the "Don't Save" button, exit
      case Ignore:
        break;

      // If the user clicked the "Cancel" button or pressed 'esc' key, don't exit
      case Cancel:
      default: // Any other case
        return false;
    }

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

    // stop the parallel job executor:
    ParallelJobExecutor.shutdown();

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
    frame.setIconImage(new ImageIcon(getClass().getResource("/fr/jmmc/jmcs/resource/favicon.png")).getImage());

    // get screen size to adjust minimum window size :
    final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

    if (logger.isLoggable(Level.INFO)) {
      logger.info("screen size = " + screenSize.getWidth() + " x " + screenSize.getHeight());
    }

    // hack for screens smaller than 1152x864 screens :
    final int appWidth = 950;
    final int appHeightMin = 700;
    final int appHeightPref = (screenSize.getHeight() >= 864) ? 800 : appHeightMin;

    final Dimension dim = new Dimension(appWidth, appHeightMin);
    frame.setMinimumSize(dim);
    frame.setPreferredSize(new Dimension(appWidth, appHeightPref));
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
    this.saveAction = new SaveObservationAction();
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

    // PIVOT starlist (SAMP) :
    new StarListSendAction();
  }

  /**
   * Create SAMP Message handlers
   */
  @Override
  protected void declareInteroperability() {
    // Add handler to load searchCal votable and get calibrators
    new VotableSampMessageHandler();
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
