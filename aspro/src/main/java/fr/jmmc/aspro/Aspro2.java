/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro;

import fr.jmmc.aspro.gui.PreferencePanel;
import fr.jmmc.aspro.gui.SettingPanel;
import fr.jmmc.aspro.gui.action.AsproExportAction;
import fr.jmmc.aspro.gui.action.ExportAllOBAction;
import fr.jmmc.aspro.gui.action.ExportOBAction;
import fr.jmmc.aspro.gui.action.ExportOIFitsAction;
import fr.jmmc.aspro.gui.action.ExportVOTableAction;
import fr.jmmc.aspro.gui.action.ImportTargetsAction;
import fr.jmmc.aspro.gui.action.ImportVOTableAction;
import fr.jmmc.aspro.gui.action.LoadObservationAction;
import fr.jmmc.aspro.gui.action.NewObservationAction;
import fr.jmmc.aspro.gui.action.SaveObservationAction;
import fr.jmmc.aspro.gui.action.TargetEditorAction;
import fr.jmmc.aspro.gui.task.AsproTaskRegistry;
import fr.jmmc.aspro.interop.BroadcastToModelFittingAction;
import fr.jmmc.aspro.interop.ObservationSampMessageHandler;
import fr.jmmc.aspro.interop.SearchCalQueryAction;
import fr.jmmc.aspro.interop.SendOBAction;
import fr.jmmc.aspro.interop.SendOIFitsAction;
import fr.jmmc.aspro.interop.SendObservationAction;
import fr.jmmc.aspro.interop.SendVOTableAction;
import fr.jmmc.aspro.interop.StarListSendAction;
import fr.jmmc.aspro.interop.VotableSampMessageHandler;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.data.MimeType;
import fr.jmmc.jmcs.data.app.ApplicationDescription;
import fr.jmmc.jmcs.gui.PreferencesView;
import fr.jmmc.jmcs.gui.action.ShowReleaseNotesAction;
import fr.jmmc.jmcs.gui.component.ComponentResizeAdapter;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.MessagePane.ConfirmSaveChanges;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.gui.task.TaskSwingWorkerExecutor;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.logging.LoggingService;
import fr.jmmc.jmcs.gui.util.ResourceImage;
import fr.jmmc.jmcs.util.concurrent.ParallelJobExecutor;
import fr.jmmc.oiexplorer.core.model.OIFitsCollectionManager;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Toolkit;
import java.util.LinkedHashMap;
import java.util.Map;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class represents the Aspro GUI application
 * @author bourgesl
 */
public final class Aspro2 extends App {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(Aspro2.class.getName());
    /** CLI arg - process */
    public final static String ARG_PROCESS = "process";

    /* members */
    /** Setting Panel */
    private SettingPanel settingPanel;
    /** Save action */
    private SaveObservationAction saveAction;

    /**
     * Main entry point : use swing setup and then launch the application
     * @param args command line arguments
     */
    public static void main(final String[] args) {
        // Start application with the command line arguments
        Bootstrapper.launchApp(new Aspro2(args));
    }

    /**
     * Return the Aspro GUI singleton
     * @return Aspro GUI singleton
     */
    public static Aspro2 getInstance() {
        return (Aspro2) App.getInstance();
    }

    /**
     * Public constructor with command line arguments
     * @param args command line arguments
     */
    public Aspro2(final String[] args) {
        super(args);
    }

    /**
     * Initialize services (main thread)
     * @throws IllegalStateException if the configuration files are not found or IO failure
     * @throws IllegalArgumentException if the load configuration failed
     */
    @Override
    protected void initServices() throws IllegalStateException, IllegalArgumentException {
        logger.debug("Aspro2.initServices: handler enter");

        // Preload configurations :
        ConfigurationManager.getInstance();

        // Create OIFitsCollectionManager at startup (JAXB factory, event queues and PlotDefinitionFactory ...)
        // to avoid OpenJDK classloader issues (ie use main thread):
        OIFitsCollectionManager.getInstance();

        // Initialize tasks and the task executor :
        AsproTaskRegistry.getInstance();
        TaskSwingWorkerExecutor.start();

        // Initialize the parallel job executor:
        ParallelJobExecutor.getInstance();

        // Initialize warning log:
        LoggingService.getInstance().addLogMapper("Warning messages", AsproConstants.ASPRO_WARNING_LOG, "WARNINGLOG");

        // TODO: decide to shift colors for baselines (detailled observability):
//        SharedSeriesAttributes.INSTANCE.setOffsetIdx(StarObservabilityData.TYPE_BASE_LINE);
        logger.debug("Aspro2.initServices: handler exit");
    }

    /**
     * Setup GUI (Swing EDT)
     * @throws RuntimeException 
     */
    @Override
    protected void setupGui() throws RuntimeException {
        logger.debug("Aspro2.setupGui() handler enter");

        prepareFrame();

        if (!Bootstrapper.isHeadless()) {
            createPreferencesView();
        }

        // Create a new observation and update the GUI :
        // even if opening a file in case the file can not be loaded:
        ObservationManager.getInstance().reset();

        logger.debug("Aspro2.setupGui() handler exit");
    }

    /**
     * Create the Preferences view
     * @return Preferences view
     */
    public static PreferencesView createPreferencesView() {
        // Retrieve application preferences and attach them to their view
        // (This instance must be instanciated after dependencies)
        final LinkedHashMap<String, JPanel> panels = new LinkedHashMap<String, JPanel>(2);
        panels.put("General settings", new PreferencePanel());

        final PreferencesView preferencesView = new PreferencesView(App.getFrame(), Preferences.getInstance(), panels);
        preferencesView.init();

        return preferencesView;
    }

    /**
     * Execute application body = make the application frame visible
     */
    @Override
    protected void execute() {
        SwingUtils.invokeLaterEDT(new Runnable() {
            /**
             * Show the application frame using EDT
             */
            @Override
            public void run() {
                logger.debug("Aspro2.execute() handler called.");

                // headless mode:
                final JFrame appFrame = App.getExistingFrame();
                if (appFrame != null) {
                    appFrame.setVisible(true);
                }
            }
        });
    }

    /**
     * Return the application state when submitting a feedback report
     *
     * @return application state as String
     */
    @Override
    public String getStateForFeedbackReport() {
        // serialize observation to xml :
        return "Current ObservationSetting:\n" + ObservationManager.getInstance().saveToString();
    }

    /**
     * Hook to handle operations before closing application.
     *
     * @return should return true if the application can exit, false otherwise
     * to cancel exit.
     */
    @Override
    public boolean canBeTerminatedNow() {
        logger.debug("Aspro2.canBeTerminatedNow() handler called.");

        return checkAndConfirmSaveChanges("closing");
    }

    /**
     * Check if the current observation was changed; if true, the user is asked to save changes
     *
     * @param beforeMessage part of the message inserted after 'before ' ?
     * @return should return true if the application can continue, false otherwise to cancel any operation.
     */
    public boolean checkAndConfirmSaveChanges(final String beforeMessage) {
        final boolean changed = ObservationManager.getInstance().isMainObservationChanged();
        if (logger.isDebugEnabled()) {
            logger.debug("changed: {}", changed);
        }

        // Ask the user if he wants to save modifications
        final ConfirmSaveChanges result = (changed) ? MessagePane.showConfirmSaveChanges(beforeMessage) : ConfirmSaveChanges.Ignore;

        // Handle user choice
        switch (result) {
            // If the user clicked the "Save" button, save and continue
            case Save:
                if (this.saveAction != null) {
                    return this.saveAction.save();
                }
                break;

            // If the user clicked the "Don't Save" button, continue
            case Ignore:
                break;

            // If the user clicked the "Cancel" button or pressed 'esc' key, don't continue
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
    public void cleanup() {
        if (getSettingPanel() != null) {
            // dispose Swing SettingPanel:
            getSettingPanel().dispose();
        }
    }

    /**
     * Prepare the frame widgets and define its minimum size
     */
    private void prepareFrame() {
        logger.debug("prepareFrame : enter");

        // initialize the actions :
        registerActions();

        final Container container;

        if (Bootstrapper.isHeadless()) {
            container = null;
        } else {
            final JFrame frame = new JFrame(ApplicationDescription.getInstance().getProgramName());

            // handle frame icon
            final Image jmmcFavImage = ResourceImage.JMMC_FAVICON.icon().getImage();
            frame.setIconImage(jmmcFavImage);

            // get screen size to adjust minimum window size :
            final Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();

            logger.info("screen size = {} x {}", screenSize.getWidth(), screenSize.getHeight());

            // hack for screens smaller than 1152x864 screens :
            final int appWidth = 950;
            final int appHeightMin = 700;
            final int appHeightPref = (screenSize.getHeight() >= 864) ? 800 : appHeightMin;

            final Dimension dim = new Dimension(appWidth, appHeightMin);
            frame.setMinimumSize(dim);
            frame.setPreferredSize(new Dimension(appWidth, appHeightPref));
            frame.addComponentListener(new ComponentResizeAdapter(dim));

            App.setFrame(frame);

            container = frame.getContentPane();
        }
        // setup Gui the main panel :
        createContent(container);

        StatusBar.show("application started.");

        logger.debug("prepareFrame : exit");
    }

    /**
     * Create the main content i.e. the setting panel
     * @param container frame's panel
     */
    private void createContent(final Container container) {
        if (container != null) {
            this.settingPanel = new SettingPanel();
            this.settingPanel.setName("settingPanel");

            // adds the panel in scrollPane
            container.add(new JScrollPane(this.settingPanel), BorderLayout.CENTER);

            // Handle status bar
            container.add(StatusBar.getInstance(), BorderLayout.SOUTH);
        }
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
        // import Targets:
        new ImportTargetsAction();
        // import VOTable:
        new ImportVOTableAction();
        // export VOTable:
        new ExportVOTableAction();
        // export OB :
        new ExportOBAction(false);
        new ExportOBAction(true);
        // export ALl OB :
        new ExportAllOBAction(false);
        new ExportAllOBAction(true);
        // export actions:
        new AsproExportAction(MimeType.PDF);
        new AsproExportAction(MimeType.PNG);
        // export OIFits :
        new ExportOIFitsAction();

        // Edit menu :
        // target editor :
        new TargetEditorAction();

        // Interop menu :
        // send observation (SAMP) :
        new SendObservationAction();
        // use interop with LITpro (SAMP) :
        new BroadcastToModelFittingAction();
        // searchCal query (SAMP) :
        new SearchCalQueryAction();

        // PIVOT starlist (SAMP) :
        new StarListSendAction();

        // Send OIFits (SAMP) :
        new SendOIFitsAction();

        // Send VOTable (SAMP) :
        new SendVOTableAction();

        // Send OB (SAMP) :
        new SendOBAction();

        // Help menu:
        new ShowReleaseNotesAction("showConf", "Aspro2 Configuration " + ConfigurationManager.getInstance().getConfDescription().getProgramVersion(), ConfigurationManager.getInstance().getConfDescription());
    }

    /**
     * Create SAMP Message handlers
     */
    @Override
    protected void declareInteroperability() {
        // Add handler to load votable (any target list or calibrators)
        new VotableSampMessageHandler();
        // Add handler to load observations (targets):
        new ObservationSampMessageHandler();
    }

    /**
     * Return the setting panel
     * @return setting panel
     */
    public SettingPanel getSettingPanel() {
        return this.settingPanel;
    }

    /**
     * Add custom command line arguments for the OIFitsProcessor
     */
    @Override
    protected void defineCustomCommandLineArgumentsAndHelp() {
        addCustomCommandLineArgument(ARG_PROCESS, false, "process an OIFITS file and model to compute observables", App.ExecMode.TTY);
        // Append Process arguments:
        OIFitsProcessor.defineCommandLineArguments(this);
    }

    /**
     * check the arguments given by the user in TTY mode
     * and performs the requested action (process)
     * Note: executed by the thread [main]: must block until asynchronous task finishes !
     * @throws IllegalArgumentException if one (or several) argument is missing or invalid
     */
    @Override
    protected void processShellCommandLine() throws IllegalArgumentException {
        final Map<String, String> argValues = getCommandLineArguments();
        logger.debug("processShellCommandLine: {}", argValues);

        if (argValues.get(ARG_PROCESS) != null) {
            // Process action:
            OIFitsProcessor.processCommandLine(this, argValues);
        }
        logger.debug("processShellCommandLine: done.");
    }

    /**
     * Update the title of the main window
     * @param fileName optional file name
     */
    public static void updateFrameTitle(final String fileName) {
        // See SampManager that adds SAMP client '[c?]'

        // Update the main frame title:
        final JFrame frame = App.getExistingFrame();
        if (frame != null) {
            final String oldTitle = frame.getTitle();
            final int pos = oldTitle.indexOf('[');
            final String suffix = (pos != -1) ? oldTitle.substring(pos, oldTitle.length()) : "";

            frame.setTitle(ApplicationDescription.getInstance().getProgramName()
                    + ((fileName != null) ? " - " + fileName : "")
                    + " " + suffix);
        }
    }
}
