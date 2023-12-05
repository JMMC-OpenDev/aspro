/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro;

import fr.jmmc.aspro.gui.PreferencePanel;
import fr.jmmc.aspro.gui.SettingPanel;
import fr.jmmc.aspro.gui.action.AsproExportAction;
import fr.jmmc.aspro.gui.action.ConfigurationEditorAction;
import fr.jmmc.aspro.gui.action.ExportAllOBAction;
import fr.jmmc.aspro.gui.action.ExportOBAction;
import fr.jmmc.aspro.gui.action.ExportOIFitsAction;
import fr.jmmc.aspro.gui.action.ExportVOTableAction;
import fr.jmmc.aspro.gui.action.GetStarAction;
import fr.jmmc.aspro.gui.action.ImportTargetsAction;
import fr.jmmc.aspro.gui.action.ImportVOTableAction;
import fr.jmmc.aspro.gui.action.LoadObservationAction;
import fr.jmmc.aspro.gui.action.NewObservationAction;
import fr.jmmc.aspro.gui.action.QueryAllRawObservationsAction;
import fr.jmmc.aspro.gui.action.QueryOneRawObservationsAction;
import fr.jmmc.aspro.gui.action.SaveObservationAction;
import fr.jmmc.aspro.gui.action.TargetEditorAction;
import fr.jmmc.aspro.gui.action.TargetUpdateSimbadAction;
import fr.jmmc.aspro.gui.task.AsproTaskRegistry;
import fr.jmmc.aspro.interop.BroadcastToModelFittingAction;
import fr.jmmc.aspro.interop.ImageSampMessageHandler;
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
import fr.jmmc.oitools.model.DataModel;
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionListener;
import java.util.LinkedHashMap;
import java.util.Map;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
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

        // Initialize tasks and the task executor :
        TaskSwingWorkerExecutor.start(2); // 2 threads (1 computation and 1 image viewer)

        // Initialize the parallel job executor:
        ParallelJobExecutor.getInstance();

        // Initialize warning log:
        LoggingService.getInstance().addLogMapper("Warning messages", AsproConstants.ASPRO_WARNING_LOG, "WARNINGLOG");

        // TODO: decide to shift colors for baselines (detailled observability):
//        SharedSeriesAttributes.INSTANCE.setOffsetIdx(StarObservabilityData.TYPE_BASE_LINE);

        // Enable OI columns for OIFits datamodel
        DataModel.setOiModelColumnsSupport(true);

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

            // Create a new observation and update the GUI :
            // even if opening a file in case the file can not be loaded:
            ObservationManager.getInstance().reset();
        }

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
     * Optional hook to override in your App, to tell App to open last recent file at startup.
     * @return true to open last recent file at startup.
     */
    @Override
    protected boolean shouldOpenFirstFileAtStartup() {
        return true;
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
            // hack for screens smaller than 1024x768 screens:
            final int appWidth = 950;
            final int appHeightMin = 700;
            final int appHeightPref = (screenSize.getHeight() >= 1000) ? 950 : appHeightMin;

            final Dimension dim = new Dimension(appWidth, appHeightMin);
            frame.setMinimumSize(dim);
            frame.addComponentListener(new ComponentResizeAdapter(dim));
            frame.setPreferredSize(new Dimension(appWidth, appHeightPref));

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
            container.add(settingPanel, BorderLayout.CENTER);

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
        // Target editor :
        new TargetEditorAction();
        // Configuration editor
        new ConfigurationEditorAction();
        // update simbad:
        new TargetUpdateSimbadAction();
        // get star:
        new GetStarAction();
        // query observations:
        new QueryOneRawObservationsAction();
        new QueryAllRawObservationsAction();

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
        final ConfigurationManager cm = ConfigurationManager.getInstance();
        new ShowReleaseNotesAction("showConf", "Aspro2 Configuration " + cm.getConfDescription().getProgramVersion(), cm.getConfDescription());
    }

    /**
     * Create SAMP Message handlers
     */
    @Override
    protected void declareInteroperability() {
        // Add handler to load votables (any target list or calibrators)
        new VotableSampMessageHandler();
        // Add handler to load observations (targets):
        new ObservationSampMessageHandler();
        // Add handler to load fits images:
        new ImageSampMessageHandler();
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

    /**
     * Create a generic progress panel (typically shown in overlay)
     *
     * @param message message displayed as tooltip
     * @param progressBar progress bar to use
     * @param cancelListener optional cancel action listener
     * @return new panel
     */
    public static JPanel createProgressPanel(final String message, final JProgressBar progressBar, final ActionListener cancelListener) {
        final JPanel progressPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 4, 0));
        progressPanel.setBorder(BorderFactory.createEtchedBorder());
        progressPanel.setToolTipText(message);

        final Dimension dim = new Dimension(80, 18);
        progressBar.setMinimumSize(dim);
        progressBar.setPreferredSize(dim);
        progressBar.setMaximumSize(dim);

        progressBar.setStringPainted(true);
        progressPanel.add(progressBar);

        if (cancelListener != null) {
            final JButton cancelBtn = new JButton("cancel");
            cancelBtn.setMargin(new Insets(0, 2, 0, 2));
            cancelBtn.addActionListener(cancelListener);
            progressPanel.add(cancelBtn);
        }

        return progressPanel;
    }

}
