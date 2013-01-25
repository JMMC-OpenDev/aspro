/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro;

import fr.jmmc.aspro.gui.SettingPanel;
import fr.jmmc.aspro.gui.action.AsproExportPDFAction;
import fr.jmmc.aspro.gui.action.ExportAllOBAction;
import fr.jmmc.aspro.gui.action.ExportOBAction;
import fr.jmmc.aspro.gui.action.ExportOIFitsAction;
import fr.jmmc.aspro.gui.action.LoadObservationAction;
import fr.jmmc.aspro.gui.action.NewObservationAction;
import fr.jmmc.aspro.gui.action.SaveObservationAction;
import fr.jmmc.aspro.gui.action.ShowPrefAction;
import fr.jmmc.aspro.gui.action.TargetEditorAction;
import fr.jmmc.aspro.gui.task.AsproTaskRegistry;
import fr.jmmc.aspro.interop.BroadcastToModelFittingAction;
import fr.jmmc.aspro.interop.SearchCalQueryAction;
import fr.jmmc.aspro.interop.StarListSendAction;
import fr.jmmc.aspro.interop.VotableSampMessageHandler;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.data.ApplicationDescription;
import fr.jmmc.jmcs.gui.component.ComponentResizeAdapter;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.component.MessagePane.ConfirmSaveChanges;
import fr.jmmc.jmcs.gui.component.StatusBar;
import fr.jmmc.jmcs.gui.task.TaskSwingWorkerExecutor;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.resource.image.ResourceImage;
import fr.jmmc.jmcs.util.concurrent.ParallelJobExecutor;
import fr.jmmc.jmcs.util.logging.LoggingService;
import fr.jmmc.oiexplorer.core.model.OIFitsCollectionManager;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Image;
import java.awt.Toolkit;
import javax.swing.JFrame;
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
        Bootstrapper.launch(new Aspro2(args));
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

    @Override
    protected void initServices() throws IllegalStateException, IllegalArgumentException {

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
    }

    @Override
    protected void setupGui() throws RuntimeException {
        logger.debug("AsproGui.init() handler : enter");

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

        logger.debug("AsproGui.init() handler : exit");
    }

    /**
     * Execute application body = make the application frame visible
     */
    @Override
    protected void execute() {
        logger.debug("AsproGui.execute() handler called.");

        SwingUtils.invokeLaterEDT(new Runnable() {
            /**
             * Show the application frame using EDT
             */
            @Override
            public void run() {
                logger.debug("AsproGui.ready : handler called.");

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
    public boolean canBeTerminatedNow() {
        logger.debug("AsproGui.finish() handler called.");

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
    public void cleanup() {
        // dispose Swing SettingPanel:
        getSettingPanel().dispose();

        // stop the task executor :
        TaskSwingWorkerExecutor.stop();

        // stop the parallel job executor:
        ParallelJobExecutor.shutdown();

        super.cleanup();
    }

    /**
     * Prepare the frame widgets and define its minimum size
     * @param frame
     */
    private void prepareFrame(final JFrame frame) {
        logger.debug("prepareFrame : enter");

        frame.setTitle(ApplicationDescription.getInstance().getProgramName());

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

        // setupGui the main panel :
        createContent();

        // initialize the actions :
        registerActions();

        // Handle status bar
        getFramePanel().add(new StatusBar(), BorderLayout.SOUTH);

        StatusBar.show("application started.");

        App.setFrame(frame);

        logger.debug("prepareFrame : exit");
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
        new AsproExportPDFAction();
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
}
