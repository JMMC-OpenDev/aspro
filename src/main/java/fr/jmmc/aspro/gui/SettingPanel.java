/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.event.OIFitsEvent;
import fr.jmmc.aspro.model.event.ObservationEvent;
import fr.jmmc.aspro.model.event.ObservationEventType;
import fr.jmmc.aspro.model.event.ObservationListener;
import fr.jmmc.aspro.model.event.TargetSelectionEvent;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetRawObservation;
import fr.jmmc.aspro.model.rawobs.RawObservation;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.data.app.ApplicationDescription;
import fr.jmmc.jmcs.gui.component.Disposable;
import fr.jmmc.jmcs.gui.task.TaskSwingWorkerExecutor;
import fr.jmmc.jmcs.util.ObjectUtils;
import fr.jmmc.oiexplorer.core.export.DocumentExportable;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.List;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.Timer;
import javax.swing.ToolTipManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This panel corresponds to the single observation setting panel
 * @author bourgesl
 */
public final class SettingPanel extends JPanel implements ObservationListener, Disposable {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(SettingPanel.class.getName());
    /** enable / disable Obs Plan */
    private static final boolean ENABLE_OBS_PLAN = ApplicationDescription.isAlphaOrBetaVersion();
    /** enable / disable Target table */
    private static final boolean ENABLE_TARGET_TABLE = true;
    /** default mouse cursor refresh period = 100 ms */
    private static final int REFRESH_PERIOD = 100;
    /** shared tooltip manager */
    private final static ToolTipManager tm = ToolTipManager.sharedInstance();

    /* Tab names */
    /** name of the tab pane corresponding to the observation description */
    public static final String TAB_OBS_DESC = "Notebook";
    /** name of the tab pane corresponding to the observation panel */
    public static final String TAB_OBS_PLAN = "Obs plan";
    /** name of the tab pane corresponding to the target panel */
    public static final String TAB_TARGETS = "Targets";
    /** name of the tab pane corresponding to the interferometer map */
    public static final String TAB_INTERFEROMETER_MAP = "Map";
    /** name of the tab pane corresponding to the observability panel */
    public static final String TAB_OBSERVABILITY = "Observability";
    /** name of the tab pane corresponding to the uv coverage panel */
    public static final String TAB_UV_COVERAGE = "UV coverage";
    /** name of the tab pane corresponding to the OIFits info panel (HTML view) */
    public static final String TAB_OIFITS_HTML = "OIFits info";
    /** name of the tab pane corresponding to the OIFits viewer of generated OIFits files */
    public static final String TAB_OIFITS_VIEWER = "OIFits viewer";

    /* members */
    /** timeline refresh Swing timer */
    private final Timer timerMouseCursorRefresh;
    /** basic observation form */
    private BasicObservationForm observationForm = null;
    /** observability panel */
    private ObservabilityPanel observabilityPanel = null;
    /** uv coverage panel */
    private UVCoveragePanel uvCoveragePanel = null;
    /** OIFits viewer panel */
    private OIFitsViewPanel oiFitsViewPanel = null;
    /** raw obs panel */
    private RawObservationTablePanel rawObsPanel = null;

    /** 
     * Creates new form SettingPanel
     */
    public SettingPanel() {
        initComponents();

        // Create the timeline refresh timer:
        this.timerMouseCursorRefresh = new Timer(REFRESH_PERIOD, new ActionListener() {
            /**
             * Invoked when the timer action occurs.
             */
            @Override
            public void actionPerformed(final ActionEvent ae) {
                final JFrame appFrame = App.getFrame();
                final Cursor currentCursor = appFrame.getCursor();

                final Cursor newCursor = (TaskSwingWorkerExecutor.isTaskRunning()) ? Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR)
                        : Cursor.getDefaultCursor();

                if (newCursor != currentCursor) {
                    appFrame.setCursor(newCursor);
                }
            }
        });

        postInit();

        // anyway enable mouse cursor timer:
        enableMouseCursorRefreshTimer(true);
    }

    /**
     * Start/Stop the internal mouse cursor Refresh timer
     * @param enable true to enable it, false otherwise
     */
    private void enableMouseCursorRefreshTimer(final boolean enable) {
        if (enable) {
            if (!this.timerMouseCursorRefresh.isRunning()) {
                logger.debug("Starting timer: {}", this.timerMouseCursorRefresh);

                this.timerMouseCursorRefresh.start();
            }
        } else {
            if (this.timerMouseCursorRefresh.isRunning()) {
                logger.debug("Stopping timer: {}", this.timerMouseCursorRefresh);

                this.timerMouseCursorRefresh.stop();
            }
        }
    }

    /**
     * Free any ressource or reference to this instance :
     * remove this instance form Preference Observers
     */
    @Override
    public void dispose() {
        if (logger.isDebugEnabled()) {
            logger.debug("dispose: {}", ObjectUtils.getObjectInfo(this));
        }

        // call disposable components:
        if (observabilityPanel != null) {
            observabilityPanel.dispose();
            observabilityPanel = null;
        }
        if (uvCoveragePanel != null) {
            uvCoveragePanel.dispose();
            uvCoveragePanel = null;
        }
        if (oiFitsViewPanel != null) {
            oiFitsViewPanel.dispose();
            oiFitsViewPanel = null;
        }
        rawObsPanel = null;

        // disable timeline refresh timer:
        enableMouseCursorRefreshTimer(false);
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jSplitPane = new javax.swing.JSplitPane();
        jSplitPaneMain = new javax.swing.JSplitPane();
        jScrollPaneTabbedPane = new javax.swing.JScrollPane();
        jTabbedPane = new javax.swing.JTabbedPane();

        setLayout(new java.awt.BorderLayout());

        jSplitPane.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPane.setResizeWeight(0.1);
        jSplitPane.setContinuousLayout(true);

        jSplitPaneMain.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
        jSplitPaneMain.setResizeWeight(0.9);

        jTabbedPane.setName("AsproTab"); // NOI18N
        jScrollPaneTabbedPane.setViewportView(jTabbedPane);

        jSplitPaneMain.setLeftComponent(jScrollPaneTabbedPane);

        jSplitPane.setRightComponent(jSplitPaneMain);

        add(jSplitPane, java.awt.BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents

    /**
     * This method is useful to set the models and specific features of initialized swing components :
     */
    private void postInit() {

        // Register a change listener for the tabbed panel :
        this.jTabbedPane.addChangeListener(new ChangeListener() {
            /**
             * This method is called whenever the selected tab changes
             * @param evt change event
             */
            @Override
            public final void stateChanged(final ChangeEvent evt) {
                if (observabilityPanel != null && jTabbedPane.getSelectedComponent() != observabilityPanel) {
                    // check if the BaseLine Limits are active; if true, disable the checkbox
                    observabilityPanel.disableBaseLineLimits();
                }
            }
        });

        // register this setting panel as an observation listener (listener 1) :
        ObservationManager.getInstance().register(this);

        // Add panels :
        // create the observation description form:
        final ObservationDescriptionForm obsDescForm = new ObservationDescriptionForm();
        obsDescForm.setName("obsDescForm");
        this.jTabbedPane.addTab(TAB_OBS_DESC, obsDescForm);

        // register the observation form as an observation listener :
        ObservationManager.getInstance().register(obsDescForm);

        if (ENABLE_OBS_PLAN) {
            // create the observation table:
            final ObservationTablePanel obsTablePanel = new ObservationTablePanel();
            obsTablePanel.setName("obsTablePanel");
            this.jTabbedPane.addTab(TAB_OBS_PLAN, obsTablePanel);

            // register the observation form as an observation listener :
            ObservationManager.getInstance().register(obsTablePanel);
        }

        if (ENABLE_TARGET_TABLE) {
            // create the target table:
            final TargetTablePanel targetTablePanel = new TargetTablePanel();
            targetTablePanel.setName("targetTablePanel");
            this.jTabbedPane.addTab(TAB_TARGETS, targetTablePanel);

            // register the observation form as an observation listener :
            ObservationManager.getInstance().register(targetTablePanel);
        }

        // create the map panel :
        final InterferometerMapPanel mapPanel = new InterferometerMapPanel();
        mapPanel.setName("mapPanel");
        this.jTabbedPane.addTab(TAB_INTERFEROMETER_MAP, mapPanel);

        // register the map panel as an observation listener before the observation form (listener 2) :
        ObservationManager.getInstance().register(mapPanel);

        // create the observation form that will send a changed event on the current observation (listener 3) :
        this.observationForm = new BasicObservationForm();
        observationForm.setName("observationForm");

        // register the observation form as an observation listener :
        ObservationManager.getInstance().register(this.observationForm);

        // add the observation form :
        this.jSplitPane.setLeftComponent(this.observationForm);

        // add the raw obs panel :
        rawObsPanel = new RawObservationTablePanel();

        this.jSplitPaneMain.setRightComponent(rawObsPanel);
        this.jSplitPaneMain.setOneTouchExpandable(true);
        resetRawObsPanel();

        this.addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent e) {
                shouldShowRawObsPanel(rawObsPanel.isVisible());
            }
        });
    }
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JScrollPane jScrollPaneTabbedPane;
    private javax.swing.JSplitPane jSplitPane;
    private javax.swing.JSplitPane jSplitPaneMain;
    private javax.swing.JTabbedPane jTabbedPane;
    // End of variables declaration//GEN-END:variables

    /**
     * Handle the given event on the given observation = 
     * add the missing plot panels
     * 
     * @param event event
     */
    @Override
    public void onProcess(final ObservationEvent event) {
        if (logger.isDebugEnabled()) {
            logger.debug("event [{}] process IN", event.getType());
        }

        final ObservationEventType type = event.getType();

        if (type == ObservationEventType.LOADED
                || type == ObservationEventType.TARGET_CHANGED
                || type == ObservationEventType.REFRESH) {

            if (type == ObservationEventType.LOADED) {
                resetRawObsPanel();

                // show interferometer map when a file is loaded or the observation is reset :
                final int idx = this.jTabbedPane.indexOfTab(TAB_INTERFEROMETER_MAP);
                if (idx != -1) {
                    this.jTabbedPane.setSelectedIndex(idx);
                }
            }

            final ObservationSetting observation = (type == ObservationEventType.REFRESH)
                    ? event.getObservationCollection().getFirstObservation() : event.getObservation();

            if (type == ObservationEventType.TARGET_CHANGED) {
                // retrieve the selected target from its name:
                final Target target = observation.getSelectedTarget();
                // force refresh raw obs content:
                this.onTargetSelectionChanged(target);
            }

            // Observability panel :
            if (this.observabilityPanel == null) {
                // create the observability panel :
                this.observabilityPanel = new ObservabilityPanel();
                this.observabilityPanel.setName("observabilityPanel");

                // add the observability panel :
                this.jTabbedPane.addTab(TAB_OBSERVABILITY, this.observabilityPanel);

                // register the observability panel as an observation listener :
                ObservationManager.getInstance().register(this.observabilityPanel);
            }

            // UV coverage panel :
            int uvPanelIndex = -1;
            if (this.uvCoveragePanel != null) {
                uvPanelIndex = this.jTabbedPane.indexOfComponent(this.uvCoveragePanel);
            }

            /* disable UV Coverage & OIFits panels for single dishes */
            if (observation.hasTargets()
                    && observation.getInstrumentConfiguration().getStationList().size() > 1) {
                if (uvPanelIndex == -1) {
                    // create the uv coverage panel :
                    this.uvCoveragePanel = new UVCoveragePanel();
                    this.uvCoveragePanel.setName("uvCoveragePanel");

                    // add the uv coverage panel :
                    this.jTabbedPane.addTab(TAB_UV_COVERAGE, this.uvCoveragePanel);

                    // register the uv coverage panel as an observation listener :
                    final ObservationManager om = ObservationManager.getInstance();
                    om.register(this.uvCoveragePanel);

                    if (type == ObservationEventType.REFRESH) {
                        // Fire events to initialize the UV Coverage panel properly:

                        // target selection:
                        om.fireTargetSelectionChanged(this.uvCoveragePanel);

                        // update the instrument settings:
                        om.fireObservationUpdate();
                    }
                }
            } else {
                if (uvPanelIndex != -1) {
                    // remove the uv panel :
                    this.jTabbedPane.removeTabAt(uvPanelIndex);

                    // free uv coverage panel references (listeners):
                    this.uvCoveragePanel.dispose();
                    this.uvCoveragePanel = null;
                }
                if (this.oiFitsViewPanel != null) {
                    // remove the OIFits viewer panel :
                    this.jTabbedPane.remove(this.oiFitsViewPanel);

                    // free OIFits viewer panel references (listeners):
                    this.oiFitsViewPanel.dispose();
                    this.oiFitsViewPanel = null;

                    this.oiFitsViewPanel = null;
                }
            }
        } else if (type == ObservationEventType.TARGET_SELECTION_CHANGED
                && event instanceof TargetSelectionEvent) {
            this.onTargetSelectionChanged(((TargetSelectionEvent) event).getTarget());
        } else if (type == ObservationEventType.OIFITS_DONE
                && event instanceof OIFitsEvent
                && ((OIFitsEvent) event).getOIFitsData() != null) {

            // create the OIFits viewer panel if null :      
            if (this.oiFitsViewPanel == null) {
                this.oiFitsViewPanel = new OIFitsViewPanel();
                this.oiFitsViewPanel.setName("oiFitsViewer");

                // add the OIFits viewer panel :
                this.jTabbedPane.addTab(TAB_OIFITS_VIEWER, this.oiFitsViewPanel);

                // register the OIFits viewer panel as an observation listener (listener 4) :
                ObservationManager.getInstance().register(this.oiFitsViewPanel);
            }
        }

        // hide tooltip:
        hideTooltip();

        if (logger.isDebugEnabled()) {
            logger.debug("event [{}] process OUT", event.getType());
        }
    }

    /**
     * Hide any displayed tooltip
     */
    private void hideTooltip() {
        // disable the tooltip manager temporarly to hide the tooltip on target list:
        tm.setEnabled(false);
        tm.setEnabled(true);
    }

    /**
     * Returns the currently selected component in the tabbedpane
     * @return selected component or null if the tabbedpane is empty
     */
    public Component getTabSelectedComponent() {
        return jTabbedPane.getSelectedComponent();
    }

    /**
     * Returns the currently selected component in the tabbedpane as a DocumentExportable
     * @return selected component or null if the tabbedpane is empty
     */
    public DocumentExportable getExportableSelectedComponent() {
        Component com = getTabSelectedComponent();
        if (com instanceof DocumentExportable) {
            return (DocumentExportable) com;
        }
        return null;
    }

    /**
     * Return true if the selected tab is using target models
     * @return true if the selected tab is using target models
     */
    public boolean isSelectedTabUsingTargetModel() {
        final Component com = getTabSelectedComponent();

        return com == this.uvCoveragePanel || com == this.oiFitsViewPanel;
    }

    /**
     * Return true if the selected tab is the uv coverage panel
     * @return true if the selected tab is the uv coverage panel
     */
    public boolean isUVCoveragePanelSelected() {
        final Component com = getTabSelectedComponent();

        return com == this.uvCoveragePanel;
    }

    /**
     * Return the observation form
     * @return observation form
     */
    public BasicObservationForm getObservationForm() {
        return observationForm;
    }

    /**
     * Return the observability panel
     * @return observability panel or null if undefined
     */
    public ObservabilityPanel getObservabilityPanel() {
        return observabilityPanel;
    }

    /**
     * Return the uv coverage panel
     * @return uv coverage panel or null if undefined
     */
    public UVCoveragePanel getUVCoveragePanel() {
        return uvCoveragePanel;
    }

    // --- RawObs panel handling ---
    private void resetRawObsPanel() {
        rawObsPanel.resetFilters();
        onTargetSelectionChanged(null);
    }

    /**
     * Update the selected target for the given observation
     * @param target selected target
     */
    private void onTargetSelectionChanged(final Target target) {
        logger.debug("onTargetSelectionChanged : {}", target);

        List<RawObservation> rawObsList = null;

        // update the current selected target :
        if (target != null) {
            final ObservationSetting observation = ObservationManager.getInstance().getMainObservation();

            final TargetRawObservation targetRawObs = observation.getTargetRawObservation(target);
            if (targetRawObs != null) {
                // Get all observations:
                if (!targetRawObs.getObservations().isEmpty()) {
                    rawObsList = targetRawObs.getObservations();
                }
            }
        }
        // update data:
        rawObsPanel.setData(rawObsList);

        shouldShowRawObsPanel(rawObsList != null);
    }

    /**
     * Tell GUI to show or hide raw obs panel
     * @param visible
     */
    public void shouldShowRawObsPanel(final boolean visible) {
        if (rawObsPanel.isVisible() != visible) {
            rawObsPanel.setVisible(visible);
        }
        if (visible) {
            final int totalHeight = (int) jSplitPaneMain.getBounds().getHeight();
            final int dividerHeight = jSplitPaneMain.getDividerSize();
            final int freeHeight = totalHeight - dividerHeight;

            logger.debug("jSplitPaneMain Height: {}", totalHeight);
            logger.debug("freeHeight: {}", freeHeight);

            int rawObsPanelHeight = freeHeight;

            int prefHeight = rawObsPanel.getPreferredSize().height;

            rawObsPanelHeight -= Math.max(prefHeight, (int) Math.ceil(0.15 * freeHeight)); // 15%

            logger.debug("divider location: {}", rawObsPanelHeight);
            jSplitPaneMain.setDividerLocation(rawObsPanelHeight);
        }
    }
}
