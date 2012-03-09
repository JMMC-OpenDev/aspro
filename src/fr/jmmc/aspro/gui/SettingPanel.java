/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.model.event.ObservationEventType;
import fr.jmmc.aspro.model.event.ObservationListener;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.event.OIFitsEvent;
import fr.jmmc.aspro.model.event.ObservationEvent;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.gui.task.TaskSwingWorkerExecutor;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.logging.Level;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.Timer;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * This panel corresponds to the single observation setting panel
 * @author bourgesl
 */
public final class SettingPanel extends JPanel implements ObservationListener, Disposable {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(SettingPanel.class.getName());
  /** default mouse cursor refresh period = 100 ms */
  private static final int REFRESH_PERIOD = 100;
  /** enable / disable OIFits panel */
  private static final boolean ENABLE_OIFITS = false;

  /* Tab names */
  /** name of the tab pane corresponding to the interferometer map */
  public static final String TAB_INTERFEROMETER_MAP = "Map";
  /** name of the tab pane corresponding to the observability panel */
  public static final String TAB_OBSERVABILITY = "Observability";
  /** name of the tab pane corresponding to the uv coverage panel */
  public static final String TAB_UV_COVERAGE = "UV coverage";
  /** name of the tab pane corresponding to the OIFits panel */
  public static final String TAB_OIFITS = "OIFits";
  /** name of the tab pane corresponding to the vis2 view of generated OIFits */
  public static final String TAB_VIS2 = "Vis2 plot";

  /* members */
  /** timeline refresh Swing timer */
  private final Timer timerMouseCursorRefresh;
  /** basic observation form */
  private BasicObservationForm observationForm = null;
  /** observability panel */
  private ObservabilityPanel observabilityPanel = null;
  /** uv coverage panel */
  private UVCoveragePanel uvCoveragePanel = null;
  /** OIFits panel */
  private OIFitsPanel oiFitsPanel = null;
  /** Vis2 panel */
  private Vis2Panel vis2Panel = null;

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
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("Starting timer: " + this.timerMouseCursorRefresh);
        }
        this.timerMouseCursorRefresh.start();
      }
    } else {
      if (this.timerMouseCursorRefresh.isRunning()) {
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("Stopping timer: " + this.timerMouseCursorRefresh);
        }
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
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("dispose : " + this);
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
    jTabbedPane = new javax.swing.JTabbedPane();

    setLayout(new java.awt.BorderLayout());

    jSplitPane.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);
    jSplitPane.setResizeWeight(0.05);
    jSplitPane.setContinuousLayout(true);
    jSplitPane.setRightComponent(jTabbedPane);

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

    // create the map panel :
    final InterferometerMapPanel mapPanel = new InterferometerMapPanel();
    mapPanel.setName("mapPanel");

    // register the map panel as an observation listener before the observation form (listener 2) :
    ObservationManager.getInstance().register(mapPanel);

    // add the map panel :
    this.jTabbedPane.addTab(TAB_INTERFEROMETER_MAP, mapPanel);

    // create the observation form that will send a changed event on the current observation (listener 3) :
    this.observationForm = new BasicObservationForm();
    observationForm.setName("observationForm");

    // register the observation form as an observation listener :
    ObservationManager.getInstance().register(this.observationForm);

    // add the observation form :
    this.jSplitPane.setLeftComponent(this.observationForm);
  }
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JSplitPane jSplitPane;
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
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("event [" + event.getType() + "] process IN");
    }

    final ObservationEventType type = event.getType();

    if (type == ObservationEventType.TARGET_CHANGED
            || type == ObservationEventType.LOADED) {

      if (type == ObservationEventType.LOADED) {
        // show interferometer map when a file is loaded or the observation is reset :
        this.jTabbedPane.setSelectedIndex(0);
      }

      final ObservationSetting observation = event.getObservation();

      // Observability panel :
      if (this.observabilityPanel == null) {
        // create the observability panel :
        this.observabilityPanel = new ObservabilityPanel();
        this.observabilityPanel.setName("observabilityPanel");

        // register the observability panel as an observation listener :
        ObservationManager.getInstance().register(this.observabilityPanel);

        // the event must be propagated to the new registered listener :
        this.observabilityPanel.onProcess(event);

        // add the observability panel :
        this.jTabbedPane.addTab(TAB_OBSERVABILITY, this.observabilityPanel);
      }

      // UV coverage panel :
      int uvPanelIndex = -1;
      if (this.uvCoveragePanel != null) {
        uvPanelIndex = this.jTabbedPane.indexOfComponent(this.uvCoveragePanel);
      }

      if (observation.hasTargets()) {
        if (uvPanelIndex == -1) {
          // create the uv coverage panel :
          this.uvCoveragePanel = new UVCoveragePanel();
          this.uvCoveragePanel.setName("uvCoveragePanel");

          // register the uv coverage panel as an observation listener :
          ObservationManager.getInstance().register(this.uvCoveragePanel);

          // the event must be propagated to the new registered listener :
          this.uvCoveragePanel.onProcess(event);

          // add the uv coverage panel :
          this.jTabbedPane.addTab(TAB_UV_COVERAGE, this.uvCoveragePanel);
        }
      } else {
        if (uvPanelIndex != -1) {
          // remove the uv panel :
          this.jTabbedPane.removeTabAt(uvPanelIndex);

          // unregister the uv panel for the next event :
          ObservationManager.getInstance().unregister(this.uvCoveragePanel);

          // free uv coverage panel references :
          this.uvCoveragePanel.dispose();
          this.uvCoveragePanel = null;
        }
        if (this.vis2Panel != null) {
          // remove the vis2 panel :
          this.jTabbedPane.remove(this.vis2Panel);

          // unregister the vis2 panel for the next event :
          ObservationManager.getInstance().unregister(this.vis2Panel);

          this.vis2Panel = null;
        }
        if (ENABLE_OIFITS && this.oiFitsPanel != null) {
          // remove the OIFits panel :
          this.jTabbedPane.remove(this.oiFitsPanel);

          // unregister the OIFits panel for the next event :
          ObservationManager.getInstance().unregister(this.oiFitsPanel);

          this.oiFitsPanel = null;
        }
      }
    } else if (type == ObservationEventType.OIFITS_DONE
            && event instanceof OIFitsEvent
            && ((OIFitsEvent) event).getOIFitsFile() != null) {

      // create the vis2 panel if null :      
      if (this.vis2Panel == null) {
        this.vis2Panel = new Vis2Panel();
        this.vis2Panel.setName("vis2Panel");

        // register the vis2 panel as an observation listener (listener 4) :
        ObservationManager.getInstance().register(this.vis2Panel);

        // the event must be propagated to the new registered listener :
        this.vis2Panel.onProcess(event);

        // add the vis2 panel :
        this.jTabbedPane.addTab(TAB_VIS2, this.vis2Panel);
      }

      if (ENABLE_OIFITS) {
        // OIFits panel :
        if (this.oiFitsPanel == null) {
          // create the OIFits panel :
          this.oiFitsPanel = new OIFitsPanel();
          this.oiFitsPanel.setName("oiFitsPanel");

          // register the OIFits panel as an observation listener :
          ObservationManager.getInstance().register(this.oiFitsPanel);

          // the event must be propagated to the new registered listener :
          this.oiFitsPanel.onProcess(event);

          // add the OIFits panel :
          this.jTabbedPane.addTab(TAB_OIFITS, this.oiFitsPanel);
        }
      }
    }
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("event [" + type + "] process OUT");
    }
  }

  /**
   * Returns the currently selected component in the tabbedpane
   * @return selected component or null if the tabbedpane is empty
   */
  public Component getTabSelectedComponent() {
    return jTabbedPane.getSelectedComponent();
  }

  /**
   * Return true if the selected tab is using target models
   * @return true if the selected tab is using target models
   */
  public boolean isSelectedTabUsingTargetModel() {
    final Component com = getTabSelectedComponent();

    return com == this.uvCoveragePanel || com == this.vis2Panel;
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
}
