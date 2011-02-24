/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SettingPanel.java,v 1.31 2011-02-24 17:14:11 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.30  2011/02/22 18:11:29  bourgesl
 * Major UI changes : configuration multi-selection, unique target selection in main form
 *
 * Revision 1.29  2011/01/28 16:32:35  mella
 * Add new observationEvents (CHANGED replaced by DO_UPDATE, REFRESH and REFRESH_UV)
 * Modify the observationListener interface
 *
 * Revision 1.28  2011/01/27 17:07:29  bourgesl
 * use target changed event to create UVCoveragePanel
 * SettingPanel is now the third listener in order to synchronize correctly target lists
 *
 * Revision 1.27  2011/01/21 16:23:23  bourgesl
 * import ObservationEventType
 *
 * Revision 1.26  2010/12/17 15:17:53  bourgesl
 * use hasTarget
 *
 * Revision 1.25  2010/11/25 07:58:32  bourgesl
 * typo
 *
 * Revision 1.24  2010/09/15 15:08:48  bourgesl
 * disable again OIFits (dev only)
 *
 * Revision 1.23  2010/09/08 16:00:30  bourgesl
 * unregister Preference Observers when the widget is released (Preference View, UV Coverage Panel)
 *
 * Revision 1.22  2010/09/02 15:54:42  bourgesl
 * disable OIFits panel for production
 *
 * Revision 1.21  2010/06/23 12:52:42  bourgesl
 * added the OIFits panel (creation / removal) and proper registration of Observation listeners in ObservationManager
 *
 * Revision 1.20  2010/06/17 10:02:50  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.19  2010/06/09 12:51:53  bourgesl
 * add getTabSelectedComponent() to get the current active tab component (use to export chart to PDF)
 *
 * Revision 1.18  2010/06/08 14:49:29  bourgesl
 * fixed the 'baseLine Limits' bug that has a side effect = UV Coverage is unavailable
 * javadoc
 *
 * Revision 1.17  2010/05/11 12:08:27  bourgesl
 * simple Interferometer Map (stations + baselines) automatically refreshed when the chosen baseline configuration changes
 *
 * Revision 1.16  2010/02/12 15:53:18  bourgesl
 * added target model editor
 *
 * Revision 1.15  2010/01/20 16:18:37  bourgesl
 * observation form refactoring
 *
 * Revision 1.14  2010/01/15 13:52:14  bourgesl
 * instrumentMode synchronized properly between the observation and the UI widgets (load/change/reset)
 *
 * Revision 1.13  2010/01/14 17:03:37  bourgesl
 * refactoring for observation LOAD / CHANGE events
 *
 * Revision 1.12  2010/01/11 13:58:43  bourgesl
 * bad class name for UV Coverage Panel
 *
 * Revision 1.11  2010/01/08 16:51:17  bourgesl
 * initial uv coverage
 *
 * Revision 1.10  2010/01/05 17:19:29  bourgesl
 * updated basic UV panel
 *
 * Revision 1.9  2009/12/15 16:31:49  bourgesl
 * added uv panel
 *
 * Revision 1.8  2009/12/04 15:38:27  bourgesl
 * Added Save action in the menu bar
 *
 * Revision 1.7  2009/11/24 15:12:09  bourgesl
 * first step to handle delay line limits
 *
 * Revision 1.6  2009/11/17 17:00:28  bourgesl
 * chosen instrument configuration propagated to observation
 *
 * Revision 1.5  2009/11/03 16:57:55  bourgesl
 * added observability plot with LST/UTC support containing only day/night/twilight zones
 *
 * Revision 1.4  2009/10/22 15:47:22  bourgesl
 * beginning of observability computation with jSkyCalc
 *
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.model.event.ObservationEventType;
import fr.jmmc.aspro.model.event.ObservationListener;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.event.ObservationEvent;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import java.awt.Component;
import java.util.logging.Level;
import javax.swing.JPanel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

/**
 * This panel corresponds to the single observation setting panel
 * @author bourgesl
 */
public final class SettingPanel extends JPanel implements ObservationListener {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.SettingPanel";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** enable / disable OIFits panel */
  private static final boolean ENABLE_OIFITS = false;

  /* Tab names */
  /** name of the tab pane corresponding to the interferometer map */
  private static final String TAB_INTERFEROMETER_MAP = "Map";
  /** name of the tab pane corresponding to the observability panel */
  private static final String TAB_OBSERVABILITY = "Observability";
  /** name of the tab pane corresponding to the uv coverage panel */
  private static final String TAB_UV_COVERAGE = "UV coverage";
  /** name of the tab pane corresponding to the OIFits panel */
  private static final String TAB_OIFITS = "OIFits";

  /* members */
  /** basic observation form */
  private BasicObservationForm observationForm = null;
  /** observability panel */
  private ObservabilityPanel observabilityPanel = null;
  /** uv coverage panel */
  private UVCoveragePanel uvCoveragePanel = null;
  /** OIFits panel */
  private OIFitsPanel oiFitsPanel = null;

  /** 
   * Creates new form SettingPanel
   */
  public SettingPanel() {
    initComponents();
    postInit();
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

    // register the map panel as an observation listener before the observation form (listener 2) :
    ObservationManager.getInstance().register(mapPanel);

    // add the map panel :
    this.jTabbedPane.addTab(TAB_INTERFEROMETER_MAP, mapPanel);

    // create the observation form that will send a changed event on the current observation (listener 3) :
    this.observationForm = new BasicObservationForm();

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
  public void onProcess(final ObservationEvent event) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("event [" + event.getType() + "] process IN");
    }

    final ObservationEventType type = event.getType();

    if (type == ObservationEventType.TARGET_CHANGED
            || type == ObservationEventType.LOADED) {

      final ObservationSetting observation = event.getObservation();

      // Observability panel :
      if (this.observabilityPanel == null) {
        // create the observability panel :
        this.observabilityPanel = new ObservabilityPanel();

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
        if (ENABLE_OIFITS && this.oiFitsPanel != null) {
          // remove the OIFits panel :
          this.jTabbedPane.remove(this.oiFitsPanel);

          // unregister the OIFits for the next event :
          ObservationManager.getInstance().unregister(this.oiFitsPanel);

          this.oiFitsPanel = null;
        }
      }
    } else if (ENABLE_OIFITS && type == ObservationEventType.OIFITS_DONE) {
      // OIFits panel :
      if (this.oiFitsPanel == null) {
        // create the OIFits panel :
        this.oiFitsPanel = new OIFitsPanel();

        // register the OIFits panel as an observation listener :
        ObservationManager.getInstance().register(this.oiFitsPanel);

        // the event must be propagated to the new registered listener :
        this.oiFitsPanel.onProcess(event);

        // add the OIFits panel :
        this.jTabbedPane.addTab(TAB_OIFITS, this.oiFitsPanel);
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
  public final Component getTabSelectedComponent() {
    return jTabbedPane.getSelectedComponent();
  }

  /**
   * Return the observation form
   * @return observation form
   */
  public final BasicObservationForm getObservationForm() {
    return observationForm;
  }

  /**
   * Return the observability panel
   * @return observability panel or null if undefined
   */
  public final ObservabilityPanel getObservabilityPanel() {
    return observabilityPanel;
  }

  /**
   * Return the uv coverage panel
   * @return uv coverage panel or null if undefined
   */
  public final UVCoveragePanel getUVCoveragePanel() {
    return uvCoveragePanel;
  }
}
