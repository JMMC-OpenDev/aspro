/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SettingPanel.java,v 1.8 2009-12-04 15:38:27 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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

import fr.jmmc.aspro.model.ObservationListener;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import java.awt.BorderLayout;
import java.util.logging.Level;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

/**
 * This panel corresponds to the single observation setting panel
 * @author bourgesl
 */
public class SettingPanel extends JPanel implements ObservationListener {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.SettingPanel";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** tabs present in the plots panel */
  protected JTabbedPane tabs;
  /** basic observation form */
  private BasicObservationForm observationForm = null;
  /** observability panel */
  private ObservabilityPanel observabilityPanel = null;

  /** Creates new form SettingPanel */
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

    jSplitPane1 = new javax.swing.JSplitPane();
    jPlotPanel = new javax.swing.JPanel();

    setLayout(new java.awt.BorderLayout());

    jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);

    jPlotPanel.setBorder(javax.swing.BorderFactory.createTitledBorder("Plots"));
    jPlotPanel.setLayout(new java.awt.BorderLayout());
    jSplitPane1.setRightComponent(jPlotPanel);

    add(jSplitPane1, java.awt.BorderLayout.CENTER);
  }// </editor-fold>//GEN-END:initComponents

  /**
   * This method is useful to set the models and specific features of initialized swing components :
   */
  private void postInit() {
    // prepare the tabs panel :
    this.tabs = new JTabbedPane(JTabbedPane.TOP);
    this.jPlotPanel.add(this.tabs, BorderLayout.CENTER);

    // register this as an observation listener :
    ObservationManager.getInstance().register(this);

    // add the observation form that will send an onChange event on the current observation :
    this.observationForm = new BasicObservationForm();
    this.jSplitPane1.setLeftComponent(observationForm);
  }
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JPanel jPlotPanel;
  private javax.swing.JSplitPane jSplitPane1;
  // End of variables declaration//GEN-END:variables

  /**
   * Handle the given event on the given observation = 
   * add the missing plot panels
   * 
   * @param type event type
   * @param observation observation
   */
  public void onProcess(final ObservationEventType type, final ObservationSetting observation) {
    if (type == ObservationEventType.CHANGED) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("onChange occured : " + observation.getName());
      }

      if (this.observabilityPanel == null) {
        this.observabilityPanel = new ObservabilityPanel();
        this.tabs.addTab("Observability", this.observabilityPanel);

        // first time, the onChange event must be propagated to the new registered listener :
        this.observabilityPanel.onProcess(type, observation);
      }
    }
  }
}
