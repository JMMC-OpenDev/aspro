/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SettingPanel.java,v 1.5 2009-11-03 16:57:55 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

/**
 *
 * @author bourgesl
 */
public class SettingPanel extends JPanel implements ObservationListener {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.SettingPanel";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /** observation manager */
  private ObservationManager om = ObservationManager.getInstance();
  /** tabs */
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
    jPanel1 = new javax.swing.JPanel();

    setLayout(new java.awt.BorderLayout());

    jSplitPane1.setOrientation(javax.swing.JSplitPane.VERTICAL_SPLIT);

    jPanel1.setBorder(javax.swing.BorderFactory.createTitledBorder("Plots"));
    jPanel1.setLayout(new java.awt.BorderLayout());
    jSplitPane1.setRightComponent(jPanel1);

    add(jSplitPane1, java.awt.BorderLayout.CENTER);
  }// </editor-fold>//GEN-END:initComponents




  /**
   * This method is useful to set the models and specific features of initialized swing components :
   */
  private void postInit() {
    this.tabs = new JTabbedPane(JTabbedPane.TOP);
    jPanel1.add(this.tabs, BorderLayout.CENTER);

    // register this as an observation listener :
    om.register(this);

    observationForm = new BasicObservationForm();
    jSplitPane1.setLeftComponent(observationForm);
  }
  
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JPanel jPanel1;
  private javax.swing.JSplitPane jSplitPane1;
  // End of variables declaration//GEN-END:variables

  public void onChange(final ObservationSetting observation) {
    logger.fine("onChange occured : " + observation);

    if (observabilityPanel == null) {
      this.observabilityPanel = new ObservabilityPanel();
      this.tabs.addTab("observability", this.observabilityPanel);

      // first time, the onChange event must be propagated to the new registered listener :
      this.observabilityPanel.onChange(observation);
    }
  }
  
}
