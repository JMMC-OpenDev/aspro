package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.model.oi.FocalInstrumentConfigurationChoice;
import fr.jmmc.aspro.model.oi.InterferometerConfigurationChoice;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.WhenSetting;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;

/**
 * This class extends the defaultTreeModel to add an dedicated API for the Observation preparation
 * @author bourgesl
 */
public class SettingTreeModel extends DefaultTreeModel {

  private ObservationSetting observation;

  public SettingTreeModel() {
    super(new DefaultMutableTreeNode(new ObservationSetting()));

    this.observation = (ObservationSetting)getRootNode().getUserObject();
    createDefaultObservationModel(this.observation);
    
    refreshNodes(this.observation);
  }

  protected void createDefaultObservationModel(final ObservationSetting obs) {
    obs.setName("default");
    obs.setWhen(new WhenSetting());
    obs.setInterferometerConfiguration(new InterferometerConfigurationChoice());
    obs.setInstrumentConfiguration(new FocalInstrumentConfigurationChoice());
    obs.getTargets().add(new Target());
  }

  /**
   * Fill the current <code>TreeModel</code>.
   */
  protected void refreshNodes(final ObservationSetting obs) {
    final DefaultMutableTreeNode r = getRootNode();
    DefaultMutableTreeNode parent;

    r.removeAllChildren();

    // "When : 01/10/2009 - 31/03/2010"
    parent = new DefaultMutableTreeNode(obs.getWhen());
    r.add(parent);

    parent = new DefaultMutableTreeNode(obs.getInterferometerConfiguration());
    r.add(parent);

    parent = new DefaultMutableTreeNode(obs.getInstrumentConfiguration());
    r.add(parent);

    parent = new DefaultMutableTreeNode("Targets");
    r.add(parent);
    parent.add(new DefaultMutableTreeNode(obs.getTargets().get(0)));
  }


  private DefaultMutableTreeNode getRootNode() {
    return (DefaultMutableTreeNode)getRoot();
  }

  
  // Getter - Setter :

  public ObservationSetting getObservation() {
    return observation;
  }



  
}