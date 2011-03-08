/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: BroadcastToModelFittingAction.java,v 1.15 2011-03-08 17:25:26 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.14  2011/03/01 17:12:06  bourgesl
 * added message if multiple configuration
 * use OM.getOIFitsFile instead of observation
 *
 * Revision 1.13  2011/02/18 15:31:39  bourgesl
 * minor refactoring on target model serialization
 *
 * Revision 1.12  2011/02/17 17:13:44  bourgesl
 * removed status changes
 *
 * Revision 1.11  2011/02/17 11:02:21  mella
 * Throw illegalStateException instead of simple error messages
 *
 * Revision 1.10  2011/02/03 17:30:05  bourgesl
 * added to do
 *
 * Revision 1.9  2010/12/15 13:36:13  bourgesl
 * refactoring
 *
 * Revision 1.8  2010/12/14 09:25:13  bourgesl
 * variable renamed
 *
 * Revision 1.7  2010/10/06 16:05:53  bourgesl
 * added comments
 *
 * Revision 1.6  2010/10/05 14:59:02  bourgesl
 * fixed composeMessage signature
 *
 * Revision 1.5  2010/10/05 13:01:46  mella
 * major cleanup to implement action that forward model and oifits to modelfitting application
 *
 * Revision 1.4  2010/10/05 07:57:49  mella
 * Add composeMessage
 *
 * Revision 1.3  2010/10/05 07:56:57  mella
 * Use SampCapability.LITPRO_START_SETTING in constructor instead of JmmcCapability
 *
 * Revision 1.2  2010/10/04 15:57:32  mella
 * Fix constant values
 * Display errors using MessagePane
 *
 * Revision 1.1  2010/10/04 12:31:48  mella
 * Renamed
 *
 * Revision 1.1  2010/10/04 12:29:30  mella
 * Add first (non-working) revision
 *
 *
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.jaxb.JAXBFactory;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.gui.task.TaskSwingWorkerExecutor;
import fr.jmmc.mcs.interop.SampCapability;
import fr.jmmc.mcs.interop.SampCapabilityAction;
import fr.jmmc.mcs.model.targetmodel.Model;
import fr.jmmc.mcs.util.FileUtils;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIFitsWriter;
import fr.nom.tam.fits.FitsException;
import java.io.File;
import java.io.IOException;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;

/**
 * This registered action represents a File Menu entry to
 * send model and generated oifits to one modelfitting application.
 *
 * @author mella
 */
public class BroadcastToModelFittingAction extends SampCapabilityAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  private final static String className = BroadcastToModelFittingAction.class.getName();
  /** Action name. This name is used to register to the ActionRegistrar */
  public final static String actionName = "broadcastToModelFittingAction";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);

  /* members */
  /** package name for JAXB generated code */
  private final static String OI_JAXB_PATH = "fr.jmmc.aspro.model.oi";
  /** internal JAXB Factory */
  private JAXBFactory jf;

  /**
   * Public constructor that automatically register the action in RegisteredAction.
   */
  public BroadcastToModelFittingAction() {
    super(className, actionName, SampCapability.LITPRO_START_SETTING);
    this.jf = JAXBFactory.getInstance(OI_JAXB_PATH);
  }

  /**
   * Should return the message you want to send
   * @throws IllegalStateException if the oifits file can not be written to a temporary file
   * @return Samp message parameters as a map
   */
  @Override
  public Map<?, ?> composeMessage() throws IllegalStateException {
    logger.fine("composeMessage");

    // Use main observation to check variants :
    if (!ObservationManager.getInstance().getMainObservation().isSingle()) {
      MessagePane.showMessage("Aspro 2 can not generate an OIFits file when multiple configurations are selected !");
      return null;
    }

    // check if there is any running task :
    if (TaskSwingWorkerExecutor.isTaskRunning()) {
      MessagePane.showMessage("Computations are in progress; please retry later.");
      return null;
    }

    // Get the oifits object
    // save it on disk
    // get Target model from the targetId given by oifits.oiTargets
    // serialize and build one samp message

    final OIFitsFile oiFitsFile = ObservationManager.getInstance().getOIFitsFile();

    if (oiFitsFile == null) {
      MessagePane.showMessage("There is currently no OIFits data (your target is not observable)");
      return null;
    }

    final File file = FileUtils.getTempFile(ExportOIFitsAction.getDefaultFileName(oiFitsFile));

    try {
      OIFitsWriter.writeOIFits(file.getAbsolutePath(), oiFitsFile);
    } catch (FitsException fe) {
      throw new IllegalStateException("Could not export to temporary file : " + file.getAbsolutePath(), fe);
    } catch (IOException ioe) {
      throw new IllegalStateException("Could not export to temporary file : " + file.getAbsolutePath(), ioe);
    }

    // Get Model assuming that target name is the first one (and only one).. of oifits.
    final String targetName = oiFitsFile.getOiTarget().getTarget()[0];

    final Target target = ObservationManager.getInstance().getTarget(targetName);

    final Model targetModel = new Model();
    targetModel.setNameAndType("Container");
    for (Model model : target.getModels()) {
      targetModel.getModels().add(model);
    }

    // Create a 4K buffer for models :
    final StringWriter sw = new StringWriter(4096);

    // serialize models to xml :
    ObservationManager.getInstance().saveObject(sw, targetModel);

    final String xmlModel = sw.toString();

    // Store parameters for reply message
    final Map<String, String> params = new HashMap<String, String>();
    params.put("model", xmlModel);
    params.put("filename", file.getAbsolutePath());

    return params;
  }
}
