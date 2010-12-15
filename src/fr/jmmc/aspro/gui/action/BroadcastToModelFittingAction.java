/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: BroadcastToModelFittingAction.java,v 1.9 2010-12-15 13:36:13 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.interop.SampCapability;
import fr.jmmc.mcs.interop.SampCapabilityAction;
import fr.jmmc.mcs.model.targetmodel.Model;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIFitsWriter;
import java.io.File;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

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
    private final static String className = "fr.jmmc.aspro.gui.action.BroadcastToModelFittingAction";
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

    @Override
    public Map<?,?> composeMessage() {
        logger.fine("composeMessage");

        // Get the oifits object
        // save it on disk
        // get Target model from the targetId given by oifits.oiTargets
        // serialize and build one samp message

        final OIFitsFile oiFitsFile = ObservationManager.getInstance().getObservation().getOIFitsFile();

        if (oiFitsFile == null) {
            MessagePane.showMessage("There is currently no OIFits data (your target is not observable)");
            return null;
        }

        File file = new File(ExportOIFitsAction.getDefaultFileName(oiFitsFile));
        file.deleteOnExit();

        try {
            OIFitsWriter.writeOIFits(file.getAbsolutePath(), oiFitsFile);
            StatusBar.show(file.getName() + " created.");
        } catch (Exception e) {
            MessagePane.showErrorMessage("Could not export to file " + file.getName() + "\n", e);
            return null;
        }

        // Get Model assuming that target name is the first one (and only one).. of oifits.
        String targetName = oiFitsFile.getOiTarget().getTarget()[0];
        Target target = ObservationManager.getInstance().getTarget(targetName);
        Model targetModel = new Model();
        targetModel.setNameAndType("Container");
        for (Model model : target.getModels()) {
            targetModel.getModels().add(model);
        }
        String xmlModel = "";
        try {
            StringWriter sw = new StringWriter();
            final Marshaller marshaller = this.jf.createMarshaller();
            marshaller.marshal(targetModel, sw);
            xmlModel = sw.toString();
            StatusBar.show("Model received from remote application.");
        } catch (JAXBException je) {
            MessagePane.showErrorMessage("Could not build model desc for samp message", je);
            return null;
        }

        // Store parameters for reply message
        final Map<String, String> params = new HashMap<String, String>();
        params.put("model", xmlModel);
        params.put("filename", file.getAbsolutePath());
        
        StatusBar.show("New settings sent by remote application ready for modification.");
        
        return params;
    }
}
