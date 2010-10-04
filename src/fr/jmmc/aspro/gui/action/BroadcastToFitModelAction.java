/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: BroadcastToFitModelAction.java,v 1.1 2010-10-04 12:29:30 mella Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 *
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.jaxb.JAXBFactory;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.model.targetmodel.Model;
import fr.jmmc.mcs.util.RegisteredAction;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.OIFitsWriter;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import javax.swing.JOptionPane;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import org.astrogrid.samp.Message;
import org.astrogrid.samp.Metadata;
import org.astrogrid.samp.SampMap;
import org.astrogrid.samp.client.AbstractMessageHandler;
import org.astrogrid.samp.client.ClientProfile;
import org.astrogrid.samp.client.DefaultClientProfile;
import org.astrogrid.samp.client.HubConnection;
import org.astrogrid.samp.client.HubConnector;

/**
 * This registered action represents a File Menu entry to
 * send model and generated oifits to one modelfitting application.
 * @author mella
 */
public class BroadcastToFitModelAction extends RegisteredAction {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class name. This name is used to register to the ActionRegistrar */
    private final static String className = "fr.jmmc.aspro.gui.action.BroadcastToFitModelAction";
    /** Action name. This name is used to register to the ActionRegistrar */
    public final static String actionName = "broadcastToFitModelAction";
    /** Class logger */
    private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);

    private static HubConnector conn = null;

    /* members */
    /** package name for JAXB generated code */
    private final static String OI_JAXB_PATH = "fr.jmmc.aspro.model.oi";
    /** internal JAXB Factory */
    private JAXBFactory jf;

    /**
     * Public constructor that automatically register the action in RegisteredAction.
     */
    public BroadcastToFitModelAction() {
        super(className, actionName);
        this.jf = JAXBFactory.getInstance(OI_JAXB_PATH);

    }

    /**
     * Handle the action event
     * @param evt action event
     */
    public void actionPerformed(final ActionEvent evt) {
        if (logger.isLoggable(Level.FINE)) {
            logger.fine("actionPerformed");
        }

        // Get the oifits object
        // save it on disk
        // get Target model from the targetId given by oifits.oiTargets
        // serialize and build one samp message

        final OIFitsFile oiFitsFile = ObservationManager.getInstance().getObservation().getOIFitsFile();

        if (oiFitsFile == null) {
            StatusBar.show("Can't get oifits to forward to one modelfitting application.");
            return;
        }

        File file = new File(ExportOIFitsAction.getName(oiFitsFile));
        file.deleteOnExit();

        try {
            OIFitsWriter.writeOIFits(file.getAbsolutePath(), oiFitsFile);
            StatusBar.show(file.getName() + " created.");
        } catch (Exception e) {
            logger.log(Level.SEVERE, "failure : ", e);
            JOptionPane.showMessageDialog(null,
                    "Could not export to file " + file.getName() + "\n" + e.getMessage(),
                    "Error", JOptionPane.ERROR_MESSAGE);
            return;
        }

        // Get Model assuming that target name is the first one (and only one).. of oifits.
        String targetName = oiFitsFile.getOiTarget().getTarget()[0];
        Target t = ObservationManager.getInstance().getObservation().getTarget(targetName);
        Model targetModel = new Model();
        targetModel.setNameAndType("Container");        
        for (Model model : t.getModels()) {
            targetModel.getModels().add(model);
        }
        String xmlModel="";
        try {
            StringWriter sw = new StringWriter();
            final Marshaller marshaller = this.jf.createMarshaller();
            marshaller.marshal(targetModel, sw);
            xmlModel=sw.toString();
        } catch (JAXBException je) {
            //throw new RuntimeException("Save failure on " + outputFile, je);
            je.printStackTrace();
            return;
        }

        // connect to samp if not already done
        if (conn == null) {
            // Construct a connector
            ClientProfile profile = DefaultClientProfile.getProfile();
            conn = new HubConnector(profile);

            // Configure it with metadata about this application
            Metadata meta = new Metadata();
            meta.setName("ASPRO2GMGMGM");
            meta.setDescriptionText("Application that does stuff");
            conn.declareMetadata(meta);

            // Prepare to receive messages with specific MType(s)
            conn.addMessageHandler(new AbstractMessageHandler("stuff.do") {

                public Map processCall(HubConnection c, String senderId, Message msg) {
                    System.out.println("TBD for " + msg);
                    return null;
                }
            });

            // This step required even if no custom message handlers added.
            conn.declareSubscriptions(conn.computeSubscriptions());

            // Keep a look out for hubs if initial one shuts down
            conn.setAutoconnect(10);
        }

        // Broadcast a message
        try {        
            HashMap params = new HashMap();
            params.put("model", xmlModel);
            params.put("filename", file.getAbsolutePath());
            conn.getConnection().notifyAll(new Message("LITpro.runfit", params));
        } catch (Exception ex) {            
            ex.printStackTrace();
        }

    }
}
