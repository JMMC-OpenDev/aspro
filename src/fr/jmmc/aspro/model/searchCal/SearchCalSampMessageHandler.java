/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SearchCalSampMessageHandler.java,v 1.1 2010-10-07 15:04:23 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.model.searchCal;

import fr.jmmc.aspro.AsproGui;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.util.FileUtils;
import fr.jmmc.aspro.util.XmlFactory;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.interop.SampCapability;
import fr.jmmc.mcs.interop.SampMessageHandler;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import javax.swing.SwingUtilities;
import org.astrogrid.samp.Message;
import org.astrogrid.samp.client.HubConnection;
import org.astrogrid.samp.client.SampException;

/**
 * This class handles the SAMP Message load votable coming from SearchCal to extract calibrators
 * @author bourgesl
 */
public final class SearchCalSampMessageHandler extends SampMessageHandler {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.model.searchCal.SearchCalSampMessageHandler";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** XSLT file path */
  private final static String XSLT_FILE = "fr/jmmc/aspro/model/searchCal/scvot2AsproObservation.xsl";

  /**
   * Public constructor
   */
  public SearchCalSampMessageHandler() {
    super(SampCapability.LOAD_VO_TABLE);
  }

  /**
   * Process the Samp message
   * @param connection hub connection
   * @param senderId sender identifier
   * @param msg Samp message
   * @return null
   *
   * @throws SampException if the votable url is incorrect or the votable can not be read
   */
  public Map<?, ?> processCall(final HubConnection connection, final String senderId, final Message msg) throws SampException {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("\tReceived '" + this.handledMType() + "' message from '" + senderId + "' : '" + msg + "'.");
    }

    // get url of votable (locally stored) :
    final String voTableURL = (String) msg.getParam("url");

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("VOTable URL = " + voTableURL);
    }

    if (voTableURL == null) {
      throw new SampException("Can not get the url of the votable");
    }

    final URI uri;
    try {
      uri = new URI(voTableURL);
    } catch (URISyntaxException use) {
      logger.log(Level.SEVERE, "invalid URI", use);

      throw new SampException("Can not read the votable : " + voTableURL, use);
    }

    try {

      final File voTableFile = new File(uri);

      final String votable = FileUtils.readFile(voTableFile);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("votable :\n" + votable);
      }

      // TODO : check SearchCal in votable

      // TODO LATER : accept any votable having a meta.ID ...

      // use an XSLT to transform the SearchCal votable document to an Aspro 2 Observation :
      final long start = System.nanoTime();

      final String document = XmlFactory.transform(votable, XSLT_FILE);

      if (logger.isLoggable(Level.INFO)) {
        logger.info("xslt : " + 1e-6d * (System.nanoTime() - start) + " ms.");
      }

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("document :\n" + document);
      }

      final ObservationManager om = ObservationManager.getInstance();

      final ObservationSetting searchCalObservation = om.load(new StringReader(document));

      final String targetName = searchCalObservation.getName();

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("science target : " + targetName);
      }

      final List<Target> calibrators = searchCalObservation.getTargets();

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("calibrators :");
        for (Target cal : calibrators) {
          logger.fine(cal.toString());
        }
      }

      // fire an observation change event (EDT) :
      SwingUtilities.invokeLater(new Runnable() {

        /**
         * Synchronized by EDT
         */
        public void run() {

          // science target position :
          int pos = om.getObservation().getTargetPosition(targetName);

          if (pos == -1) {
            MessagePane.showErrorMessage("Target '" + targetName + "' not found in SearchCal votable !");
          }

          // copy list of observation targets :
          final List<Target> editTargets = new ArrayList<Target>(om.getTargets());

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("initial targets :");
            for (Target t : editTargets) {
              logger.fine(t.toString());
            }
          }

          mergeTargets(pos, editTargets, calibrators);

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("updated targets :");
            for (Target t : editTargets) {
              logger.fine(t.toString());
            }
          }

          // update the complete list of targets :
          om.setTargets(editTargets);

          // TODO : better event handling :
          AsproGui.getInstance().getSettingPanel().getObservationForm().forceUpdateListTargets();

          om.fireObservationChanged();

          // change focus :
          AsproGui.getFrame().toFront();
        }
      });

    } catch (IOException ioe) {
      MessagePane.showErrorMessage("Can not read the votable :\n\n" + voTableURL);

      throw new SampException("Can not read the votable : " + voTableURL, ioe);
    }
    return null;
  }

  /**
   * Merge targets and calibrators
   * @param targetPos position of the science target for calibrators
   * @param editTargets current edited list of targets
   * @param calibrators list of calibrators
   */
  private static void mergeTargets(final int targetPos, final List<Target> editTargets, final List<Target> calibrators) {

    int pos = targetPos;

    // add calibrators in the target list after the science target :
    pos++;

    String name;
    for (Target cal : calibrators) {
      // add (cal) suffix in calibrator name :
      name = cal.getName() + " (cal)";

      if (!exist(editTargets, name)) {
        cal.setName(name);

        editTargets.add(pos, cal);
        pos++;
      }
    }
  }

  /**
   * Return true if the target is already present in the given list of targets
   * @param editTargets list of targets
   * @param name target name
   * @return true if the target is already present
   */
  private static boolean exist(final List<Target> editTargets, final String name) {
    for (Target t : editTargets) {
      if (t.getName().equals(name)) {
        return true;
      }
    }
    return false;
  }
}
