/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: SearchCalSampMessageHandler.java,v 1.7 2011-01-10 12:47:16 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.6  2010/12/17 15:09:39  bourgesl
 * added SearchCal origin to imported target
 * refactor to use calibrator flag and associations : updated merge targets
 *
 * Revision 1.5  2010/12/14 09:28:27  bourgesl
 * major update on mergeTargets to flag calibrators and update targets and target user informations
 *
 * Revision 1.4  2010/10/22 11:10:44  bourgesl
 * use App frame
 *
 * Revision 1.3  2010/10/11 14:15:37  bourgesl
 * SampMessageHandler refactoring
 *
 * Revision 1.2  2010/10/08 12:29:17  bourgesl
 * added error messages if nb(calibrators) = 0 or > 10
 *
 * Revision 1.1  2010/10/07 15:04:23  bourgesl
 * first SearchCal votable Samp handler : extract calibrators and merge them to the target list
 *
 */
package fr.jmmc.aspro.model.searchCal;

import fr.jmmc.aspro.gui.TargetEditorDialog;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.util.FileUtils;
import fr.jmmc.aspro.util.XmlFactory;
import fr.jmmc.mcs.gui.App;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.interop.SampCapability;
import fr.jmmc.mcs.interop.SampMessageHandler;
import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.logging.Level;
import javax.swing.SwingUtilities;
import org.astrogrid.samp.Message;
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
  /** maximum calibrators accepted at once */
  private final static int MAX_CALIBRATORS = 10;

  /**
   * Public constructor
   */
  public SearchCalSampMessageHandler() {
    super(SampCapability.LOAD_VO_TABLE);
  }

  /**
   * Implements message processing
   *
   * @param senderId public ID of sender client
   * @param message message with MType this handler is subscribed to
   * @throws SampException if any error occured while message processing
   */
  protected void processMessage(final String senderId, final Message message) throws SampException {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("\tReceived '" + this.handledMType() + "' message from '" + senderId + "' : '" + message + "'.");
    }

    // get url of votable (locally stored) :
    final String voTableURL = (String) message.getParam("url");

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

      // LATER : accept any votable having a meta.ID to import target and not only searchCal calibrators ...

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

      // Use invokeLater to avoid concurrency and ensure that 
      // data model is modified and fire events using Swing EDT :
      SwingUtilities.invokeLater(new Runnable() {

        public void run() {

          if (TargetEditorDialog.isTargetEditorActive()) {
            MessagePane.showErrorMessage("Please close the target editor first !");
            return;
          }

          // check that science target is present :
          if (om.getTarget(targetName) == null) {
            MessagePane.showErrorMessage("Target '" + targetName + "' not found in targets (wrong SearchCal target) !");
            return;
          }

          // check the number of calibrators :
          if (calibrators.isEmpty()) {
            MessagePane.showErrorMessage("No calibrator found in SearchCal response !");
            return;
          }

          if (calibrators.size() > MAX_CALIBRATORS) {
            MessagePane.showErrorMessage("Too many calibrators (" + calibrators.size() + ") found in SearchCal response !");
            return;
          }

          // use deep copy of the current observation to manipulate target and calibrator list properly :
          final ObservationSetting cloned = (ObservationSetting) om.getObservation().clone();

          // Prepare the data model (editable targets and user infos) :
          final List<Target> editTargets = cloned.getTargets();
          final TargetUserInformations editTargetUserInfos = cloned.getOrCreateTargetUserInfos();

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("initial targets :");
            for (Target t : editTargets) {
              logger.fine(t.toString());
            }
          }

          final String report = mergeTargets(editTargets, editTargetUserInfos, targetName, calibrators);

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("updated targets :");
            for (Target t : editTargets) {
              logger.fine(t.toString());
            }
          }

          // update the complete list of targets and force to update references :
          // needed to replace old target references by the new calibrator targets :
          om.updateTargets(editTargets, editTargetUserInfos);

          if (logger.isLoggable(Level.INFO)) {
            logger.info(report);
          }

          // display report message :
          MessagePane.showMessage(report);

          // change focus :
          App.getFrame().toFront();
        }
      });

    } catch (IOException ioe) {
      MessagePane.showErrorMessage("Can not read the votable :\n\n" + voTableURL);

      throw new SampException("Can not read the votable : " + voTableURL, ioe);
    }
  }

  /**
   * Merge targets and calibrators
   * @param editTargets edited list of targets
   * @param editTargetUserInfos edited target user informations
   * @param targetName science target name
   * @param calibrators list of calibrators for the science target
   * @return merge operation report
   */
  private static String mergeTargets(final List<Target> editTargets, final TargetUserInformations editTargetUserInfos,
                                     final String targetName, final List<Target> calibrators) {
    // report buffer :
    final StringBuilder sb = new StringBuilder(512);
    sb.append("Import SearchCal calibrators to target [").append(targetName).append("]\n\n");

    final Target scienceTarget = Target.getTarget(targetName, editTargets);

    if (scienceTarget != null) {
      String calName;
      Target oldCal;

      for (Target newCal : calibrators) {
        calName = Target.formatName(newCal.getName());

        // update target name :
        newCal.setName(calName);
        newCal.setOrigin("SearchCal");

        oldCal = Target.getTarget(calName, editTargets);

        if (oldCal == null) {

          // append the missing target :
          editTargets.add(newCal);

          // define it as a calibrator :
          editTargetUserInfos.addCalibrator(newCal);

          // associate it to the science target :
          editTargetUserInfos.addCalibratorToTarget(scienceTarget, newCal);

          // report message :
          sb.append(calName).append(" added as a calibrator\n");

        } else {
          // target already exist : always replace old target by the SearchCal calibrator (gilles)
          // even if the old target was modified by the user ...

          // check if the existing target had calibrators :
          if (editTargetUserInfos.hasCalibrators(oldCal)) {
            final List<Target> oldCalibrators = editTargetUserInfos.getCalibrators(oldCal);

            // report message :
            sb.append("WARNING : ").append(calName).append(" had calibrators that were removed : ");
            for (Target cal : oldCalibrators) {
              sb.append(cal.getName()).append(' ');
            }
            sb.append('\n');

            // remove calibrators related to the calibrator target :
            oldCalibrators.clear();
          }

          // note : oldCal and newCal are equals() so following operations are possible :

          if (!editTargetUserInfos.isCalibrator(oldCal)) {
            // define it as a calibrator :
            editTargetUserInfos.addCalibrator(newCal);

            // report message :
            sb.append(calName).append(" updated and flagged as a calibrator\n");
          } else {
            // report message :
            sb.append(calName).append(" updated\n");
          }

          // associate it to the science target :
          editTargetUserInfos.addCalibratorToTarget(scienceTarget, newCal);

          // replace the old target by the new calibrator :
          // note : the position of the target is not the same :
          editTargets.remove(oldCal);
          editTargets.add(newCal);
        }
      }
    }
    return sb.toString();
  }
}
