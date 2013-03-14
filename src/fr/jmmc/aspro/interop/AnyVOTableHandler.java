/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.interop;

import fr.jmmc.aspro.gui.TargetEditorDialog;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.jmcs.App;
import fr.jmmc.jmcs.gui.component.MessagePane;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.util.XslTransform;
import java.io.IOException;
import java.io.StringReader;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class handles generic VOTables (targets) and PIVOT VOTables
 * 
 * @author bourgesl
 */
public final class AnyVOTableHandler {

  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(AnyVOTableHandler.class.getName());
  /** XSLT file path */
  private final static String XSLT_FILE = "fr/jmmc/aspro/interop/vot2AsproObservation.xsl";

  /**
   * Private constructor
   */
  private AnyVOTableHandler() {
    super();
  }

  /**
   * Process the given votable
   *
   * @param votable votable to process
   * 
   * @throws IOException if an I/O exception occured
   * @throws IllegalArgumentException if the file is not an Observation
   */
  static void processMessage(final String votable) throws IOException {

    // use an XSLT to transform the SearchCal votable document to an Aspro 2 Observation :
    final long start = System.nanoTime();

    final String document = XslTransform.transform(votable, XSLT_FILE);

    logger.info("VOTable transformation (XSLT): {} ms.", 1e-6d * (System.nanoTime() - start));

    logger.debug("document :\n{}", document);

    final ObservationManager om = ObservationManager.getInstance();

    try {

      // throws IllegalArgumentException if the document can not be loaded (invalid or unmarshalling exception):
      final ObservationSetting newObservation = om.load(new StringReader(document));

      if (newObservation != null) {

        final List<Target> targets = newObservation.getTargets();

        if (logger.isDebugEnabled()) {
          logger.debug("targets:");
          for (Target cal : targets) {
            logger.debug(cal.toString());
          }
        }

        // Use invokeLater to avoid concurrency and ensure that 
        // data model is modified and fire eventsarg using Swing EDT :
        SwingUtils.invokeLaterEDT(new Runnable() {
          @Override
          public void run() {

            if (TargetEditorDialog.isTargetEditorActive()) {
              MessagePane.showErrorMessage("Please close the target editor first !");
              return;
            }

            // check the number of targets :
            if (targets.isEmpty()) {
              MessagePane.showErrorMessage("No valid target found in VOTable : missing name or RA/DEC coordinates (J2000) !");
              return;
            }

            if (!VotableSampMessageHandler.confirmImport(targets.size())) {
              return;
            }

            // TODO: use the observationContext.operation ?
            if (newObservation.getInterferometerConfiguration() == null
                    || newObservation.getInstrumentConfiguration() == null) {
              // empty configuration: add targets ...

              // use deep copy of the current observation to manipulate target and calibrator list properly :
              final ObservationSetting obsCloned = om.getMainObservation().deepClone();

              // Prepare the data model (editable targets and user infos) :
              final List<Target> editTargets = obsCloned.getTargets();
              final TargetUserInformations editTargetUserInfos = obsCloned.getOrCreateTargetUserInfos();

              if (logger.isDebugEnabled()) {
                logger.debug("initial targets:");
                for (Target t : editTargets) {
                  logger.debug(t.toString());
                }
              }

              final String report = mergeTargets(editTargets, targets);

              if (logger.isDebugEnabled()) {
                logger.debug("updated targets:");
                for (Target t : editTargets) {
                  logger.debug(t.toString());
                }
              }

              // update the complete list of targets and force to update references :
              // needed to replace old target references by the new calibrator targets :
              om.updateTargets(editTargets, editTargetUserInfos);

              if (logger.isInfoEnabled()) {
                logger.info(report);
              }

              // bring this application to front :
              App.showFrameToFront();

              // display report message :
              MessagePane.showMessage(report);

            } else {
              om.resetAndChangeObservation(newObservation);
            }

            // bring this application to front :
            App.showFrameToFront();
          }
        });
      }
    } catch (IllegalArgumentException iae) {
      // Report both Observation and VOTable in a new IllegalArgumentException to get them in the feedback report:
      throw new IllegalArgumentException("Invalid generated Aspro2 Observation:\n\n" + document + "\n\nSAMP VOTable argument:\n\n" + votable, iae);
    }
  }

  /**
   * Merge targets
   * @param editTargets edited list of targets
   * @param targets list of targets
   * @return merge operation report
   */
  private static String mergeTargets(final List<Target> editTargets, final List<Target> targets) {
    // report buffer :
    final StringBuilder sb = new StringBuilder(512);
    sb.append("Import targets from VOTable\n\n");

    String targetName;
    Target oldTarget;

    for (Target newTarget : targets) {
      targetName = Target.formatName(newTarget.getName());

      // update target name :
      newTarget.setName(targetName);
      newTarget.setOrigin("VOTable");

      oldTarget = Target.getTarget(targetName, editTargets);

      if (oldTarget == null) {

        // append the missing target :
        editTargets.add(newTarget);

        // report message :
        sb.append(targetName).append(" added\n");

      } else {
        // target already exist : skip it as the old target may be modified by the user ...

        // report message :
        sb.append(targetName).append(" skipped\n");
      }
    }
    return sb.toString();
  }
}
