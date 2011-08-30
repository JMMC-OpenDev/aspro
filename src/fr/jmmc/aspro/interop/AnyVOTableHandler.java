/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.interop;

import fr.jmmc.aspro.gui.TargetEditorDialog;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.BaseValue;
import fr.jmmc.aspro.model.oi.CalibratorInformations;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.StringValue;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.util.XmlFactory;
import fr.jmmc.jmal.ALX;
import fr.jmmc.mcs.gui.App;
import fr.jmmc.mcs.gui.MessagePane;
import java.io.IOException;
import java.io.StringReader;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.SwingUtilities;

/**
 * This class handles generic VOTables (targets) and PIVOT VOTables
 * 
 * @author bourgesl
 */
public final class AnyVOTableHandler {

  /** Class logger */
  private static final Logger logger = Logger.getLogger(VotableSampMessageHandler.class.getName());
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

    final String document = XmlFactory.transform(votable, XSLT_FILE);

    if (logger.isLoggable(Level.INFO)) {
      logger.info("VOTable transformation (XSLT) : " + 1e-6d * (System.nanoTime() - start) + " ms.");
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("document :\n" + document);
    }

    final ObservationManager om = ObservationManager.getInstance();

    final ObservationSetting newObservation = om.load(new StringReader(document));

    if (newObservation != null) {
      // Use invokeLater to avoid concurrency and ensure that 
      // data model is modified and fire events using Swing EDT :
      SwingUtilities.invokeLater(new Runnable() {

        public void run() {

          om.resetAndChangeObservation(newObservation);

          // bring this application to front :
          App.showFrameToFront();
        }
      });
    }
  }
}
