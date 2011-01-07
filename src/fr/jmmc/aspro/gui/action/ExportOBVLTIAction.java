/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ExportOBVLTIAction.java,v 1.16 2011-01-07 13:23:16 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.15  2010/12/15 13:35:24  bourgesl
 * use observation default file name
 *
 * Revision 1.14  2010/10/04 16:25:25  bourgesl
 * proper IO exception handling
 *
 * Revision 1.13  2010/10/01 15:32:28  bourgesl
 * use MessagePane.showConfirmFileOverwrite
 *
 * Revision 1.12  2010/09/24 15:54:25  bourgesl
 * better exception handling + use MessagePane
 *
 * Revision 1.11  2010/09/01 16:24:42  bourgesl
 * use JMCS DissmissableMessageDialog
 *
 * Revision 1.10  2010/09/01 13:21:32  bourgesl
 * added popup message
 *
 * Revision 1.9  2010/09/01 12:57:13  bourgesl
 * added runtime exception message to user message dialog
 *
 * Revision 1.8  2010/06/17 10:02:50  bourgesl
 * fixed warning hints - mainly not final static loggers
 *
 * Revision 1.7  2010/06/10 13:43:42  bourgesl
 * fixed file description with extension
 *
 * Revision 1.6  2010/06/09 12:53:42  bourgesl
 * actionPerformed() method renamed to process() because it does not implement Action
 * javadoc
 *
 * Revision 1.5  2010/05/26 15:29:14  bourgesl
 * light refactoring and javadoc
 *
 * Revision 1.4  2010/05/11 12:03:17  bourgesl
 * fix : check the file extension before the existence of the file to display the confirm dialog
 *
 * Revision 1.3  2010/05/06 15:42:18  bourgesl
 * use HA Min/Max + FT Mode for the target in the observation settings
 *
 * Revision 1.2  2010/05/05 14:29:09  bourgesl
 * added ha Min / Max to generate OB with correct LST intervals
 *
 * Revision 1.1  2010/04/14 13:09:23  bourgesl
 * renamed Amber action to VLTI action (AMBER + MIDI)
 *
 * Revision 1.2  2010/04/13 15:54:48  bourgesl
 * javadoc
 *
 * Revision 1.1  2010/04/02 10:07:35  bourgesl
 * simple OB generation for AMBER
 *
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.gui.UVCoveragePanel;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.AdaptiveOptics;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.Telescope;
import fr.jmmc.aspro.model.util.SpectralBandUtils;
import fr.jmmc.aspro.ob.ExportOBVLTI;
import fr.jmmc.aspro.util.FileUtils;
import fr.jmmc.mcs.astro.Band;
import fr.jmmc.mcs.gui.DismissableMessagePane;
import fr.jmmc.mcs.gui.MessagePane;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.util.FileFilterRepository;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.List;
import java.util.logging.Level;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileFilter;

/**
 * This class implements the OB generation for VLTI instruments.
 *
 * @author bourgesl
 */
public class ExportOBVLTIAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class name. This name is used to register to the ActionRegistrar */
  public final static String className = "fr.jmmc.aspro.gui.action.ExportOBVLTIAction";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(className);
  /** OBX mime type */
  public static final String OBX_MIME_TYPE = "application/obx";
  /** OBX extension */
  public static final String OBX_EXT = "obx";
  /** P2PP file prefix for science targets */
  public static final String OB_SCIENCE = "SCI";
  /** P2PP file prefix for calibrator targets */
  public static final String OB_CALIBRATOR = "CAL";
  /** Eso warning message */
  public static final String ESO_WARNING = "Please check that your observing block \n conforms to the current ESO Call for Proposal \n (object magnitudes, instrument limits ...)";
  /** double formatter for magnitudes */
  protected final static NumberFormat df2 = new DecimalFormat("0.##");
  /** action singleton */
  private static final ExportOBVLTIAction instance = new ExportOBVLTIAction();

  /**
   * Return the singleton ExportOBVLTIAction instance
   * @return ExportOBVLTIAction instance
   */
  public static ExportOBVLTIAction getInstance() {
    return instance;
  }

  /* members */
  /** last directory used to save a file; by default = user home */
  private String lastDir = System.getProperty("user.home");

  /**
   * Forbidden Constructor
   */
  private ExportOBVLTIAction() {
    super();

    FileFilterRepository.getInstance().put(OBX_MIME_TYPE, OBX_EXT, "Observing Block (" + OBX_EXT + ")");
  }

  /**
   * Execute the action.
   *
   * Note : the action event's source must be the UVCoveragePanel instance
   *
   * @param event action event
   */
  public void process(final ActionEvent event) {
    if (!(event.getSource() instanceof UVCoveragePanel)) {
      return;
    }
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("process");
    }

    final UVCoveragePanel uvCoveragePanel = (UVCoveragePanel) event.getSource();

    // extract UV Coverage Panel information :

    // TODO CALS : get both science target and calibrator target :
    final Target target = uvCoveragePanel.getSelectedTarget();

    File file = null;

    final JFileChooser fileChooser = new JFileChooser();
    fileChooser.setFileFilter(getFileFilter());

    if (this.getLastDir() != null) {
      fileChooser.setCurrentDirectory(new File(this.getLastDir()));
    }

    // default OB file name :
    fileChooser.setSelectedFile(new File(fileChooser.getCurrentDirectory(), generateOBFileName(target)));

    fileChooser.setDialogTitle("Export the target [" + target.getName() + "] as an Observing Block");

    final int returnVal = fileChooser.showSaveDialog(null);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      file = checkFileExtension(fileChooser.getSelectedFile());

      if (file.exists()) {
        if (!MessagePane.showConfirmFileOverwrite(file.getName())) {
          file = null;
        }
      }
    } else {
      file = null;
    }

    // If a file was defined (No cancel in the dialog)
    if (file != null) {
      this.setLastDir(file.getParent());

      try {
        ExportOBVLTI.process(file, target);

        StatusBar.show(file.getName() + " created.");

        // PoP up to validate OB file against ESO CfP :
        DismissableMessagePane.show(null, ESO_WARNING, Preferences.getInstance(), "ESO_OB_WARNING");

      } catch (IOException ioe) {
        MessagePane.showErrorMessage(
                "Could not export to file : " + file.getName(), ioe);
      }
    }
  }

  /**
   * Return the file filter
   * @return file filter
   */
  protected FileFilter getFileFilter() {
    return FileFilterRepository.get(OBX_MIME_TYPE);
  }

  /**
   * Check if the given file has the correct extension. If not, return a new file with it
   * @param file file to check
   * @return given file or new file with the correct extension
   */
  protected File checkFileExtension(final File file) {
    final String ext = FileUtils.getExtension(file);

    if (!OBX_EXT.equals(ext)) {
      return new File(file.getParentFile(), file.getName() + "." + OBX_EXT);
    }
    return file;
  }

  /**
   * Return the last directory used
   * @return last directory used
   */
  protected String getLastDir() {
    return this.lastDir;
  }

  /**
   * Define the last directory used
   * @param lastDir new value
   */
  protected void setLastDir(String lastDir) {
    this.lastDir = lastDir;
  }

  /**
   * Generate the Observing block file name using the given target
   * @param target target to use
   * @return Observing block file name
   */
  private String generateOBFileName(final Target target) {

    final ObservationManager om = ObservationManager.getInstance();
    final ObservationSetting observation = om.getObservation();

    // get instrument band :
    final FocalInstrumentMode insMode = observation.getInstrumentConfiguration().getFocalInstrumentMode();
    if (insMode == null) {
      throw new IllegalStateException("the instrumentMode is empty !");
    }

    final double lambda = insMode.getWaveLength();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("lambda = " + lambda);
    }

    final Band band = Band.findBand(lambda);
    final SpectralBand insBand = SpectralBandUtils.findBand(band);

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("insBand = " + insBand);
    }

    // get AO band :
    final List<Station> stations = observation.getInstrumentConfiguration().getStationList();

    // All telescopes in a configuration have the same AO system :
    final Telescope tel = stations.get(0).getTelescope();

    // AO :
    final AdaptiveOptics ao = tel.getAdaptiveOptics();

    final SpectralBand aoBand = (ao != null) ? ao.getBand() : null;
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("aoBand = " + aoBand);
    }

    // extract instrument and AO fluxes :
    final Double insMag = target.getFlux(insBand);
    final Double aoMag = target.getFlux(aoBand);

    // Decorate the target name :
    final StringBuilder sb = new StringBuilder(32);
    sb.append(target.getName());

    sb.append('_').append(insBand.name());
    sb.append(df2.format(ExportOBVLTI.getMagnitude(insMag)));

    sb.append('_').append(aoBand.name());
    sb.append(df2.format(ExportOBVLTI.getMagnitude(aoMag)));

    final String targetNameWithMagnitudes = sb.toString();

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("targetNameWithMagnitudes = " + targetNameWithMagnitudes);
    }

    final String prefix = om.isCalibrator(target) ? OB_CALIBRATOR : OB_SCIENCE;

    return observation.generateFileName(targetNameWithMagnitudes, prefix, OBX_EXT);
  }
}
