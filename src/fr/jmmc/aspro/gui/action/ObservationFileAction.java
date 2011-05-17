/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.action;

import fr.jmmc.mcs.util.FileFilterRepository;
import fr.jmmc.mcs.util.FileUtils;
import fr.jmmc.mcs.util.RegisteredAction;
import java.io.File;
import javax.swing.filechooser.FileFilter;

/**
 * Abstract file action dedicated to handle the file filter for observation files
 * @author bourgesl
 */
public abstract class ObservationFileAction extends RegisteredAction {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Observation settings mime type */
  public static final String OBSERVATION_MIME_TYPE = "application/x-aspro+xml";
  /** Observation extension = asprox */
  public static final String OBS_EXT = "asprox";
  /** last directory used to load/save an Observation file; by default = user home */
  private static String lastDir = System.getProperty("user.home");

  /**
   * Constructor, that automatically register the action in RegisteredAction.
   * Action name, icon, accelerator and description is first inited using
   * fieldName to build a MCSAction.
   * @param classPath the path of the class containing the field pointing to
   * the action, in the form returned by 'getClass().getName();'.
   * @param fieldName the name of the field pointing to the action.
   */
  public ObservationFileAction(final String classPath, final String fieldName) {
    super(classPath, fieldName);

    FileFilterRepository.getInstance().put(OBSERVATION_MIME_TYPE, OBS_EXT, "Aspro Observation Settings (" + OBS_EXT + ")");
  }

  /**
   * Return the file filter
   * @return file filter
   */
  protected FileFilter getFileFilter() {
    return FileFilterRepository.get(OBSERVATION_MIME_TYPE);
  }

  /**
   * Check if the given file has the correct extension. If not, return a new file with it
   * @param file file to check
   * @return given file or new file with the correct extension
   */
  protected File checkFileExtension(final File file) {
    final String ext = FileUtils.getExtension(file);

    if (!OBS_EXT.equals(ext)) {
      return new File(file.getParentFile(), file.getName() + "." + OBS_EXT);
    }
    return file;
  }

  /**
   * Return the last directory used
   * @return last directory used
   */
  protected String getLastDir() {
    return lastDir;
  }

  /**
   * Define the last directory used
   * @param dir new value
   */
  protected void setLastDir(String dir) {
    lastDir = dir;
  }
}
