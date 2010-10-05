/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservationFileAction.java,v 1.7 2010-06-29 11:55:54 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.6  2010/06/14 13:16:36  bourgesl
 * changed aspro file extension to 'asprox'
 *
 * Revision 1.5  2010/06/10 13:44:01  bourgesl
 * Observation File extension is 'aspro'
 * fixed file description with extension
 *
 * Revision 1.4  2010/06/09 12:54:12  bourgesl
 * javadoc
 *
 * Revision 1.3  2010/04/02 10:06:29  bourgesl
 * add missing xml extension to the file name
 *
 * Revision 1.2  2010/01/14 17:02:38  bourgesl
 * FileFilter fix
 *
 * Revision 1.1  2009/12/08 13:09:55  bourgesl
 * Added FileFilter for observation settings
 *
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.aspro.util.FileUtils;
import fr.jmmc.mcs.util.FileFilterRepository;
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

  /* members */
  /** last directory used to save a file; by default = user home */
  private String lastDir = System.getProperty("user.home");

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
    return FileFilterRepository.getInstance().get(OBSERVATION_MIME_TYPE);
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
    return this.lastDir;
  }

  /**
   * Define the last directory used
   * @param lastDir new value
   */
  protected void setLastDir(String lastDir) {
    this.lastDir = lastDir;
  }
}
