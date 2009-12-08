/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservationFileAction.java,v 1.1 2009-12-08 13:09:55 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.action;

import fr.jmmc.mcs.util.FileFilterRepository;
import fr.jmmc.mcs.util.RegisteredAction;
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
  }

  protected FileFilter getObservationFileFilter() {
    final FileFilterRepository fileFilterRepository = FileFilterRepository.getInstance();

    FileFilter filter = fileFilterRepository.get(OBSERVATION_MIME_TYPE);

    if (filter == null) {
      fileFilterRepository.put(OBSERVATION_MIME_TYPE, "xml", "Aspro Observation Settings (xml)");

      filter = fileFilterRepository.get(OBSERVATION_MIME_TYPE);
    }

    return filter;
  }

  public String getLastDir() {
    return lastDir;
  }

  public void setLastDir(String lastDir) {
    this.lastDir = lastDir;
  }

  
}
