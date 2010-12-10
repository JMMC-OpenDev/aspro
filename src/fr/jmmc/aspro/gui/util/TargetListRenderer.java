/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TargetListRenderer.java,v 1.1 2010-12-10 17:09:47 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.Target;
import java.awt.Component;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JList;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;

/**
 * This custom renderer defines the target icon (calibrator or science) and use the target Name
 * @author bourgesl
 */
public final class TargetListRenderer extends DefaultListCellRenderer {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;

  /* members */
  /** delegate */
  private final TargetRenderer delegate;

  /**
   * Public constructor
   * @param renderer target renderer
   */
  public TargetListRenderer(final TargetRenderer renderer) {
    this.delegate = renderer;
  }

  /**
   * Return a component that has been configured to display the specified
   * value. That component's <code>paint</code> method is then called to
   * "render" the cell.  If it is necessary to compute the dimensions
   * of a list because the list cells do not have a fixed size, this method
   * is called to generate a component on which <code>getPreferredSize</code>
   * can be invoked.
   *
   * @param list The JList we're painting.
   * @param value The value returned by list.getModel().getElementAt(index).
   * @param index The cells index.
   * @param isSelected True if the specified cell was selected.
   * @param cellHasFocus True if the specified cell has the focus.
   * @return A component whose paint() method will render the specified value.
   *
   * @see JList
   * @see ListSelectionModel
   * @see ListModel
   */
  @Override
  public Component getListCellRendererComponent(
          JList list,
          Object value,
          int index,
          boolean isSelected,
          boolean cellHasFocus) {

    final String val;
    if (value == null) {
      val = null;
    } else {
      final Target target = (Target) value;
      val = target.getName();
    }

    super.getListCellRendererComponent(
            list, val, index,
            isSelected, cellHasFocus);

    if (value instanceof Target) {
      delegate.setIcon(this, (Target) value);
    }

    return this;
  }
}
