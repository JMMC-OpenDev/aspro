/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TargetTransferHandler.java,v 1.1 2010-12-03 09:34:01 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.util.Arrays;
import java.util.logging.Level;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JTree;
import javax.swing.TransferHandler;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;

/**
 *
 * @author bourgesl
 */
public final class TargetTransferHandler extends TransferHandler {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.util.TargetTransferHandler";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* members */
  /** list of edited target user informations (clone) */
  private final TargetUserInformations editTargetUserInfos;

  /**
   * Public constructor
   * @param targetUserInfos target user informations
   */
  public TargetTransferHandler(final TargetUserInformations targetUserInfos) {
    super();
    this.editTargetUserInfos = targetUserInfos;
  }

  /**
   * Create a Transferable to use as the source for a data transfer.
   *
   * @param comp  The component holding the data to be transfered.  This
   *  argument is provided to enable sharing of TransferHandlers by
   *  multiple components.
   * @return  The representation of the data to be transfered.
   *
   */
  @Override
  protected Transferable createTransferable(final JComponent comp) {
    if (comp instanceof JList) {
      final JList list = (JList) comp;
      final Object[] values = list.getSelectedValues();

      if (values == null || values.length == 0) {
        return null;
      }

      logger.severe("JList drag selection : " + Arrays.deepToString(values));

      return new GenericTransferable(values);
    } else if (comp instanceof JTree) {
      final JTree tree = (JTree) comp;

      final DefaultMutableTreeNode node = (DefaultMutableTreeNode) tree.getLastSelectedPathComponent();

      /* if nothing is selected */
      if (node == null) {
        return null;
      }

      /* retrieve the node that was selected */
      final Object userObject = node.getUserObject();

      if (userObject != null) {

        logger.severe("JTree drag selection : " + userObject);

        if (userObject instanceof Target) {
          // Target so not null :

          // Check if the target is a calibrator

          if (isCalibrator((Target) userObject)) {
            return new GenericTransferable(userObject);
          }

        } else {
          logger.severe("unsupported object type : " + comp);
        }
      }

    } else {
      logger.severe("unsupported component : " + comp);
    }

    return null;
  }

  private boolean isCalibrator(final Target target) {
    return this.editTargetUserInfos.getCalibrators().contains(target);
  }

  /**
   * Returns the type of transfer actions supported by the source;
   * any bitwise-OR combination of {@code COPY}, {@code MOVE}
   * and {@code LINK}.
   * <p>
   * Some models are not mutable, so a transfer operation of {@code MOVE}
   * should not be advertised in that case. Returning {@code NONE}
   * disables transfers from the component.
   *
   * @param c  the component holding the data to be transferred;
   *           provided to enable sharing of <code>TransferHandler</code>s
   * @return {@code COPY} if the transfer property can be found,
   *          otherwise returns <code>NONE</code>
   */
  @Override
  public int getSourceActions(final JComponent c) {
    return COPY;
  }

  /**
   * Indicates whether a component will accept an import of the given
   * set of data flavors prior to actually attempting to import it.
   * <p>
   * Note: Swing now calls the newer version of <code>canImport</code>
   * that takes a <code>TransferSupport</code>, which in turn calls this
   * method (only if the component in the {@code TransferSupport} is a
   * {@code JComponent}). Developers are encouraged to call and override the
   * newer version as it provides more information (and is the only
   * version that supports use with a {@code TransferHandler} set directly
   * on a {@code JFrame} or other non-{@code JComponent}).
   *
   * @param comp  the component to receive the transfer;
   *              provided to enable sharing of <code>TransferHandler</code>s
   * @param transferFlavors  the data formats available
   * @return  true if the data can be inserted into the component, false otherwise
   * @see #canImport(TransferHandler.TransferSupport)
   */
  public boolean canImport(JComponent comp,
                           DataFlavor[] transferFlavors) {

    if (comp instanceof JTree) {
//      logger.severe("Destination component : " + comp);

      // tester si le noeud courant est bien une target ...

      // Dur dur à faire
      return true;
    }
    // List does not accept drop ...
    return false;
  }

  /**
   * Invoked after data has been exported.  This method should remove
   * the data that was transferred if the action was <code>MOVE</code>.
   * <p>
   * This method is implemented to do nothing since <code>MOVE</code>
   * is not a supported action of this implementation
   * (<code>getSourceActions</code> does not include <code>MOVE</code>).
   *
   * @param source the component that was the source of the data
   * @param data   The data that was transferred or possibly null
   *               if the action is <code>NONE</code>.
   * @param action the actual action that was performed
   */
  @Override
  protected void exportDone(final JComponent source, final Transferable data, final int action) {
    try {
      logger.severe("exportDone : " + data.getTransferData(null));
    } catch (Exception e) {
      logger.log(Level.SEVERE, "exportDone", e);
    }
  }
}
