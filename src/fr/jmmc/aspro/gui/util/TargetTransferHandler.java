/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TargetTransferHandler.java,v 1.2 2010-12-03 16:28:48 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.1  2010/12/03 09:34:01  bourgesl
 * first try using drag and drop between calibrator list and target tree
 * added calibrator list coupled with Calibrator button
 * changed font for target tree
 *
 */
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.Target;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.util.Arrays;
import java.util.logging.Level;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.TransferHandler;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

/**
 * This custom transfer handler manages the Drag and Drop of Target objects (calibrators) between JList and JTree instances.
 *
 * JTree.dropMode = 'USE_SELECTION' to be compatible with Java 1.5
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

  /**
   * Public constructor
   */
  public TargetTransferHandler() {
    super();
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
      final Object userObject = list.getSelectedValue();

      if (userObject == null) {
        return null;
      }

      logger.severe("JList drag selection : " + userObject);

      if (userObject instanceof Target) {
        // Target so not null :
        final Target srcTarget = (Target) userObject;

        // note : we suppose that the target is a calibrator :

        // only drag calibrators (not science targets) :
        return new GenericTransferable(new Object[]{srcTarget, null});

      } else {
        logger.severe("unsupported object type : " + comp);
      }

    } else if (comp instanceof TargetJTree) {
      final TargetJTree tree = (TargetJTree) comp;

      final DefaultMutableTreeNode sourceNode = tree.getLastSelectedNode();

      /* if nothing is selected */
      if (sourceNode == null) {
        return null;
      }

      /* retrieve the node that was selected */
      final Object userObject = sourceNode.getUserObject();

      if (userObject != null) {

        logger.severe("JTree drag selection : " + userObject);

        if (userObject instanceof Target) {
          // Target so not null :
          final Target srcTarget = (Target) userObject;

          // Check if the target is a calibrator

          if (tree.isCalibrator((Target) userObject)) {
            // only drag calibrators (not science targets) :
            return new GenericTransferable(new Object[]{srcTarget, new TreePath(sourceNode.getPath())});
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
    return COPY_OR_MOVE;
  }

  /**
   * Indicates whether a component will accept an import of the given
   * set of data flavors prior to actually attempting to import it.
   *
   * @param comp  the component to receive the transfer;
   *              provided to enable sharing of <code>TransferHandler</code>s
   * @param transferFlavors  the data formats available
   * @return  true if the data can be inserted into the component, false otherwise
   * @see #canImport(TransferHandler.TransferSupport)
   */
  @Override
  public boolean canImport(final JComponent comp,
                           final DataFlavor[] transferFlavors) {

    if (comp instanceof TargetJTree) {

      // TODO : tester si le noeud courant est bien une target ...

      // Pas evident a faire (Java 5)

      return true;
    }
    // List does not accept drop ...
    return false;
  }

  /**
   * Causes a transfer to a component from a clipboard or a
   * DND drop operation.  The <code>Transferable</code> represents
   * the data to be imported into the component.
   *
   * @param comp  the component to receive the transfer;
   *              provided to enable sharing of <code>TransferHandler</code>s
   * @param data     the data to import
   * @return  true if the data was inserted into the component, false otherwise
   * @see #importData(TransferHandler.TransferSupport)
   */
  @Override
  public boolean importData(final JComponent comp, final Transferable data) {

    if (data != null && comp instanceof TargetJTree) {
      Target srcTarget = null;
      try {
        final Object[] objects = (Object[]) data.getTransferData(DataFlavor.stringFlavor);

        logger.severe("transfered objects : " + Arrays.toString(objects));

        srcTarget = (Target)objects[0];

      } catch (Exception e) {
        logger.log(Level.SEVERE, "failure", e);
      }

      if (srcTarget != null) {
        logger.severe("target (calibrator) : " + srcTarget);

        final TargetJTree tree = (TargetJTree) comp;

        final DefaultMutableTreeNode destNode = tree.getLastSelectedNode();

        /* if nothing is selected */
        if (destNode == null) {
          return false;
        }

        /* retrieve the node that was selected */
        final Object userObject = destNode.getUserObject();

        if (userObject != null) {

          logger.severe("JTree DROP selection : " + userObject);

          if (userObject instanceof Target) {

            final Target destTarget = (Target) userObject;

            return tree.addCalibrator(srcTarget, destNode, destTarget);

          } else {
            logger.severe("unsupported object type : " + comp);
          }
        }
      }

      return false;
    }
    return false;
  }

  /**
   * Invoked after data has been exported.  This method should remove
   * the data that was transferred if the action was <code>MOVE</code>.
   *
   * @param source the component that was the source of the data
   * @param data   The data that was transferred or possibly null
   *               if the action is <code>NONE</code>.
   * @param action the actual action that was performed
   */
  @Override
  protected void exportDone(final JComponent source, final Transferable data, final int action) {
    if (data != null && action == MOVE) {
      if (source instanceof TargetJTree) {
        logger.severe("exportDone : " + data + " action = " + ((action == MOVE) ? "move" : "copy"));

        // TODO : remove the previous node :

      }
    }
  }
}
