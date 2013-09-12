/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.TransferHandler;
import javax.swing.tree.DefaultMutableTreeNode;

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
  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(TargetTransferHandler.class.getName());

  /* members */
  /** list of edited targets (clone) */
  private final List<Target> editTargets;
  /** edited target user informations (clone) */
  private final TargetUserInformations editTargetUserInfos;

  /**
   * Public constructor
   * @param targets list of targets
   * @param targetUserInfos target user informations
   */
  public TargetTransferHandler(final List<Target> targets, final TargetUserInformations targetUserInfos) {
    super();

    this.editTargets = targets;
    this.editTargetUserInfos = targetUserInfos;
  }

  /**
   * Create a Transferable to use as the source for a data transfer.
   *
   * @param sourceComponent  The component holding the data to be transfered.  This
   *  argument is provided to enable sharing of TransferHandlers by
   *  multiple components.
   * @return  The representation of the data to be transfered.
   *
   */
  @Override
  protected Transferable createTransferable(final JComponent sourceComponent) {
    if (sourceComponent instanceof JList) {
      final JList list = (JList) sourceComponent;
      final Object userObject = list.getSelectedValue();

      if (userObject != null) {
        if (userObject instanceof Target) {
          // Target so not null :
          final Target sourceTarget = (Target) userObject;

          // Check if the target is a calibrator ?
          if (isCalibrator(sourceTarget)) {

            logger.debug("JList DRAG selection: {}", sourceTarget);

            // only drag calibrators (not science targets) :
            return new TargetTransferable(sourceTarget.getIdentifier());
          }
        } else {
          logger.warn("unsupported object type: {}", userObject);
        }
      }

    } else if (sourceComponent instanceof TargetJTree) {
      final TargetJTree tree = (TargetJTree) sourceComponent;
      final DefaultMutableTreeNode sourceNode = tree.getLastSelectedNode();

      if (sourceNode != null) {
        /* retrieve the node that was selected */
        final Object userObject = sourceNode.getUserObject();

        if (userObject != null) {
          if (userObject instanceof Target) {
            // Target so not null :
            final Target sourceTarget = (Target) userObject;

            // Check if the target is a calibrator ?
            if (isCalibrator(sourceTarget)) {

              logger.debug("JTree DRAG selection: {}", sourceTarget);

              Target parentTarget = null;

              // Check the parent node to get the science target :
              final DefaultMutableTreeNode parentNode = tree.getParentNode(sourceNode);

              if (parentNode != null) {
                final Object parentUserObject = parentNode.getUserObject();
                if (parentUserObject instanceof Target) {
                  parentTarget = (Target) parentUserObject;
                }
              }

              // only drag calibrators (not science targets) :
              return new TargetTransferable(sourceTarget.getIdentifier(), (parentTarget != null) ? parentTarget.getIdentifier() : null);
            }
          } else {
            logger.warn("unsupported object type: {}", userObject);
          }
        }
      }

    } else {
      logger.warn("unsupported component: {}", sourceComponent);
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
   * @param sourceComponent  the component holding the data to be transferred;
   *           provided to enable sharing of <code>TransferHandler</code>s
   * @return {@code COPY} if the transfer property can be found,
   *          otherwise returns <code>NONE</code>
   */
  @Override
  public int getSourceActions(final JComponent sourceComponent) {
    if (sourceComponent instanceof JList) {
      return COPY;
    } else if (sourceComponent instanceof TargetJTree) {
      return COPY_OR_MOVE;
    }
    return NONE;
  }

  /**
   * Indicates whether a component will accept an import of the given
   * set of data flavors prior to actually attempting to import it.
   *
   * @param destinationComponent  the component to receive the transfer;
   *              provided to enable sharing of <code>TransferHandler</code>s
   * @param transferFlavors  the data formats available
   * @return  true if the data can be inserted into the component, false otherwise
   * @see #canImport(TransferHandler.TransferSupport)
   */
  @Override
  public boolean canImport(final JComponent destinationComponent,
          final DataFlavor[] transferFlavors) {

    if (destinationComponent instanceof TargetJTree) {
      // Check if the transferFlavors are supported :
      if (TargetTransferable.TargetIdentifiersDataFlavor.equals(transferFlavors[0])) {
        // Java 5 does not have an API to handle easily the drop location.
        // It is not possible to have a different behaviour depending on the target type (calibrator or science)
        return true;
      }
    }
    return false;
  }

  /**
   * Causes a transfer to a component from a clipboard or a
   * DND drop operation.  The <code>Transferable</code> represents
   * the data to be imported into the component.
   *
   * @param destinationComponent  the component to receive the transfer;
   *              provided to enable sharing of <code>TransferHandler</code>s
   * @param data     the data to import
   * @return  true if the data was inserted into the component, false otherwise
   * @see #importData(TransferHandler.TransferSupport)
   */
  @Override
  public boolean importData(final JComponent destinationComponent, final Transferable data) {

    /* In case of copy/paste action, we must check that the destination component supports the data flavor */
    if (data != null
            && destinationComponent instanceof TargetJTree
            && canImport(destinationComponent, data.getTransferDataFlavors())) {

      final TargetJTree tree = (TargetJTree) destinationComponent;
      final DefaultMutableTreeNode destinationNode = tree.getLastSelectedNode();

      if (destinationNode != null) {
        /* retrieve the node that was selected */
        final Object userObject = destinationNode.getUserObject();

        if (userObject != null) {
          if (userObject instanceof Target) {
            final Target destinationTarget = (Target) userObject;

            // Check if the target is NOT a calibrator i.e. a science target ?
            if (!isCalibrator(destinationTarget)) {
              logger.debug("JTree DROP selection: {}", destinationTarget);

              // Extract data :
              final TargetTransferable transfer = extractData(data);
              if (transfer != null) {
                final Target srcTarget = getTargetById(transfer.getTargetId());
                if (srcTarget != null) {
                  logger.debug("target (calibrator): {}", srcTarget);

                  return tree.addCalibrator(srcTarget, destinationNode, destinationTarget);
                }
              }
            }
          } else {
            // can occur on root node ('Targets' string)
            logger.debug("unsupported object type: {}", userObject);
          }
        }
      }
    }
    return false;
  }

  /**
   * Invoked after data has been exported.  This method should remove
   * the data that was transferred if the action was <code>MOVE</code>.
   *
   * @param sourceComponent the component that was the source of the data
   * @param data   The data that was transferred or possibly null
   *               if the action is <code>NONE</code>.
   * @param action the actual action that was performed
   */
  @Override
  protected void exportDone(final JComponent sourceComponent, final Transferable data, final int action) {

    if (data != null
            && action == MOVE
            && sourceComponent instanceof TargetJTree) {

      // Extract data :
      final TargetTransferable transfer = extractData(data);
      if (transfer != null) {
        logger.debug("exportDone : Transfered object: {}", transfer);

        final Target srcTarget = getTargetById(transfer.getTargetId());
        if (srcTarget != null) {
          logger.debug("target (calibrator): {}", srcTarget);

          final Target parentTarget = getTargetById(transfer.getParentTargetId());
          if (parentTarget != null) {
            logger.debug("target (science): {}", parentTarget);

            final TargetJTree tree = (TargetJTree) sourceComponent;

            final DefaultMutableTreeNode parentNode = tree.findTreeNode(parentTarget);
            if (parentNode != null) {
              final DefaultMutableTreeNode srcNode = TargetJTree.findTreeNode(parentNode, srcTarget);
              if (srcNode != null) {
                // remove the moved node :
                tree.removeCalibrator(srcNode, srcTarget, parentNode, parentTarget, false);
              }
            }
          }
        }
      }
    }
  }

  /**
   * Extract TargetTransferable from data
   * @param data   The data that was transferred or possibly null
   *               if the action is <code>NONE</code>.
   * @return TargetTransferable or null
   */
  private final TargetTransferable extractData(final Transferable data) {
    TargetTransferable transfer = null;
    try {
      transfer = (TargetTransferable) data.getTransferData(TargetTransferable.TargetIdentifiersDataFlavor);
    } catch (UnsupportedFlavorException ufe) {
      logger.error("unsupported format", ufe);
    } catch (IOException ioe) {
      logger.error("I/O failure", ioe);
    }
    return transfer;
  }

// --- Utility methods -------------------------------------------------------
  /**
   * Return true if the given target is a calibrator
   * i.e. the calibrator list contains the given target
   * @param target target to use
   * @return true if the given target is a calibrator
   */
  private final boolean isCalibrator(final Target target) {
    return this.editTargetUserInfos.isCalibrator(target);
  }

  /**
   * Return the target of the given identifier in the given list of targets
   * @param id target identifier
   * @return target or null if the target was not found
   */
  private final Target getTargetById(final String id) {
    return Target.getTargetById(id, this.editTargets);
  }
}
