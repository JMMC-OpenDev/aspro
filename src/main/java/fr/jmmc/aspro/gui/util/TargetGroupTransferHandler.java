/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.gui.TargetGroupForm;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetGroup;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.jmcs.gui.component.StatusBar;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import javax.swing.JComponent;
import javax.swing.TransferHandler;
import javax.swing.tree.DefaultMutableTreeNode;

/**
 * This custom transfer handler manages the Drag and Drop of Target & TargetGroup objects between JTree instances.
 *
 * JTree.dropMode = 'USE_SELECTION' to be compatible with Java 1.5
 *
 * @author bourgesl
 */
public final class TargetGroupTransferHandler extends TransferHandler {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(TargetGroupTransferHandler.class.getName());

    /* members */
    /** list of edited targets (clone) */
    private final List<Target> editTargets;
    /** edited target user informations (clone) */
    private final TargetUserInformations editTargetUserInfos;
    /** related form */
    private final TargetGroupForm form;

    /**
     * Public constructor
     * @param targets list of targets
     * @param targetUserInfos target user informations
     * @param form form to refresh after tree operations
     */
    public TargetGroupTransferHandler(final List<Target> targets, final TargetUserInformations targetUserInfos,
                                      final TargetGroupForm form) {
        super();

        this.editTargets = targets;
        this.editTargetUserInfos = targetUserInfos;
        this.form = form;
    }

    private void refreshForm(final Target selected) {
        form.refresh(selected);
    }

    private void refreshForm(final Target selected, final TargetGroup group, final Target childTarget) {
        form.refresh(selected, group, childTarget);
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
        if (sourceComponent instanceof TargetGroupJTree) {
            final TargetGroupJTree tree = (TargetGroupJTree) sourceComponent;
            final DefaultMutableTreeNode sourceNode = tree.getLastSelectedNode();

            // Only drag targets (possibly within parent group):
            /* retrieve the node that was selected */
            if (sourceNode != null && sourceNode.getUserObject() instanceof Target) {
                final Target sourceTarget = (Target) sourceNode.getUserObject();

                logger.debug("JTree DRAG selection: {}", sourceTarget);

                TargetGroup parentGroup = null;
                Target parentTarget = null;

                // Check the parent node to get the science target :
                DefaultMutableTreeNode parentNode = tree.getParentNode(sourceNode);

                if (parentNode != null && parentNode.getUserObject() instanceof TargetGroup) {
                    parentGroup = (TargetGroup) parentNode.getUserObject();

                    parentNode = tree.getParentNode(parentNode);

                    if (parentNode != null && parentNode.getUserObject() instanceof Target) {
                        parentTarget = (Target) parentNode.getUserObject();
                    }
                }

                // only drag targets:
                return new TargetGroupTransferable(
                        tree.getName(),
                        sourceTarget.getIdentifier(),
                        sourceNode.isLeaf(),
                        (parentGroup != null) ? parentGroup.getIdentifier() : null,
                        (parentTarget != null) ? parentTarget.getIdentifier() : null
                );
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
        if (sourceComponent instanceof TargetGroupJTree) {
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

        if (destinationComponent instanceof TargetGroupJTree) {
            // Check if the transferFlavors are supported :
            if (TargetGroupTransferable.TargetGroupIdentifiersDataFlavor.equals(transferFlavors[0])) {
                // Java 5 does not have an API to handle easily the drop location.
                // It is not possible to have a different behaviour depending on the target type (target or group)
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
        boolean result = false;

        /* In case of copy/paste action, we must check that the destination component supports the data flavor */
        if (data != null
                && destinationComponent instanceof TargetGroupJTree
                && canImport(destinationComponent, data.getTransferDataFlavors())) {

            final TargetGroupJTree tree = (TargetGroupJTree) destinationComponent;
            final DefaultMutableTreeNode destinationNode = tree.getLastSelectedNode();

            /* retrieve the node that was selected */
            if (destinationNode != null) {
                final Object userObject = destinationNode.getUserObject();

                if (userObject != null) {
                    if (userObject instanceof Target) {
                        final Target destinationTarget = (Target) userObject;

                        logger.debug("JTree DROP selection: {}", destinationTarget);

                        // Extract data :
                        final TargetGroupTransferable transfer = extractData(data);

                        if (transfer != null && hasGroup(transfer)) {
                            logger.debug("transfer: {}", transfer);

                            final Target srcTarget = getTargetById(transfer.getTargetId());
                            if (srcTarget != null) {
                                logger.debug("target: {}", srcTarget);
                                logger.debug("group id: {}", transfer.getTargetGroupId());

                                if (isTreeTargets(tree)) {
                                    // add target association:
                                    final TargetGroup parentGroup = getGroupById(transfer.getTargetGroupId());

                                    // check parentGroup is OB
                                    if (parentGroup.isCategoryOB()) {
                                        logger.debug("group: {}", parentGroup);

                                        // use the parent node to check destination is not an OB target:
                                        DefaultMutableTreeNode parentNode = tree.getParentNode(destinationNode);

                                        if (parentNode == tree.getRootNode()) {
                                            result = this.editTargetUserInfos.getOrCreateTargetInformation(destinationTarget).addTargetInGroupMembers(parentGroup, srcTarget);

                                            if (result) {
                                                // memorize values for later selection:
                                                selectedSrcTarget = destinationTarget;
                                                selectedGroup = parentGroup;
                                                selectedChildTarget = srcTarget;
                                            }
                                        } else {
                                            StatusBar.show("Invalid DnD: target [" + destinationTarget.getName() + "] is not a science target (SCI / CAL)");
                                        }
                                    } else {
                                        StatusBar.show("Invalid DnD: group [" + parentGroup.getName() + "] do not support associations (only "
                                                + TargetGroup.CATEGORY_OB + ')');
                                    }
                                }
                            }
                            // Check if action happens on the same tree:
                            if (result && !tree.getName().equals(transfer.getTreeId())) {
                                result = false; // avoid move between trees
                            }
                        }
                    } else if (userObject instanceof TargetGroup) {
                        final TargetGroup destinationGroup = (TargetGroup) userObject;

                        logger.debug("JTree DROP selection: {}", destinationGroup);

                        // Extract data :
                        final TargetGroupTransferable transfer = extractData(data);

                        if (transfer != null) {
                            logger.debug("transfer: {}", transfer);

                            final Target srcTarget = getTargetById(transfer.getTargetId());
                            if (srcTarget != null) {
                                logger.debug("target: {}", srcTarget);

                                if (isTreeGroups(tree)) {
                                    boolean valid = !destinationGroup.isCategoryOB();
                                    if (!valid) {
                                        // OB group only: check target is a SCIENCE target (has no child OB targets):
                                        if (transfer.isLeaf()) {
                                            valid = true;
                                        } else {
                                            StatusBar.show("Invalid DnD: target [" + srcTarget.getName() + "] has already associations");
                                        }
                                    }
                                    if (valid) {
                                        if (!this.editTargetUserInfos.hasTargetInTargetGroup(destinationGroup, srcTarget)) {
                                            // add target into selected group:
                                            result = this.editTargetUserInfos.addTargetToTargetGroup(destinationGroup, srcTarget);

                                            if (result) {
                                                // memorize values for later selection:
                                                selectedSrcTarget = srcTarget;
                                                selectedGroup = destinationGroup;
                                                selectedChildTarget = null;
                                            }
                                        }
                                    }
                                }
                            }
                            // Check if action happens on the same tree:
                            if (result && !tree.getName().equals(transfer.getTreeId())) {
                                result = false; // avoid move between trees
                            }
                        }
                    } else {
                        // can occur on root node ('Targets' string)
                        logger.debug("unsupported object type: {}", userObject);
                    }
                }
            }
        }
        return result;
    }

    private transient Target selectedSrcTarget = null;
    private transient TargetGroup selectedGroup = null;
    private transient Target selectedChildTarget = null;

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
                && sourceComponent instanceof TargetGroupJTree) {

            if (action == MOVE) {
                // Extract data :
                final TargetGroupTransferable transfer = extractData(data);

                if (transfer != null && hasGroup(transfer)) {
                    logger.debug("exportDone : Transfered object: {}", transfer);

                    final TargetGroupJTree tree = (TargetGroupJTree) sourceComponent;

                    // Check move only happens within the same tree:
                    if (tree.getName().equals(transfer.getTreeId())) {
                        final Target target = getTargetById(transfer.getTargetId());
                        if (target != null) {
                            logger.debug("target: {}", target);
                            logger.debug("group id: {}", transfer.getTargetGroupId());

                            final TargetGroup parentGroup = getGroupById(transfer.getTargetGroupId());
                            if (parentGroup != null) {
                                logger.debug("group: {}", parentGroup);

                                if (isTreeTargets(tree)) {
                                    // moved target association:
                                    final Target parentTarget = getTargetById(transfer.getParentTargetId());
                                    if (parentTarget != null) {
                                        logger.debug("parentTarget: {}", parentTarget);

                                        // remove target association:
                                        this.editTargetUserInfos.getOrCreateTargetInformation(parentTarget).removeTargetInGroupMembers(parentGroup, target);
                                    }
                                } else if (isTreeGroups(tree)) {
                                    // moved target between groups:
                                    this.editTargetUserInfos.removeTargetFromTargetGroup(parentGroup, target);
                                }
                            }
                        }
                    }
                }
            }
            // or (action == COPY) :

            if (selectedSrcTarget != null) {
                logger.debug("selectedSrcTarget:   {}", selectedSrcTarget);
                logger.debug("selectedGroup:       {}", selectedGroup);
                logger.debug("selectedChildTarget: {}", selectedChildTarget);

                // anyway:
                refreshForm(selectedSrcTarget, selectedGroup, selectedChildTarget);
            } else {
                // anyway:
                refreshForm(null);
            }
        }
    }

    /**
     * Extract TargetGroupTransferable from data
     * @param data   The data that was transferred or possibly null
     *               if the action is <code>NONE</code>.
     * @return TargetTransferable or null
     */
    private TargetGroupTransferable extractData(final Transferable data) {
        TargetGroupTransferable transfer = null;
        try {
            transfer = (TargetGroupTransferable) data.getTransferData(TargetGroupTransferable.TargetGroupIdentifiersDataFlavor);
        } catch (UnsupportedFlavorException ufe) {
            logger.error("unsupported format", ufe);
        } catch (IOException ioe) {
            logger.error("I/O failure", ioe);
        }
        return transfer;
    }

// --- Utility methods -------------------------------------------------------
    /**
     * Return the target of the given identifier in the given list of targets
     * @param id target identifier
     * @return target or null if the target was not found
     */
    private Target getTargetById(final String id) {
        return Target.getTargetById(id, this.editTargets);
    }

    private TargetGroup getGroupById(final String id) {
        return TargetGroup.getGroupById(id, this.editTargetUserInfos.getGroups());
    }

    private boolean isTreeTargets(final TargetGroupJTree tree) {
        return TargetGroupForm.TREE_TARGETS.equals(tree.getName());
    }

    private boolean isTreeGroups(final TargetGroupJTree tree) {
        return TargetGroupForm.TREE_GROUPS.equals(tree.getName());
    }

    private boolean hasGroup(final TargetGroupTransferable transfer) {
        return transfer.getTargetGroupId() != null;
    }
}
