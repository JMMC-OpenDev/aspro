/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.io.Serializable;

/**
 * A transferable implementation for the specific Target and TargetGroup data transfer of some Swing
 * components (JTree).
 *
 * @author bourgesl
 */
public final class TargetGroupTransferable implements Serializable, Transferable {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** custom DataFlavor instance to represent this class */
    public final static DataFlavor TargetGroupIdentifiersDataFlavor = new DataFlavor(TargetGroupTransferable.class, "Target and group identifiers");
    /** supported DataFlavors */
    private final static DataFlavor[] supportedDataFlavors = new DataFlavor[]{TargetGroupIdentifiersDataFlavor};

    /* member */
    /** tree identifier */
    private final String treeId;
    /** target identifier */
    private final String targetId;
    /** flag indicating if the target node is a leaf (no children) */
    private final boolean isLeaf;
    /** optional target group identifier */
    private final String targetGroupId;
    /** optional parent target identifier */
    private final String parentTargetId;

    /**
     * Public constructor with the given target identifiers to transfer
     * @param treeId tree identifier
     * @param targetId target identifier
     * @param isLeaf flag indicating if the target node is a leaf
     * @param targetGroupId target group identifier
     * @param parentTargetId optional parent target identifier
     */
    public TargetGroupTransferable(final String treeId, final String targetId, final boolean isLeaf,
                                   final String targetGroupId, final String parentTargetId) {
        this.treeId = treeId;
        this.targetId = targetId;
        this.isLeaf = isLeaf;
        this.targetGroupId = targetGroupId;
        this.parentTargetId = parentTargetId;
    }

    /**
     * Returns an array of DataFlavor objects indicating the flavors the data
     * can be provided in.  The array should be ordered according to preference
     * for providing the data (from most richly descriptive to least descriptive).
     * @return an array of data flavors in which this data can be transferred
     */
    @Override
    public DataFlavor[] getTransferDataFlavors() {
        return supportedDataFlavors;
    }

    /**
     * Returns whether or not the specified data flavor is supported for
     * this object.
     * @param flavor the requested flavor for the data
     * @return boolean indicating whether or not the data flavor is supported
     */
    @Override
    public boolean isDataFlavorSupported(final DataFlavor flavor) {
        return (flavor.getRepresentationClass() == supportedDataFlavors[0].getRepresentationClass());
    }

    /**
     * Returns an object which represents the data to be transferred.  The class
     * of the object returned is defined by the representation class of the flavor.
     *
     * @param flavor the requested flavor for the data
     * @return an object
     * @see DataFlavor#getRepresentationClass
     * @exception IOException                if the data is no longer available
     *              in the requested flavor.
     * @exception UnsupportedFlavorException if the requested data flavor is
     *              not supported.
     */
    @Override
    public Object getTransferData(final DataFlavor flavor) throws UnsupportedFlavorException, IOException {
        // return this instance as it is serializable (immutable strings)
        return this;
    }

    // --- Transfer data ---------------------------------------------------------
    /**
     * Return the tree identifier
     * @return tree identifier
     */
    public String getTreeId() {
        return treeId;
    }

    /**
     * Return the target identifier
     * @return target identifier
     */
    public String getTargetId() {
        return targetId;
    }

    /**
     * Return flag indicating if the target node is a leaf (no children)
     * @return flag indicating if the target node is a leaf (no children)
     */
    public boolean isLeaf() {
        return isLeaf;
    }

    /**
     * Return the optional target group identifier
     * @return optional target group identifier
     */
    public String getTargetGroupId() {
        return targetGroupId;
    }

    /**
     * Return the optional parent target identifier
     * @return optional parent target identifier
     */
    public String getParentTargetId() {
        return parentTargetId;
    }

    @Override
    public String toString() {
        return "TargetGroupTransferable[" + this.treeId + "] { " + this.targetId
                + "[" + isLeaf + "] (" + this.targetGroupId + ") <= " + this.parentTargetId + "}";
    }
}
