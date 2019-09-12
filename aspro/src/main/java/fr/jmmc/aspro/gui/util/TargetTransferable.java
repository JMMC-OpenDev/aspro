/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * A transferable implementation for the specific Target data transfer of some Swing
 * components (JList / JTree).
 *
 * @author bourgesl
 */
public final class TargetTransferable implements Serializable, Transferable {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** custom DataFlavor instance to represent this class */
    public final static DataFlavor TargetIdentifiersDataFlavor = new DataFlavor(TargetTransferable.class, "Target identifiers");
    /** supported DataFlavors */
    private final static DataFlavor[] supportedDataFlavors = new DataFlavor[]{TargetIdentifiersDataFlavor};

    /* member */
    /** target identifier(s) */
    private final List<String> targetIds;
    /** optional parent target identifier */
    private final String parentTargetId;

    private static List<String> asList(final String targetId) {
        final List<String> targetIds = new ArrayList<String>(1);
        targetIds.add(targetId);
        return targetIds;
    }

    /**
     * Public constructor with the given target identifier to transfer
     * @param targetIds target identifier(s)
     */
    public TargetTransferable(final List<String> targetIds) {
        this(targetIds, null);
    }

    /**
     * Public constructor with the given target identifiers to transfer
     * @param targetId target identifier
     * @param parentTargetId optional parent target identifier
     */
    public TargetTransferable(final String targetId, final String parentTargetId) {
        this.targetIds = asList(targetId);
        this.parentTargetId = parentTargetId;
    }

    /**
     * Private constructor with the given target identifiers to transfer
     * @param targetIds target identifier(s)
     * @param parentTargetId optional parent identifier
     */
    private TargetTransferable(final List<String> targetIds, final String parentTargetId) {
        this.targetIds = targetIds;
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
     * Return the target identifier(s)
     * @return target identifier(s)
     */
    public List<String> getTargetIds() {
        return targetIds;
    }

    /**
     * Return only the first target identifier
     * @return first target identifier
     */
    public String getSingleTargetId() {
        return targetIds.get(0);
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
        return "TargetTransferable { " + this.targetIds + " <= " + this.parentTargetId + "}";
    }
}
