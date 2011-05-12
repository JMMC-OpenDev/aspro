/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: TargetTransferable.java,v 1.1 2010-12-06 17:00:00 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2010/12/03 16:28:48  bourgesl
 * first try to use drag and drop with List and Tree
 *
 * Revision 1.1  2010/12/03 09:34:01  bourgesl
 * first try using drag and drop between calibrator list and target tree
 * added calibrator list coupled with Calibrator button
 * changed font for target tree
 *
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;
import java.io.Serializable;

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
  /** target identifier */
  private final String targetId;
  /** optional target identifier of the parent target */
  private final String parentTargetId;

  /**
   * Public constructor with the given target identifiers to transfer
   * @param targetId target identifier
   */
  public TargetTransferable(final String targetId) {
    this(targetId, null);
  }

  /**
   * Public constructor with the given target identifiers to transfer
   * @param targetId target identifier
   * @param parentTargetId optional target identifier of the parent target
   */
  public TargetTransferable(final String targetId, final String parentTargetId) {
    this.targetId = targetId;
    this.parentTargetId = parentTargetId;
  }

  /**
   * Returns an array of DataFlavor objects indicating the flavors the data
   * can be provided in.  The array should be ordered according to preference
   * for providing the data (from most richly descriptive to least descriptive).
   * @return an array of data flavors in which this data can be transferred
   */
  public DataFlavor[] getTransferDataFlavors() {
    return supportedDataFlavors;
  }

  /**
   * Returns whether or not the specified data flavor is supported for
   * this object.
   * @param flavor the requested flavor for the data
   * @return boolean indicating whether or not the data flavor is supported
   */
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
  public Object getTransferData(final DataFlavor flavor) throws UnsupportedFlavorException, IOException {
    // return this instance as it is serializable (two immutable strings)
    return this;
  }

  // --- Transfer data ---------------------------------------------------------
  /**
   * Return the target identifier
   * @return target identifier
   */
  public String getTargetId() {
    return targetId;
  }

  /**
   * Return the optional target identifier of the parent target
   * @return optional target identifier of the parent target
   */
  public String getParentTargetId() {
    return parentTargetId;
  }

  @Override
  public String toString() {
    return "TargetTransferable { " + this.targetId + " <= " + this.parentTargetId + "}";
  }
}
