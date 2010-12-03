/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: GenericTransferable.java,v 1.1 2010-12-03 09:34:01 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.gui.util;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.IOException;

/**
 * A transferable implementation for the specific Target data transfer of some Swing
 * components (JList / JTree).
 *
 * @author bourgesl
 */
public final class GenericTransferable implements Transferable {

  /* member */
  /** the data this transferable transports */
  private Object data;
  /** supported DataFlavor objects */
  private final DataFlavor[] supportedDataFlavors;

  /**
   * Public constructor with the given data object to transfer
   * @param object data object
   */
  public GenericTransferable(final Object object) {
    this.data = object;
    this.supportedDataFlavors = new DataFlavor[]{
              new DataFlavor(data.getClass(), "application/x-java-serialized-object")
            };
  }

  /**
   * Returns an array of DataFlavor objects indicating the flavors the data
   * can be provided in.  The array should be ordered according to preference
   * for providing the data (from most richly descriptive to least descriptive).
   * @return an array of data flavors in which this data can be transferred
   */
  public DataFlavor[] getTransferDataFlavors() {
    return this.supportedDataFlavors;
  }

  /**
   * Returns whether or not the specified data flavor is supported for
   * this object.
   * @param flavor the requested flavor for the data
   * @return boolean indicating whether or not the data flavor is supported
   */
  public boolean isDataFlavorSupported(final DataFlavor flavor) {
    return (flavor.getRepresentationClass() == this.supportedDataFlavors[0].getRepresentationClass());
  }

  /**
   * Returns an object which represents the data to be transferred.  The class
   * of the object returned is defined by the representation class of the flavor.
   *
   * @param flavor the requested flavor for the data
   * @see DataFlavor#getRepresentationClass
   * @exception IOException                if the data is no longer available
   *              in the requested flavor.
   * @exception UnsupportedFlavorException if the requested data flavor is
   *              not supported.
   */
  public Object getTransferData(final DataFlavor flavor) throws UnsupportedFlavorException, IOException {
    return this.data;
  }
}
