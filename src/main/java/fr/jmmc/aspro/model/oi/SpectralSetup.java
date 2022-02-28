
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlList;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the spectral setup to provide several instrumental quantities
 *             
 * 
 * <p>Java class for SpectralSetup complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="SpectralSetup"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="column" type="{http://www.jmmc.fr/aspro-oi/0.1}SpectralSetupColumn" maxOccurs="unbounded"/&gt;
 *         &lt;element name="data" type="{http://www.jmmc.fr/aspro-oi/0.1}ValueList"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SpectralSetup", propOrder = {
    "columns",
    "data"
})
public class SpectralSetup
    extends OIBase
{

    @XmlElement(name = "column", required = true)
    protected List<SpectralSetupColumn> columns;
    @XmlList
    @XmlElement(type = Double.class)
    protected double[] data;

    /**
     * Gets the value of the columns property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the columns property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getColumns().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link SpectralSetupColumn }
     * 
     * 
     */
    public List<SpectralSetupColumn> getColumns() {
        if (columns == null) {
            columns = new ArrayList<SpectralSetupColumn>();
        }
        return this.columns;
    }

    /**
     * 
     * 
     * @return
     *     array of
     *     {@link Double }
     *     
     */
    public double[] getData() {
        if (this.data == null) {
            return new double[ 0 ] ;
        }
        double[] retVal = new double[this.data.length] ;
        System.arraycopy(this.data, 0, retVal, 0, this.data.length);
        return (retVal);
    }

    /**
     * 
     * 
     * @return
     *     one of
     *     {@link Double }
     *     
     */
    public double getData(int idx) {
        if (this.data == null) {
            throw new IndexOutOfBoundsException();
        }
        return this.data[idx];
    }

    public int getDataLength() {
        if (this.data == null) {
            return  0;
        }
        return this.data.length;
    }

    /**
     * 
     * 
     * @param values
     *     allowed objects are
     *     {@link Double }
     *     
     */
    public void setData(double[] values) {
        int len = values.length;
        this.data = ((double[]) new double[len] );
        for (int i = 0; (i<len); i ++) {
            this.data[i] = new Double(values[i]);
        }
    }

    /**
     * 
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public double setData(int idx, double value) {
        return this.data[idx] = new Double(value);
    }
    
//--simple--preserve
    /** number of columns (read-only) */
    @javax.xml.bind.annotation.XmlTransient
    protected int nbCols = 0;

    public int getNbCols() {
        return this.nbCols;
    }

    /** number of rows (read-only) */
    @javax.xml.bind.annotation.XmlTransient
    protected int nbRows = 0;

    public int getNbRows() {
        return this.nbRows;
    }

    @Override
    public String toString() {
        return "SpectralSetup[" + getColumns()
                + ((data != null) ? ("\ndata:\n" + java.util.Arrays.toString(data)) : "")
                + "]";
    }

    /**
     * Initialize and check this instance
     * @param logger logger to use
     * @throws IllegalStateException if the configuration is severly invalid !
     */
    public void init(final org.slf4j.Logger logger) throws IllegalStateException {
        final List<SpectralSetupColumn> cols = getColumns();
        if (cols.isEmpty()) {
            throw new IllegalStateException("Missing columns !");
        }
        // check matrix size is "compatible" with column lengths:
        final int nCols = cols.size();
        final int dataLength = getDataLength();
        if ((dataLength == 0) || (dataLength % nCols != 0)) {
            throw new IllegalStateException("Invalid data length [" + dataLength + "] for " + nCols + " columns !");
        }
        // Split 1D array into columns:
        final int nRows = dataLength / nCols;

        final double[][] matrix = new double[nCols][nRows];
        final double[] data1D = this.data;

        for (int i = 0, offset; i < nRows; i++) {
            offset = i * nCols;
            for (int j = 0; j < nCols; j++) {
                matrix[j][i] = data1D[offset + j];
            }
        }

        for (int j = 0; j < nCols; j++) {
            final SpectralSetupColumn column = cols.get(j);
            if (column.getQuantity() == null) {
                throw new IllegalStateException("Invalid quantity for column[" + j + "]!");
            }
            /*            
            System.out.println("getColumns["+j+"]: "+cols.get(j));
            System.out.println("Values: "+Arrays.toString(matrix[j]));
             */
            // define column values:
            column.values = matrix[j];
        }

        // TODO: check if lambda are sorted ?
        // TODO: KEEP lambda in meters ALWAYS !
        SpectralSetupColumn col;
        col = getColumn(SpectralSetupQuantity.LAMBDA);
        if (col == null) {
            throw new IllegalStateException("Missing column LAMBDA !");
        } else {
            convertMeterToMicroMeter(col.getValues());
        }
        col = getColumn(SpectralSetupQuantity.DELTA_LAMBDA);
        if (col == null) {
            throw new IllegalStateException("Missing column DELTA_LAMBDA !");
        } else {
            convertMeterToMicroMeter(col.getValues());
        }

        // update dimensions:
        this.nbCols = nCols;
        this.nbRows = nRows;

        // prune data array:
        this.data = null;
    }

    private static void convertMeterToMicroMeter(final double[] values) {
        for (int i = 0; i < values.length; i++) {
            values[i] /= fr.jmmc.aspro.AsproConstants.MICRO_METER;
        }
    }

    /**
     * Return the column values of the given quantity scaled by the given scale factor
     * @param quantity quantity to look up
     * @param scaleFactor to scale
     * @return column values or null if the column was not found
     */
    public double[] getAndScaleColumn(final SpectralSetupQuantity quantity, final double scaleFactor) {
        final SpectralSetupColumn col = getColumn(quantity, null);
        if (col != null) {
            final double[] src = col.getValues();
            final double[] dst = new double[src.length];
            for (int i = 0; i < src.length; i++) {
                dst[i] = src[i] * scaleFactor;
            }
            return dst;
        }
        return null;
    }

    /**
     * Return the column of the given quantity
     * @param quantity quantity to look up
     * @return column or null if the column was not found
     */
    public SpectralSetupColumn getColumn(final SpectralSetupQuantity quantity) {
        return getColumn(quantity, null);
    }

    /**
     * Return the column of the given quantity matching the given telescope or without telescope if no match
     * @param quantity quantity to look up
     * @param telescope optional telescope to look up
     * @return column or null if the column was not found
     */
    public SpectralSetupColumn getColumnOrDefault(final SpectralSetupQuantity quantity, final Telescope telescope) {
        SpectralSetupColumn col = getColumn(quantity, telescope);
        if (col == null) {
            col = getColumn(quantity);
        }
        return col;
    }
    
    /**
     * Return the column of the given quantity and optionally matching the given telescope
     * @param quantity quantity to look up
     * @param telescope optional telescope to look up
     * @return column or null if the column was not found
     */
    public SpectralSetupColumn getColumn(final SpectralSetupQuantity quantity, final Telescope telescope) {
        for (SpectralSetupColumn c : getColumns()) {
            if (c.getQuantity() == quantity) {
                if ((telescope != null) && (c.getTelescope() != null)
                        && !c.getTelescope().getName().equals(telescope.getName())) {
                    continue;
                }
                return c;
            }
        }
        return null;
    }

//--simple--preserve

}
