
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the column meta data
 *             
 * 
 * <p>Java class for SpectralSetupColumn complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="SpectralSetupColumn"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;attribute name="quantity" use="required" type="{http://www.jmmc.fr/aspro-oi/0.1}SpectralSetupQuantity" /&gt;
 *       &lt;attribute name="telescope" type="{http://www.w3.org/2001/XMLSchema}IDREF" /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SpectralSetupColumn")
public class SpectralSetupColumn
    extends OIBase
{

    @XmlAttribute(name = "quantity", required = true)
    protected SpectralSetupQuantity quantity;
    @XmlAttribute(name = "telescope")
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected Telescope telescope;

    /**
     * Gets the value of the quantity property.
     * 
     * @return
     *     possible object is
     *     {@link SpectralSetupQuantity }
     *     
     */
    public SpectralSetupQuantity getQuantity() {
        return quantity;
    }

    /**
     * Sets the value of the quantity property.
     * 
     * @param value
     *     allowed object is
     *     {@link SpectralSetupQuantity }
     *     
     */
    public void setQuantity(SpectralSetupQuantity value) {
        this.quantity = value;
    }

    /**
     * Gets the value of the telescope property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public Telescope getTelescope() {
        return telescope;
    }

    /**
     * Sets the value of the telescope property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setTelescope(Telescope value) {
        this.telescope = value;
    }
    
//--simple--preserve

    /** extracted data values (read-only) */
    @javax.xml.bind.annotation.XmlTransient
    protected double[] values = null;
    
    public double[] getValues() {
        return this.values;
    }
    
    @Override
    public String toString() {
        return "SpectralSetupColumn[quantity=" + getQuantity() + " telescope=" + getTelescope() 
                + ((values != null) ? (" values: " + java.util.Arrays.toString(values)) : "")
                + "]";
    }
//--simple--preserve

}
