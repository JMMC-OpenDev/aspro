
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes one moon restriction rule (FLI, separation ...)
 *       
 * 
 * <p>Java class for MoonRestriction complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="MoonRestriction">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="fli" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="separation" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "MoonRestriction", propOrder = {
    "fli",
    "separation"
})
public class MoonRestriction
    extends OIBase
{

    protected Double fli;
    protected double separation;

    /**
     * Gets the value of the fli property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFli() {
        return fli;
    }

    /**
     * Sets the value of the fli property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFli(Double value) {
        this.fli = value;
    }

    /**
     * Gets the value of the separation property.
     * 
     */
    public double getSeparation() {
        return separation;
    }

    /**
     * Sets the value of the separation property.
     * 
     */
    public void setSeparation(double value) {
        this.separation = value;
    }

}
