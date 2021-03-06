
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes a moon restriction rule (FLI, separation ...)
 *             
 * 
 * <p>Java class for MoonRestriction complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="MoonRestriction"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="fli" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="flux" type="{http://www.jmmc.fr/aspro-oi/0.1}FluxCondition" minOccurs="0"/&gt;
 *         &lt;element name="separation" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "MoonRestriction", propOrder = {
    "fli",
    "flux",
    "separation"
})
public class MoonRestriction
    extends OIBase
{

    protected Double fli;
    protected FluxCondition flux;
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
     * Gets the value of the flux property.
     * 
     * @return
     *     possible object is
     *     {@link FluxCondition }
     *     
     */
    public FluxCondition getFlux() {
        return flux;
    }

    /**
     * Sets the value of the flux property.
     * 
     * @param value
     *     allowed object is
     *     {@link FluxCondition }
     *     
     */
    public void setFlux(FluxCondition value) {
        this.flux = value;
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
    
//--simple--preserve

    @Override
    public String toString() {
        return "MoonRestriction{" + "fli=" + fli + ", flux=" + flux + ", separation=" + separation + '}';
    }
    
//--simple--preserve

}
