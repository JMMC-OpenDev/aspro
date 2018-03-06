
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes an Adaptive Optics system
 *             
 * 
 * <p>Java class for AdaptiveOptics complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="AdaptiveOptics"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="band" type="{http://www.jmmc.fr/aspro-oi/0.1}SpectralBand"/&gt;
 *         &lt;element name="numberActuators" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="magLimit" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "AdaptiveOptics", propOrder = {
    "band",
    "numberActuators",
    "magLimit"
})
public class AdaptiveOptics
    extends OIBase
{

    @XmlElement(required = true)
    
    protected SpectralBand band;
    protected int numberActuators;
    protected Double magLimit;

    /**
     * Gets the value of the band property.
     * 
     * @return
     *     possible object is
     *     {@link SpectralBand }
     *     
     */
    public SpectralBand getBand() {
        return band;
    }

    /**
     * Sets the value of the band property.
     * 
     * @param value
     *     allowed object is
     *     {@link SpectralBand }
     *     
     */
    public void setBand(SpectralBand value) {
        this.band = value;
    }

    /**
     * Gets the value of the numberActuators property.
     * 
     */
    public int getNumberActuators() {
        return numberActuators;
    }

    /**
     * Sets the value of the numberActuators property.
     * 
     */
    public void setNumberActuators(int value) {
        this.numberActuators = value;
    }

    /**
     * Gets the value of the magLimit property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getMagLimit() {
        return magLimit;
    }

    /**
     * Sets the value of the magLimit property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setMagLimit(Double value) {
        this.magLimit = value;
    }

}
