
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the spatial filter of the focal instrument, typically a telescope aperture + fiber injection
 *                 whose airy radius FWHM ~ 1.023 lambda / diameter
 *             
 * 
 * <p>Java class for SpatialFilter complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="SpatialFilter"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="scalingFactor" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="waveLengthRef" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SpatialFilter", propOrder = {
    "scalingFactor",
    "waveLengthRef"
})
public class SpatialFilter
    extends OIBase
{

    protected Double scalingFactor;
    protected Double waveLengthRef;

    /**
     * Gets the value of the scalingFactor property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getScalingFactor() {
        return scalingFactor;
    }

    /**
     * Sets the value of the scalingFactor property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setScalingFactor(Double value) {
        this.scalingFactor = value;
    }

    /**
     * Gets the value of the waveLengthRef property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getWaveLengthRef() {
        return waveLengthRef;
    }

    /**
     * Sets the value of the waveLengthRef property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setWaveLengthRef(Double value) {
        this.waveLengthRef = value;
    }
    
//--simple--preserve
    public double getScalingFactorOrNaN() {
        return (scalingFactor != null) ? scalingFactor.doubleValue() : Double.NaN;
    }

    public double getWaveLengthRefOrNaN() {
        return (waveLengthRef != null) ? waveLengthRef.doubleValue() : Double.NaN;
    }

    @Override
    public String toString() {
        return "SpatialFilter{" + "scalingFactor=" + scalingFactor + ", waveLengthRef=" + waveLengthRef + '}';
    }

//--simple--preserve

}
