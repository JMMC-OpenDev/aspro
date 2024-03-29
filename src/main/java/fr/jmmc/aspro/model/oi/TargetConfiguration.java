
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the configuration used during the observation of the target.
 *             
 * 
 * <p>Java class for TargetConfiguration complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="TargetConfiguration"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="HAMin" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="HAMax" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="aoSetup" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="fringeTrackerMode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="instrumentWaveLengthRef" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="aoTarget" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="fringeTrackerTarget" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "TargetConfiguration", propOrder = {
    "haMin",
    "haMax",
    "aoSetup",
    "fringeTrackerMode",
    "instrumentWaveLengthRef",
    "aoTarget",
    "fringeTrackerTarget"
})
public class TargetConfiguration
    extends OIBase
{

    @XmlElement(name = "HAMin")
    protected Double haMin;
    @XmlElement(name = "HAMax")
    protected Double haMax;
    protected String aoSetup;
    protected String fringeTrackerMode;
    protected Double instrumentWaveLengthRef;
    protected String aoTarget;
    protected String fringeTrackerTarget;

    /**
     * Gets the value of the haMin property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getHAMin() {
        return haMin;
    }

    /**
     * Sets the value of the haMin property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setHAMin(Double value) {
        this.haMin = value;
    }

    /**
     * Gets the value of the haMax property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getHAMax() {
        return haMax;
    }

    /**
     * Sets the value of the haMax property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setHAMax(Double value) {
        this.haMax = value;
    }

    /**
     * Gets the value of the aoSetup property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAoSetup() {
        return aoSetup;
    }

    /**
     * Sets the value of the aoSetup property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAoSetup(String value) {
        this.aoSetup = value;
    }

    /**
     * Gets the value of the fringeTrackerMode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFringeTrackerMode() {
        return fringeTrackerMode;
    }

    /**
     * Sets the value of the fringeTrackerMode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFringeTrackerMode(String value) {
        this.fringeTrackerMode = value;
    }

    /**
     * Gets the value of the instrumentWaveLengthRef property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getInstrumentWaveLengthRef() {
        return instrumentWaveLengthRef;
    }

    /**
     * Sets the value of the instrumentWaveLengthRef property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setInstrumentWaveLengthRef(Double value) {
        this.instrumentWaveLengthRef = value;
    }

    /**
     * Gets the value of the aoTarget property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAoTarget() {
        return aoTarget;
    }

    /**
     * Sets the value of the aoTarget property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAoTarget(String value) {
        this.aoTarget = value;
    }

    /**
     * Gets the value of the fringeTrackerTarget property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFringeTrackerTarget() {
        return fringeTrackerTarget;
    }

    /**
     * Sets the value of the fringeTrackerTarget property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFringeTrackerTarget(String value) {
        this.fringeTrackerTarget = value;
    }
    
//--simple--preserve

    @Override
    protected boolean areEquals(final OIBase o) {
        if (!super.areEquals(o)) {
            return false;
        }
        final TargetConfiguration other = (TargetConfiguration)o;
        return (areEquals(this.haMin, other.getHAMin())
                && areEquals(this.haMax, other.getHAMax())
                && areEquals(this.aoSetup, other.getAoSetup())
                && areEquals(this.fringeTrackerMode, other.getFringeTrackerMode())
                && areEquals(this.instrumentWaveLengthRef, other.getInstrumentWaveLengthRef()))
                && areEquals(this.aoTarget, other.getAoTarget())
                && areEquals(this.fringeTrackerTarget, other.getFringeTrackerTarget());
    }
    
//--simple--preserve

}
