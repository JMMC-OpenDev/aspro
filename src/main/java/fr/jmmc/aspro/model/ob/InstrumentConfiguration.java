
package fr.jmmc.aspro.model.ob;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the chosen instrument and its mode
 *             
 * 
 * <p>Java class for InstrumentConfiguration complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="InstrumentConfiguration"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="instrumentMode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="dit" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="acquisitionTime" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="instrumentWaveLengthRef" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="instrumentWaveBandRef" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="fringeTrackerMode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "InstrumentConfiguration", propOrder = {
    "name",
    "instrumentMode",
    "dit",
    "acquisitionTime",
    "instrumentWaveLengthRef",
    "instrumentWaveBandRef",
    "fringeTrackerMode"
})
public class InstrumentConfiguration
    extends OIBase
{

    @XmlElement(required = true)
    protected String name;
    protected String instrumentMode;
    protected Double dit;
    protected Double acquisitionTime;
    protected Double instrumentWaveLengthRef;
    protected Double instrumentWaveBandRef;
    protected String fringeTrackerMode;

    /**
     * Gets the value of the name property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the value of the name property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setName(String value) {
        this.name = value;
    }

    /**
     * Gets the value of the instrumentMode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInstrumentMode() {
        return instrumentMode;
    }

    /**
     * Sets the value of the instrumentMode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInstrumentMode(String value) {
        this.instrumentMode = value;
    }

    /**
     * Gets the value of the dit property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getDit() {
        return dit;
    }

    /**
     * Sets the value of the dit property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setDit(Double value) {
        this.dit = value;
    }

    /**
     * Gets the value of the acquisitionTime property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getAcquisitionTime() {
        return acquisitionTime;
    }

    /**
     * Sets the value of the acquisitionTime property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setAcquisitionTime(Double value) {
        this.acquisitionTime = value;
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
     * Gets the value of the instrumentWaveBandRef property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getInstrumentWaveBandRef() {
        return instrumentWaveBandRef;
    }

    /**
     * Sets the value of the instrumentWaveBandRef property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setInstrumentWaveBandRef(Double value) {
        this.instrumentWaveBandRef = value;
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

}
