
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes a focal instrument mode (AMBER : "Low_HK" ... "Medium_K_1_2300" ... "High_K_1_2481")
 *       
 * 
 * <p>Java class for FocalInstrumentMode complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FocalInstrumentMode">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="waveLengthMin" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="waveLengthMax" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FocalInstrumentMode", propOrder = {
    "name",
    "waveLengthMin",
    "waveLengthMax"
})
public class FocalInstrumentMode
    extends OIBase
{

    @XmlElement(required = true)
    protected String name;
    protected double waveLengthMin;
    protected double waveLengthMax;

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
     * Gets the value of the waveLengthMin property.
     * 
     */
    public double getWaveLengthMin() {
        return waveLengthMin;
    }

    /**
     * Sets the value of the waveLengthMin property.
     * 
     */
    public void setWaveLengthMin(double value) {
        this.waveLengthMin = value;
    }

    /**
     * Gets the value of the waveLengthMax property.
     * 
     */
    public double getWaveLengthMax() {
        return waveLengthMax;
    }

    /**
     * Sets the value of the waveLengthMax property.
     * 
     */
    public void setWaveLengthMax(double value) {
        this.waveLengthMax = value;
    }

}
