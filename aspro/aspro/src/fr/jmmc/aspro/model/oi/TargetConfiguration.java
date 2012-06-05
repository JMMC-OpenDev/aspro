
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes the configuration used during the observation of the target.
 *       
 * 
 * <p>Java class for TargetConfiguration complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="TargetConfiguration">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="HAMin" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="HAMax" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="fringeTrackerMode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "TargetConfiguration", propOrder = {
    "haMin",
    "haMax",
    "fringeTrackerMode"
})
public class TargetConfiguration
    extends OIBase
{

    @XmlElement(name = "HAMin")
    protected Double haMin;
    @XmlElement(name = "HAMax")
    protected Double haMax;
    protected String fringeTrackerMode;

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
