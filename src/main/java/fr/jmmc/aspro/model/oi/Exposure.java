
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes an exposure (SCIENCE | SKY | DEAD_TIME)
 *                 on interferometric and/or photometric channels
 *             
 * 
 * <p>Java class for Exposure complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Exposure">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;attribute name="type" use="required" type="{http://www.jmmc.fr/aspro-oi/0.1}ExposureType" />
 *       &lt;attribute name="mode" type="{http://www.jmmc.fr/aspro-oi/0.1}ExposureMode" default="ALL" />
 *       &lt;attribute name="unit" type="{http://www.w3.org/2001/XMLSchema}double" default="1" />
 *       &lt;attribute name="beams" type="{http://www.w3.org/2001/XMLSchema}int" />
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Exposure")
public class Exposure
    extends OIBase
{

    @XmlAttribute(name = "type", required = true)
    protected ExposureType type;
    @XmlAttribute(name = "mode")
    protected ExposureMode mode;
    @XmlAttribute(name = "unit")
    protected Double unit;
    @XmlAttribute(name = "beams")
    protected Integer beams;

    /**
     * Gets the value of the type property.
     * 
     * @return
     *     possible object is
     *     {@link ExposureType }
     *     
     */
    public ExposureType getType() {
        return type;
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value
     *     allowed object is
     *     {@link ExposureType }
     *     
     */
    public void setType(ExposureType value) {
        this.type = value;
    }

    /**
     * Gets the value of the mode property.
     * 
     * @return
     *     possible object is
     *     {@link ExposureMode }
     *     
     */
    public ExposureMode getMode() {
        if (mode == null) {
            return ExposureMode.ALL;
        } else {
            return mode;
        }
    }

    /**
     * Sets the value of the mode property.
     * 
     * @param value
     *     allowed object is
     *     {@link ExposureMode }
     *     
     */
    public void setMode(ExposureMode value) {
        this.mode = value;
    }

    /**
     * Gets the value of the unit property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public double getUnit() {
        if (unit == null) {
            return  1.0D;
        } else {
            return unit;
        }
    }

    /**
     * Sets the value of the unit property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setUnit(Double value) {
        this.unit = value;
    }

    /**
     * Gets the value of the beams property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getBeams() {
        return beams;
    }

    /**
     * Sets the value of the beams property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setBeams(Integer value) {
        this.beams = value;
    }
    
//--simple--preserve
    @Override
    public String toString() {
        return "Exposure[type=" + getType() + " mode=" + getMode()
                + " unit=" + getUnit() + " beams=" + getBeams() + "]";
    }
//--simple--preserve

}
