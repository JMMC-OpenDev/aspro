
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes a telescope or a telescope family ...
 *       
 * 
 * <p>Java class for Telescope complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Telescope">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}ID"/>
 *         &lt;element name="diameter" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="maxElevation" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="adaptiveOptics" type="{http://www.jmmc.fr/aspro-oi/0.1}AdaptiveOptics" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Telescope", propOrder = {
    "name",
    "diameter",
    "maxElevation",
    "adaptiveOptics"
})
public class Telescope
    extends OIBase
{

    @XmlElement(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String name;
    protected double diameter;
    protected double maxElevation;
    protected AdaptiveOptics adaptiveOptics;

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
     * Gets the value of the diameter property.
     * 
     */
    public double getDiameter() {
        return diameter;
    }

    /**
     * Sets the value of the diameter property.
     * 
     */
    public void setDiameter(double value) {
        this.diameter = value;
    }

    /**
     * Gets the value of the maxElevation property.
     * 
     */
    public double getMaxElevation() {
        return maxElevation;
    }

    /**
     * Sets the value of the maxElevation property.
     * 
     */
    public void setMaxElevation(double value) {
        this.maxElevation = value;
    }

    /**
     * Gets the value of the adaptiveOptics property.
     * 
     * @return
     *     possible object is
     *     {@link AdaptiveOptics }
     *     
     */
    public AdaptiveOptics getAdaptiveOptics() {
        return adaptiveOptics;
    }

    /**
     * Sets the value of the adaptiveOptics property.
     * 
     * @param value
     *     allowed object is
     *     {@link AdaptiveOptics }
     *     
     */
    public void setAdaptiveOptics(AdaptiveOptics value) {
        this.adaptiveOptics = value;
    }

}
