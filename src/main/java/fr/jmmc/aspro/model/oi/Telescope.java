
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
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
 *                 This type describes a telescope or a telescope family ...
 *             
 * 
 * <p>Java class for Telescope complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Telescope"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}ID"/&gt;
 *         &lt;element name="diameter" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="maxElevation" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="adaptiveOptics" type="{http://www.jmmc.fr/aspro-oi/0.1}AdaptiveOptics" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="moonPointingRestriction" type="{http://www.jmmc.fr/aspro-oi/0.1}MoonPointingRestriction" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Telescope", propOrder = {
    "name",
    "diameter",
    "maxElevation",
    "adaptiveOptics",
    "moonPointingRestriction"
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
    protected List<AdaptiveOptics> adaptiveOptics;
    protected MoonPointingRestriction moonPointingRestriction;

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
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the adaptiveOptics property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getAdaptiveOptics().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link AdaptiveOptics }
     * 
     * 
     */
    public List<AdaptiveOptics> getAdaptiveOptics() {
        if (adaptiveOptics == null) {
            adaptiveOptics = new ArrayList<AdaptiveOptics>();
        }
        return this.adaptiveOptics;
    }

    /**
     * Gets the value of the moonPointingRestriction property.
     * 
     * @return
     *     possible object is
     *     {@link MoonPointingRestriction }
     *     
     */
    public MoonPointingRestriction getMoonPointingRestriction() {
        return moonPointingRestriction;
    }

    /**
     * Sets the value of the moonPointingRestriction property.
     * 
     * @param value
     *     allowed object is
     *     {@link MoonPointingRestriction }
     *     
     */
    public void setMoonPointingRestriction(MoonPointingRestriction value) {
        this.moonPointingRestriction = value;
    }
    
//--simple--preserve
    @Override
    public String toString() {
        return "Telescope[" + ((this.name != null) ? this.name : "undefined") + "]";
    }
//--simple--preserve

}
