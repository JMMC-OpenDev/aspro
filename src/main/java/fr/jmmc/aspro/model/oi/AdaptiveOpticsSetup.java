
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
 *                 This type describes an AO setup
 *             
 * 
 * <p>Java class for AdaptiveOpticsSetup complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="AdaptiveOpticsSetup"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}ID"/&gt;
 *         &lt;element name="numberSubPupils" type="{http://www.w3.org/2001/XMLSchema}int"/&gt;
 *         &lt;element name="numberActuators" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="dit" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="ron" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="quantumEfficiency" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="transmission" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "AdaptiveOpticsSetup", propOrder = {
    "name",
    "numberSubPupils",
    "numberActuators",
    "dit",
    "ron",
    "quantumEfficiency",
    "transmission"
})
public class AdaptiveOpticsSetup
    extends OIBase
{

    @XmlElement(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String name;
    protected int numberSubPupils;
    protected Integer numberActuators;
    protected double dit;
    protected double ron;
    protected double quantumEfficiency;
    protected Double transmission;

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
     * Gets the value of the numberSubPupils property.
     * 
     */
    public int getNumberSubPupils() {
        return numberSubPupils;
    }

    /**
     * Sets the value of the numberSubPupils property.
     * 
     */
    public void setNumberSubPupils(int value) {
        this.numberSubPupils = value;
    }

    /**
     * Gets the value of the numberActuators property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getNumberActuators() {
        return numberActuators;
    }

    /**
     * Sets the value of the numberActuators property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setNumberActuators(Integer value) {
        this.numberActuators = value;
    }

    /**
     * Gets the value of the dit property.
     * 
     */
    public double getDit() {
        return dit;
    }

    /**
     * Sets the value of the dit property.
     * 
     */
    public void setDit(double value) {
        this.dit = value;
    }

    /**
     * Gets the value of the ron property.
     * 
     */
    public double getRon() {
        return ron;
    }

    /**
     * Sets the value of the ron property.
     * 
     */
    public void setRon(double value) {
        this.ron = value;
    }

    /**
     * Gets the value of the quantumEfficiency property.
     * 
     */
    public double getQuantumEfficiency() {
        return quantumEfficiency;
    }

    /**
     * Sets the value of the quantumEfficiency property.
     * 
     */
    public void setQuantumEfficiency(double value) {
        this.quantumEfficiency = value;
    }

    /**
     * Gets the value of the transmission property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getTransmission() {
        return transmission;
    }

    /**
     * Sets the value of the transmission property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setTransmission(Double value) {
        this.transmission = value;
    }
    
//--simple--preserve
    /** parent AdaptiveOptics */
    @javax.xml.bind.annotation.XmlTransient
    private AdaptiveOptics ao = null;

    public AdaptiveOptics getAdaptiveOptics() {
        return ao;
    }

    public void setAdaptiveOptics(final AdaptiveOptics ao) {
        this.ao = ao;
    }

    @Override
    public final String toString() {
        return "AdaptiveOpticsSetup[" + ((this.name != null) ? this.name : "undefined") + "]";
    }
//--simple--preserve

}
