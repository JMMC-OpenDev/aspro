
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the chosen interferometer and its parameters
 *             
 * 
 * <p>Java class for InterferometerConfigurationChoice complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="InterferometerConfigurationChoice"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="minElevation" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="allPops" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "InterferometerConfigurationChoice", propOrder = {
    "name",
    "minElevation",
    "allPops"
})
public class InterferometerConfigurationChoice
    extends OIBase
{

    @XmlElement(required = true)
    protected String name;
    protected double minElevation;
    protected String allPops;

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
     * Gets the value of the minElevation property.
     * 
     */
    public double getMinElevation() {
        return minElevation;
    }

    /**
     * Sets the value of the minElevation property.
     * 
     */
    public void setMinElevation(double value) {
        this.minElevation = value;
    }

    /**
     * Gets the value of the allPops property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAllPops() {
        return allPops;
    }

    /**
     * Sets the value of the allPops property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAllPops(String value) {
        this.allPops = value;
    }
    
//--simple--preserve
    /** resolved reference to the interferometer configuration (read only) */
    @javax.xml.bind.annotation.XmlTransient
    private InterferometerConfiguration interferometerConfiguration = null;

    /**
     * Return the reference to the interferometer configuration (read only)
     * @return interferometer configuration or null
     */
    public final InterferometerConfiguration getInterferometerConfiguration() {
        return interferometerConfiguration;
    }

    /**
     * Define the reference to the interferometer configuration (read only)
     * @param interferometerConfiguration interferometer configuration
     */
    public final void setInterferometerConfiguration(final InterferometerConfiguration interferometerConfiguration) {
        this.interferometerConfiguration = interferometerConfiguration;
    }

    /** resolved reference to the list of PoPs (read only) */
    @javax.xml.bind.annotation.XmlTransient
    private java.util.Map<String, Pop> popsFixed = null;

    /**
     * Return the reference to the list of PoPs (read only)
     * @return list of PoPs or null
     */
    public final java.util.Map<String, Pop> getFixedPops() {
        return popsFixed;
    }

    /**
     * Define the reference to the list of PoPs (read only)
     * @param popMapping list of PoPs
     */
    public final void setFixedPops(final java.util.Map<String, Pop> popMapping) {
        this.popsFixed = popMapping;
    }

    @Override
    protected boolean areEquals(final OIBase o) {
        if (!super.areEquals(o)) {
            return false;
        }
        final InterferometerConfigurationChoice other = (InterferometerConfigurationChoice) o;
        return (areEquals(this.name, other.getName())
                && areEquals(this.minElevation, other.getMinElevation())
                && areEquals(this.allPops, other.getAllPops()));
    }

//--simple--preserve

}
