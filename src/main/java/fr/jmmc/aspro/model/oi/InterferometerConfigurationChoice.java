
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
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
 *         &lt;element name="bestPopsMode" type="{http://www.jmmc.fr/aspro-oi/0.1}BestPopsMode" minOccurs="0"/&gt;
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
    "allPops",
    "bestPopsMode"
})
public class InterferometerConfigurationChoice
    extends OIBase
{

    @XmlElement(required = true)
    protected String name;
    protected double minElevation;
    protected String allPops;
    
    protected BestPopsMode bestPopsMode;

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

    /**
     * Gets the value of the bestPopsMode property.
     * 
     * @return
     *     possible object is
     *     {@link BestPopsMode }
     *     
     */
    public BestPopsMode getBestPopsMode() {
        return bestPopsMode;
    }

    /**
     * Sets the value of the bestPopsMode property.
     * 
     * @param value
     *     allowed object is
     *     {@link BestPopsMode }
     *     
     */
    public void setBestPopsMode(BestPopsMode value) {
        this.bestPopsMode = value;
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

    /**
    * @return bestPopsMode or ALL if undefined
    */
    public BestPopsMode getBestPopsModeOrDefault() {
        return (bestPopsMode != null) ? bestPopsMode : BestPopsMode.ALL;
    }
    
    /**
    * @return true if bestPopsMode == BestPopsMode.SELECTED
    */
    public boolean isBestPopsModeSelected() {
        return (bestPopsMode != null) && (bestPopsMode == BestPopsMode.SELECTED);
    }

    @Override
    protected boolean areEquals(final OIBase o) {
        if (!super.areEquals(o)) {
            return false;
        }
        final InterferometerConfigurationChoice other = (InterferometerConfigurationChoice) o;
        return (areEquals(this.name, other.getName())
                && areEquals(this.minElevation, other.getMinElevation())
                && areEquals(this.allPops, other.getAllPops())
                && areEquals(this.getBestPopsModeOrDefault(), other.getBestPopsModeOrDefault()));
    }

//--simple--preserve

}
