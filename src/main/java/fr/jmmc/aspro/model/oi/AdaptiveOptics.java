
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
 *                 This type describes an Adaptive Optics system
 *             
 * 
 * <p>Java class for AdaptiveOptics complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="AdaptiveOptics"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}ID"/&gt;
 *         &lt;element name="band" type="{http://www.jmmc.fr/aspro-oi/0.1}SpectralBand"/&gt;
 *         &lt;element name="instrumentBand" type="{http://www.jmmc.fr/aspro-oi/0.1}SpectralBand" minOccurs="0"/&gt;
 *         &lt;element name="setup" type="{http://www.jmmc.fr/aspro-oi/0.1}AdaptiveOpticsSetup" maxOccurs="unbounded"/&gt;
 *         &lt;element name="magLimit" type="{http://www.jmmc.fr/aspro-oi/0.1}AtmTelValue" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "AdaptiveOptics", propOrder = {
    "name",
    "band",
    "instrumentBand",
    "setups",
    "magLimits"
})
public class AdaptiveOptics
    extends OIBase
{

    @XmlElement(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String name;
    @XmlElement(required = true)
    
    protected SpectralBand band;
    
    protected SpectralBand instrumentBand;
    @XmlElement(name = "setup", required = true)
    protected List<AdaptiveOpticsSetup> setups;
    @XmlElement(name = "magLimit")
    protected List<AtmTelValue> magLimits;

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
     * Gets the value of the band property.
     * 
     * @return
     *     possible object is
     *     {@link SpectralBand }
     *     
     */
    public SpectralBand getBand() {
        return band;
    }

    /**
     * Sets the value of the band property.
     * 
     * @param value
     *     allowed object is
     *     {@link SpectralBand }
     *     
     */
    public void setBand(SpectralBand value) {
        this.band = value;
    }

    /**
     * Gets the value of the instrumentBand property.
     * 
     * @return
     *     possible object is
     *     {@link SpectralBand }
     *     
     */
    public SpectralBand getInstrumentBand() {
        return instrumentBand;
    }

    /**
     * Sets the value of the instrumentBand property.
     * 
     * @param value
     *     allowed object is
     *     {@link SpectralBand }
     *     
     */
    public void setInstrumentBand(SpectralBand value) {
        this.instrumentBand = value;
    }

    /**
     * Gets the value of the setups property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the setups property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getSetups().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link AdaptiveOpticsSetup }
     * 
     * 
     */
    public List<AdaptiveOpticsSetup> getSetups() {
        if (setups == null) {
            setups = new ArrayList<AdaptiveOpticsSetup>();
        }
        return this.setups;
    }

    /**
     * Gets the value of the magLimits property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the magLimits property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getMagLimits().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link AtmTelValue }
     * 
     * 
     */
    public List<AtmTelValue> getMagLimits() {
        if (magLimits == null) {
            magLimits = new ArrayList<AtmTelValue>();
        }
        return this.magLimits;
    }
    
//--simple--preserve
    
    /**
    * @param atmQual optional atmosphere quality
    * @return magnitude limit associated to the given atmosphere quality and telescope (or default)
    */
    public double getMagLimit(final AtmosphereQuality atmQual) {
        for (AtmTelValue val : getMagLimits()) {
            if (val.match(atmQual, null)) {
                return val.getValue(); // first, so take care of ordering values
            }
        }
        return Double.NaN;
    }    
    
    @Override
    public String toString() {
        return "AdaptiveOptics[" + ((this.name != null) ? this.name : "undefined") 
                + " band: " + band
                + ((this.instrumentBand != null) ? " instrumentBand: " + instrumentBand : "")
                + " magLimits: " + getMagLimits()
                + " setups: " + setups
                + "]";
    }
//--simple--preserve

}
