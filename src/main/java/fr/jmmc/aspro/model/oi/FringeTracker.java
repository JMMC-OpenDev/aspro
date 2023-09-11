
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
 *                 This type describes a Fringe Tracker system
 *             
 * 
 * <p>Java class for FringeTracker complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FringeTracker"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}ID"/&gt;
 *         &lt;element name="band" type="{http://www.jmmc.fr/aspro-oi/0.1}SpectralBand"/&gt;
 *         &lt;element name="mode" type="{http://www.w3.org/2001/XMLSchema}string" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="instrumentVisibility" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="magLimit" type="{http://www.jmmc.fr/aspro-oi/0.1}AtmTelValue" maxOccurs="unbounded"/&gt;
 *         &lt;element name="maxIntegration" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FringeTracker", propOrder = {
    "name",
    "band",
    "modes",
    "instrumentVisibility",
    "magLimits",
    "maxIntegration"
})
public class FringeTracker
    extends OIBase
{

    @XmlElement(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String name;
    @XmlElement(required = true)
    
    protected SpectralBand band;
    @XmlElement(name = "mode")
    protected List<String> modes;
    protected double instrumentVisibility;
    @XmlElement(name = "magLimit", required = true)
    protected List<AtmTelValue> magLimits;
    protected double maxIntegration;

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
     * Gets the value of the modes property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the modes property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getModes().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getModes() {
        if (modes == null) {
            modes = new ArrayList<String>();
        }
        return this.modes;
    }

    /**
     * Gets the value of the instrumentVisibility property.
     * 
     */
    public double getInstrumentVisibility() {
        return instrumentVisibility;
    }

    /**
     * Sets the value of the instrumentVisibility property.
     * 
     */
    public void setInstrumentVisibility(double value) {
        this.instrumentVisibility = value;
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

    /**
     * Gets the value of the maxIntegration property.
     * 
     */
    public double getMaxIntegration() {
        return maxIntegration;
    }

    /**
     * Sets the value of the maxIntegration property.
     * 
     */
    public void setMaxIntegration(double value) {
        this.maxIntegration = value;
    }
    
//--simple--preserve
    @Override
    public final String toString() {
        return "FringeTracker [" + ((this.name != null) ? this.name : "undefined") + "]";
    }
    
    /**
    * @param atmQual optional atmosphere quality
    * @param tel optional telescope
    * @return magnitude limit associated to the given atmosphere quality and telescope (or default)
    */
    public double getMagLimit(final AtmosphereQuality atmQual, final Telescope tel) {
        for (AtmTelValue val : getMagLimits()) {
            if (val.match(atmQual, tel)) {
                return val.getValue(); // first, so take care of ordering values
            }
        }
        return Double.NaN;
    }    
//--simple--preserve

}
