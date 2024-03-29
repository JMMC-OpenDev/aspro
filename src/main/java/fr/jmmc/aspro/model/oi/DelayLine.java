
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes a delay line (DL) present in the interferometer
 *             
 * 
 * <p>Java class for DelayLine complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="DelayLine"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}ID"/&gt;
 *         &lt;element name="station" type="{http://www.w3.org/2001/XMLSchema}IDREF" minOccurs="0"/&gt;
 *         &lt;element name="maximumThrow" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="setup" type="{http://www.jmmc.fr/aspro-oi/0.1}DelayLineModeSetup" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DelayLine", propOrder = {
    "name",
    "station",
    "maximumThrow",
    "setups"
})
public class DelayLine
    extends OIBase
{

    @XmlElement(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String name;
    @XmlElement(type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected Station station;
    protected double maximumThrow;
    @XmlElement(name = "setup")
    protected List<DelayLineModeSetup> setups;

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
     * Gets the value of the station property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public Station getStation() {
        return station;
    }

    /**
     * Sets the value of the station property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setStation(Station value) {
        this.station = value;
    }

    /**
     * Gets the value of the maximumThrow property.
     * 
     */
    public double getMaximumThrow() {
        return maximumThrow;
    }

    /**
     * Sets the value of the maximumThrow property.
     * 
     */
    public void setMaximumThrow(double value) {
        this.maximumThrow = value;
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
     * {@link DelayLineModeSetup }
     * 
     * 
     */
    public List<DelayLineModeSetup> getSetups() {
        if (setups == null) {
            setups = new ArrayList<DelayLineModeSetup>();
        }
        return this.setups;
    }
    
//--simple--preserve
    @Override
    public final boolean equals(final Object obj) {
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final DelayLine other = (DelayLine) obj;
        if ((this.name == null) ? (other.name != null) : !this.name.equals(other.name)) {
            return false;
        }
        return true;
    }

    @Override
    public final int hashCode() {
        int hash = 7;
        hash = 29 * hash + (this.name != null ? this.name.hashCode() : 0);
        return hash;
    }

    @Override
    public final String toString() {
        return "DelayLine : " + ((this.name != null) ? this.name : "undefined") + " = " + maximumThrow;
    }

    /**
     * @param mode  optional delay line mode
     * @return the maximumThrow corresponding to the given mode or default value if null
     */
    public double getMaximumThrow(final DelayLineMode mode) {
        if ((mode != null) && (setups != null)) {
            for (DelayLineModeSetup setup : getSetups()) {
                if (mode.equals(setup.getMode())) {
                    return setup.getMaximumThrow();
                }
            }
        }
        return getMaximumThrow();
    }

//--simple--preserve

}
