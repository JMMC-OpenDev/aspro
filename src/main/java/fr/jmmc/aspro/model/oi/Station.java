
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
 *                 This type describes a station for a given telescope
 *             
 * 
 * <p>Java class for Station complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Station"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}ID"/&gt;
 *         &lt;element name="telescope" type="{http://www.w3.org/2001/XMLSchema}IDREF"/&gt;
 *         &lt;element name="relativePosition" type="{http://www.jmmc.fr/aspro-oi/0.1}Position3D"/&gt;
 *         &lt;element name="delayLineFixedOffset" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="popLink" type="{http://www.jmmc.fr/aspro-oi/0.1}PopLink" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="horizon" type="{http://www.jmmc.fr/aspro-oi/0.1}HorizonProfile" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Station", propOrder = {
    "name",
    "telescope",
    "relativePosition",
    "delayLineFixedOffset",
    "popLinks",
    "horizon"
})
public class Station
    extends OIBase
{

    @XmlElement(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String name;
    @XmlElement(required = true, type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected Telescope telescope;
    @XmlElement(required = true)
    protected Position3D relativePosition;
    protected Double delayLineFixedOffset;
    @XmlElement(name = "popLink")
    protected List<PopLink> popLinks;
    protected HorizonProfile horizon;

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
     * Gets the value of the telescope property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public Telescope getTelescope() {
        return telescope;
    }

    /**
     * Sets the value of the telescope property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setTelescope(Telescope value) {
        this.telescope = value;
    }

    /**
     * Gets the value of the relativePosition property.
     * 
     * @return
     *     possible object is
     *     {@link Position3D }
     *     
     */
    public Position3D getRelativePosition() {
        return relativePosition;
    }

    /**
     * Sets the value of the relativePosition property.
     * 
     * @param value
     *     allowed object is
     *     {@link Position3D }
     *     
     */
    public void setRelativePosition(Position3D value) {
        this.relativePosition = value;
    }

    /**
     * Gets the value of the delayLineFixedOffset property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getDelayLineFixedOffset() {
        return delayLineFixedOffset;
    }

    /**
     * Sets the value of the delayLineFixedOffset property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setDelayLineFixedOffset(Double value) {
        this.delayLineFixedOffset = value;
    }

    /**
     * Gets the value of the popLinks property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the popLinks property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getPopLinks().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link PopLink }
     * 
     * 
     */
    public List<PopLink> getPopLinks() {
        if (popLinks == null) {
            popLinks = new ArrayList<PopLink>();
        }
        return this.popLinks;
    }

    /**
     * Gets the value of the horizon property.
     * 
     * @return
     *     possible object is
     *     {@link HorizonProfile }
     *     
     */
    public HorizonProfile getHorizon() {
        return horizon;
    }

    /**
     * Sets the value of the horizon property.
     * 
     * @param value
     *     allowed object is
     *     {@link HorizonProfile }
     *     
     */
    public void setHorizon(HorizonProfile value) {
        this.horizon = value;
    }
    
//--simple--preserve
    /** cached unique station identifier (used for its horizon profile) */
    @javax.xml.bind.annotation.XmlTransient
    private String key = null;

    /**
     * Return the (cached) station key
     * @param interferometerName interferometer name used to compute a cache key (interferometer name - station name)
     * @return "interferometer name - station name"
     */
    public final String getKey(final String interferometerName) {
        if (this.key == null) {
            // profile key = '<interferometer> - <station>' :
            this.key = interferometerName + " - " + this.name;
        }
        return this.key;
    }

    @Override
    public final String toString() {
        return "Station : " + ((this.name != null) ? this.name : "undefined");
    }

//--simple--preserve

}
