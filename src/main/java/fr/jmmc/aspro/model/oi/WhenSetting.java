
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.datatype.XMLGregorianCalendar;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes when the observation will happen
 *             
 * 
 * <p>Java class for WhenSetting complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="WhenSetting"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="date" type="{http://www.w3.org/2001/XMLSchema}date"/&gt;
 *         &lt;element name="nightRestriction" type="{http://www.w3.org/2001/XMLSchema}boolean"/&gt;
 *         &lt;element name="atmosphereQuality" type="{http://www.jmmc.fr/aspro-oi/0.1}AtmosphereQuality" minOccurs="0"/&gt;
 *         &lt;element name="windAzimuth" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "WhenSetting", propOrder = {
    "date",
    "nightRestriction",
    "atmosphereQuality",
    "windAzimuth"
})
public class WhenSetting
    extends OIBase
{

    @XmlElement(required = true)
    @XmlSchemaType(name = "date")
    protected XMLGregorianCalendar date;
    @XmlElement(defaultValue = "true")
    protected boolean nightRestriction;
    
    protected AtmosphereQuality atmosphereQuality;
    protected Double windAzimuth;

    /**
     * Gets the value of the date property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getDate() {
        return date;
    }

    /**
     * Sets the value of the date property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setDate(XMLGregorianCalendar value) {
        this.date = value;
    }

    /**
     * Gets the value of the nightRestriction property.
     * 
     */
    public boolean isNightRestriction() {
        return nightRestriction;
    }

    /**
     * Sets the value of the nightRestriction property.
     * 
     */
    public void setNightRestriction(boolean value) {
        this.nightRestriction = value;
    }

    /**
     * Gets the value of the atmosphereQuality property.
     * 
     * @return
     *     possible object is
     *     {@link AtmosphereQuality }
     *     
     */
    public AtmosphereQuality getAtmosphereQuality() {
        return atmosphereQuality;
    }

    /**
     * Sets the value of the atmosphereQuality property.
     * 
     * @param value
     *     allowed object is
     *     {@link AtmosphereQuality }
     *     
     */
    public void setAtmosphereQuality(AtmosphereQuality value) {
        this.atmosphereQuality = value;
    }

    /**
     * Gets the value of the windAzimuth property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getWindAzimuth() {
        return windAzimuth;
    }

    /**
     * Sets the value of the windAzimuth property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setWindAzimuth(Double value) {
        this.windAzimuth = value;
    }
    
//--simple--preserve

    @Override
    protected boolean areEquals(final OIBase o) {
        if (!super.areEquals(o)) {
            return false;
        }
        final WhenSetting other = (WhenSetting)o;
        return (areEquals(this.date, other.getDate())
                && areEquals(this.nightRestriction, other.isNightRestriction())
                && areEquals(this.atmosphereQuality, other.getAtmosphereQuality())
                && areEquals(this.windAzimuth, other.getWindAzimuth()));
    }
    
//--simple--preserve

}
