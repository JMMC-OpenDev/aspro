
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlList;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes a focal instrument configuration item (AMBER : U1-U2-U4)
 *             
 * 
 * <p>Java class for FocalInstrumentConfigurationItem complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FocalInstrumentConfigurationItem"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="altName" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="stations" type="{http://www.w3.org/2001/XMLSchema}IDREFS"/&gt;
 *         &lt;element name="channels" type="{http://www.w3.org/2001/XMLSchema}IDREFS" minOccurs="0"/&gt;
 *         &lt;element name="delayLines" type="{http://www.w3.org/2001/XMLSchema}IDREFS" minOccurs="0"/&gt;
 *         &lt;element name="pops" type="{http://www.w3.org/2001/XMLSchema}IDREFS" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FocalInstrumentConfigurationItem", propOrder = {
    "altName",
    "stations",
    "channels",
    "delayLines",
    "pops"
})
public class FocalInstrumentConfigurationItem
    extends OIBase
{

    protected String altName;
    @XmlList
    @XmlElement(required = true, type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREFS")
    protected List<Station> stations;
    @XmlList
    @XmlElement(type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREFS")
    protected List<Channel> channels;
    @XmlList
    @XmlElement(type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREFS")
    protected List<DelayLine> delayLines;
    @XmlList
    @XmlElement(type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREFS")
    protected List<Pop> pops;

    /**
     * Gets the value of the altName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAltName() {
        return altName;
    }

    /**
     * Sets the value of the altName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAltName(String value) {
        this.altName = value;
    }

    /**
     * Gets the value of the stations property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the stations property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getStations().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Object }
     * 
     * 
     */
    public List<Station> getStations() {
        if (stations == null) {
            stations = new ArrayList<Station>();
        }
        return this.stations;
    }

    /**
     * Gets the value of the channels property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the channels property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getChannels().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Object }
     * 
     * 
     */
    public List<Channel> getChannels() {
        if (channels == null) {
            channels = new ArrayList<Channel>();
        }
        return this.channels;
    }

    /**
     * Gets the value of the delayLines property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the delayLines property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getDelayLines().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Object }
     * 
     * 
     */
    public List<DelayLine> getDelayLines() {
        if (delayLines == null) {
            delayLines = new ArrayList<DelayLine>();
        }
        return this.delayLines;
    }

    /**
     * Gets the value of the pops property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the pops property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getPops().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Object }
     * 
     * 
     */
    public List<Pop> getPops() {
        if (pops == null) {
            pops = new ArrayList<Pop>();
        }
        return this.pops;
    }
    
//--simple--preserve
    /** computed name */
    @javax.xml.bind.annotation.XmlTransient
    private String name = null;

    /**
     * Return the name of this configuration i.e. 'XX YY ZZ' where station names are XX, YY and ZZ
     * @return name i.e. 'XX YY ZZ'
     */
    public final String getName() {
        String s = this.name;
        if (s == null) {
            s = generateName();
        }
        return s;
    }

    /**
     * Generate the name as a string containing station names like 'XX YY ZZ'
     * @return generated name
     */
    private final String generateName() {
        synchronized (this) {
            String s = this.name;
            if (s == null) {
                final StringBuilder sb = new StringBuilder();
                for (Station station : getStations()) {
                    sb.append(station.getName()).append(' ');
                }
                sb.deleteCharAt(sb.length() - 1);
                s = sb.toString();
                this.name = s;
            }
            return s;
        }
    }
    /** maximum distance between 2 stations in meter */
    @javax.xml.bind.annotation.XmlTransient
    private double maxBaseLine = 0d;

    /**
     * Return the maximum distance between 2 stations in meter
     * @return maximum distance between 2 stations in meter
     */
    public final double getMaxBaseLine() {
        return maxBaseLine;
    }

    /**
     * Define the maximum distance between 2 stations in meter
     * @param maxBaseLine maximum distance between 2 stations in meter
     */
    public final void setMaxBaseLine(final double maxBaseLine) {
        this.maxBaseLine = maxBaseLine;
    }

    @Override
    public final String toString() {
        return "FocalInstrumentConfigurationItem [ stations: " + getName() + " - pops: " + getPops() + " - channels: " + getChannels() + " - "
                + getMaxBaseLine() + " m]";
    }
//--simple--preserve

}
