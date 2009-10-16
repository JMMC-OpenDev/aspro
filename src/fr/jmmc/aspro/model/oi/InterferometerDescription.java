
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
 *         This type describes the interferometer site
 * 
 *         Information to add :
 * 
 *         obsMinElev 20
 * 
 *       
 * 
 * <p>Java class for InterferometerDescription complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="InterferometerDescription">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}ID"/>
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="position" type="{http://www.jmmc.fr/aspro-oi/0.1}Position3D"/>
 *         &lt;element name="posSph" type="{http://www.jmmc.fr/aspro-oi/0.1}LonLatAlt" minOccurs="0"/>
 *         &lt;element name="telescope" type="{http://www.jmmc.fr/aspro-oi/0.1}Telescope" maxOccurs="unbounded"/>
 *         &lt;element name="station" type="{http://www.jmmc.fr/aspro-oi/0.1}Station" maxOccurs="unbounded"/>
 *         &lt;element name="channel" type="{http://www.jmmc.fr/aspro-oi/0.1}Channel" maxOccurs="unbounded"/>
 *         &lt;element name="switchyard" type="{http://www.jmmc.fr/aspro-oi/0.1}SwitchYard" minOccurs="0"/>
 *         &lt;element name="delayLine" type="{http://www.jmmc.fr/aspro-oi/0.1}DelayLine" maxOccurs="unbounded"/>
 *         &lt;element name="pop" type="{http://www.jmmc.fr/aspro-oi/0.1}Pop" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="fringeTracker" type="{http://www.jmmc.fr/aspro-oi/0.1}FringeTracker" minOccurs="0"/>
 *         &lt;element name="focalInstrument" type="{http://www.jmmc.fr/aspro-oi/0.1}FocalInstrument" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "InterferometerDescription", propOrder = {
    "name",
    "description",
    "position",
    "posSph",
    "telescopes",
    "stations",
    "channels",
    "switchyard",
    "delayLines",
    "pops",
    "fringeTracker",
    "focalInstruments"
})
public class InterferometerDescription
    extends OIBase
{

    @XmlElement(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String name;
    @XmlElement(required = true)
    protected String description;
    @XmlElement(required = true)
    protected Position3D position;
    protected LonLatAlt posSph;
    @XmlElement(name = "telescope", required = true)
    protected List<Telescope> telescopes;
    @XmlElement(name = "station", required = true)
    protected List<Station> stations;
    @XmlElement(name = "channel", required = true)
    protected List<Channel> channels;
    protected SwitchYard switchyard;
    @XmlElement(name = "delayLine", required = true)
    protected List<DelayLine> delayLines;
    @XmlElement(name = "pop")
    protected List<Pop> pops;
    protected FringeTracker fringeTracker;
    @XmlElement(name = "focalInstrument", required = true)
    protected List<FocalInstrument> focalInstruments;

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
     * Gets the value of the description property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDescription() {
        return description;
    }

    /**
     * Sets the value of the description property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDescription(String value) {
        this.description = value;
    }

    /**
     * Gets the value of the position property.
     * 
     * @return
     *     possible object is
     *     {@link Position3D }
     *     
     */
    public Position3D getPosition() {
        return position;
    }

    /**
     * Sets the value of the position property.
     * 
     * @param value
     *     allowed object is
     *     {@link Position3D }
     *     
     */
    public void setPosition(Position3D value) {
        this.position = value;
    }

    /**
     * Gets the value of the posSph property.
     * 
     * @return
     *     possible object is
     *     {@link LonLatAlt }
     *     
     */
    public LonLatAlt getPosSph() {
        return posSph;
    }

    /**
     * Sets the value of the posSph property.
     * 
     * @param value
     *     allowed object is
     *     {@link LonLatAlt }
     *     
     */
    public void setPosSph(LonLatAlt value) {
        this.posSph = value;
    }

    /**
     * Gets the value of the telescopes property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the telescopes property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getTelescopes().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Telescope }
     * 
     * 
     */
    public List<Telescope> getTelescopes() {
        if (telescopes == null) {
            telescopes = new ArrayList<Telescope>();
        }
        return this.telescopes;
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
     * {@link Station }
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
     * {@link Channel }
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
     * Gets the value of the switchyard property.
     * 
     * @return
     *     possible object is
     *     {@link SwitchYard }
     *     
     */
    public SwitchYard getSwitchyard() {
        return switchyard;
    }

    /**
     * Sets the value of the switchyard property.
     * 
     * @param value
     *     allowed object is
     *     {@link SwitchYard }
     *     
     */
    public void setSwitchyard(SwitchYard value) {
        this.switchyard = value;
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
     * {@link DelayLine }
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
     * {@link Pop }
     * 
     * 
     */
    public List<Pop> getPops() {
        if (pops == null) {
            pops = new ArrayList<Pop>();
        }
        return this.pops;
    }

    /**
     * Gets the value of the fringeTracker property.
     * 
     * @return
     *     possible object is
     *     {@link FringeTracker }
     *     
     */
    public FringeTracker getFringeTracker() {
        return fringeTracker;
    }

    /**
     * Sets the value of the fringeTracker property.
     * 
     * @param value
     *     allowed object is
     *     {@link FringeTracker }
     *     
     */
    public void setFringeTracker(FringeTracker value) {
        this.fringeTracker = value;
    }

    /**
     * Gets the value of the focalInstruments property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the focalInstruments property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getFocalInstruments().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link FocalInstrument }
     * 
     * 
     */
    public List<FocalInstrument> getFocalInstruments() {
        if (focalInstruments == null) {
            focalInstruments = new ArrayList<FocalInstrument>();
        }
        return this.focalInstruments;
    }
    
//--simple--preserve

  /* custom members */

  /** list of configurations referring to this interferometer (read only) */
  @javax.xml.bind.annotation.XmlTransient
  private List<InterferometerConfiguration> configurations = null;

  /**
   * @return list of configurations using this interferometer
   */
  public List<InterferometerConfiguration> getConfigurations() {
    if (configurations == null) {
      configurations = new ArrayList<InterferometerConfiguration>();
    }
    return configurations;
  }

  @Override
  public String toString() {
    return "InterferometerDescription [" + ((this.name != null) ? this.name : "undefined") + "]";
  }
//--simple--preserve

}
