
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes the observation settings (when, where, what)
 * 
 *         Information to add :
 * 
 *         - focal instrument (focalinst)
 *         - configuration (configIndex)
 *         - insMode
 *         - insNchan
 *         - insRes
 * 
 *         - atmosphere Quality (atmoQual)
 *         - min elevation for the target (minElev) "Min. Elevation (degrees)" /range oipt%obsMinElev 90
 * 
 *         - polychromatic (flag)
 * 
 *         - target(s)
 * 
 *    let oipt%popslist 'oipt%popslist' /Prompt "Pops Config (eg. 1234, 0000=none)"
 * 
 *    let oipt%atmoQual 'oipt%atmoQual' /prompt "Atmosphere Quality" /choice "Excellent" "Good" "Average" "Bad" "Awful"
 * 
 *    let oipt%dit 'oipt%dit' /prompt "Detector Integration Time" /Choice 25 50 100
 * 
 *    let oipt%insMode 'oipt%insMode' /prompt "Spectral Mode" /INDEX oipt%vlti%amber%insMode
 * 
 *    let oipt%polychromatic 'oipt%polychromatic' /prompt "Simulate Instrument`s spectral resolution"
 * 
 *    let oipt%obsDuration 'oipt%obsDuration' /prompt "Total Integration Time (s) per calibrated point"
 * 
 *   let oipt%ha1 'oipt%ha1' /prompt "Hour Angle Start" /range -12 12 *
 *   let oipt%ha2 'oipt%ha2' /prompt "Hour Angle End" /range -12 12 *
 *   let oipt%minElev 'oipt%minElev' /prompt "Min. Elevation (degrees)" /range oipt%obsMinElev 90
 *   let oipt%samplingTime 'oipt%samplingTime' /prompt "Observing Sampling Periodicity (min)"
 * 
 *       
 * 
 * <p>Java class for Observation complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Observation">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="when" type="{http://www.jmmc.fr/aspro-oi/0.1}WhenSetting"/>
 *         &lt;element name="interferometerConfiguration" type="{http://www.jmmc.fr/aspro-oi/0.1}InterferometerConfigurationChoice"/>
 *         &lt;element name="instrumentConfiguration" type="{http://www.jmmc.fr/aspro-oi/0.1}FocalInstrumentConfigurationChoice"/>
 *         &lt;element name="target" type="{http://www.jmmc.fr/aspro-oi/0.1}Target" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Observation", propOrder = {
    "name",
    "when",
    "interferometerConfiguration",
    "instrumentConfiguration",
    "targets"
})
@XmlRootElement(name = "observationSetting")
public class ObservationSetting
    extends OIBase
{

    @XmlElement(required = true)
    protected String name;
    @XmlElement(required = true)
    protected WhenSetting when;
    @XmlElement(required = true)
    protected InterferometerConfigurationChoice interferometerConfiguration;
    @XmlElement(required = true)
    protected FocalInstrumentConfigurationChoice instrumentConfiguration;
    @XmlElement(name = "target", required = true)
    protected List<Target> targets;

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
     * Gets the value of the when property.
     * 
     * @return
     *     possible object is
     *     {@link WhenSetting }
     *     
     */
    public WhenSetting getWhen() {
        return when;
    }

    /**
     * Sets the value of the when property.
     * 
     * @param value
     *     allowed object is
     *     {@link WhenSetting }
     *     
     */
    public void setWhen(WhenSetting value) {
        this.when = value;
    }

    /**
     * Gets the value of the interferometerConfiguration property.
     * 
     * @return
     *     possible object is
     *     {@link InterferometerConfigurationChoice }
     *     
     */
    public InterferometerConfigurationChoice getInterferometerConfiguration() {
        return interferometerConfiguration;
    }

    /**
     * Sets the value of the interferometerConfiguration property.
     * 
     * @param value
     *     allowed object is
     *     {@link InterferometerConfigurationChoice }
     *     
     */
    public void setInterferometerConfiguration(InterferometerConfigurationChoice value) {
        this.interferometerConfiguration = value;
    }

    /**
     * Gets the value of the instrumentConfiguration property.
     * 
     * @return
     *     possible object is
     *     {@link FocalInstrumentConfigurationChoice }
     *     
     */
    public FocalInstrumentConfigurationChoice getInstrumentConfiguration() {
        return instrumentConfiguration;
    }

    /**
     * Sets the value of the instrumentConfiguration property.
     * 
     * @param value
     *     allowed object is
     *     {@link FocalInstrumentConfigurationChoice }
     *     
     */
    public void setInstrumentConfiguration(FocalInstrumentConfigurationChoice value) {
        this.instrumentConfiguration = value;
    }

    /**
     * Gets the value of the targets property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the targets property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getTargets().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Target }
     * 
     * 
     */
    public List<Target> getTargets() {
        if (targets == null) {
            targets = new ArrayList<Target>();
        }
        return this.targets;
    }
    
//--simple--preserve

    @Override
    public String toString() {
      return "Observation : " + ((this.name != null) ? this.name : "undefined");
    }

//--simple--preserve

}
