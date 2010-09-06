
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
 *         - atmosphere Quality (atmoQual) "Excellent" "Good" "Average" "Bad" "Awful"
 * 
 *         - dit "Detector Integration Time" /Choice 25 50 100
 *         - obsDuration "Total Integration Time (s) per calibrated point"
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

  /** computed observability data (read only) */
  @javax.xml.bind.annotation.XmlTransient
  private fr.jmmc.aspro.model.observability.ObservabilityData observabilityData = null;

  /**
   * Return the computed observability data (read only)
   * @return computed observability data or null
   */
  public fr.jmmc.aspro.model.observability.ObservabilityData getObservabilityData() {
    return this.observabilityData;
  }

  /**
   * Define the computed observability data (read only)
   * @param obsData computed observability data
   */
  public void setObservabilityData(final fr.jmmc.aspro.model.observability.ObservabilityData obsData) {
    this.observabilityData = obsData;
  }

  /** computed OIFits structure (read only) */
  @javax.xml.bind.annotation.XmlTransient
  private fr.jmmc.oitools.model.OIFitsFile oiFitsFile = null;

  /**
   * Return the computed OIFits structure (read only)
   * @return OIFits structure or null
   */
  public fr.jmmc.oitools.model.OIFitsFile getOIFitsFile() {
    return this.oiFitsFile;
  }

  /**
   * Define the computed OIFits structure (read only)
   * @param oiFitsFile computed OIFits structure
   */
  public void setOIFitsFile(final fr.jmmc.oitools.model.OIFitsFile oiFitsFile) {
    this.oiFitsFile = oiFitsFile;
  }

  @Override
  public String toString() {
    return "Observation : " + ((this.name != null) ? this.name : "undefined");
  }
  
//--simple--preserve

}
