
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the configuration for a given interferometer and its instruments
 * 
 *                 http://www.eso.org/sci/facilities/paranal/telescopes/vlti/configuration/index.html
 *             
 * 
 * <p>Java class for InterferometerConfiguration complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="InterferometerConfiguration"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="version" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="interferometer" type="{http://www.w3.org/2001/XMLSchema}IDREF"/&gt;
 *         &lt;element name="switchyard" type="{http://www.w3.org/2001/XMLSchema}IDREF" minOccurs="0"/&gt;
 *         &lt;element name="instrument" type="{http://www.jmmc.fr/aspro-oi/0.1}FocalInstrumentConfiguration" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "InterferometerConfiguration", propOrder = {
    "name",
    "version",
    "interferometer",
    "switchyard",
    "instruments"
})
public class InterferometerConfiguration
    extends OIBase
{

    protected String name;
    protected String version;
    @XmlElement(required = true, type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected InterferometerDescription interferometer;
    @XmlElement(type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected SwitchYard switchyard;
    @XmlElement(name = "instrument")
    protected List<FocalInstrumentConfiguration> instruments;

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
     * Gets the value of the version property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getVersion() {
        return version;
    }

    /**
     * Sets the value of the version property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setVersion(String value) {
        this.version = value;
    }

    /**
     * Gets the value of the interferometer property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public InterferometerDescription getInterferometer() {
        return interferometer;
    }

    /**
     * Sets the value of the interferometer property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setInterferometer(InterferometerDescription value) {
        this.interferometer = value;
    }

    /**
     * Gets the value of the switchyard property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
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
     *     {@link Object }
     *     
     */
    public void setSwitchyard(SwitchYard value) {
        this.switchyard = value;
    }

    /**
     * Gets the value of the instruments property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the instruments property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getInstruments().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link FocalInstrumentConfiguration }
     * 
     * 
     */
    public List<FocalInstrumentConfiguration> getInstruments() {
        if (instruments == null) {
            instruments = new ArrayList<FocalInstrumentConfiguration>();
        }
        return this.instruments;
    }
    
//--simple--preserve
 /** minimum distance between 2 stations in meter */
  @javax.xml.bind.annotation.XmlTransient
  private double minBaseLine = 0d;

  /**
   * Return the minimum distance between 2 stations in meter
   * @return minimum distance between 2 stations in meter
   */
  public final double getMinBaseLine() {
    return minBaseLine;
  }

  /**
   * Define the minimum distance between 2 stations in meter
   * @param minBaseLine minimum distance between 2 stations in meter
   */
  public final void setMinBaseLine(final double minBaseLine) {
    this.minBaseLine = minBaseLine;
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

  /**
   * Return a deep "copy" of this instance
   * @return deep "copy" of this instance
   */
  @Override
  public final Object clone() {
    final InterferometerConfiguration copy = (InterferometerConfiguration) super.clone();

    // Copy list of instrument configuration:
    if (copy.instruments != null) {
      copy.instruments = new ArrayList<FocalInstrumentConfiguration>(copy.instruments);
    }

    return copy;
  }

  @Override
  public final String toString() {
    return "InterferometerConfiguration : " + ((this.name != null) ? this.name : "undefined");
  }

//--simple--preserve

}
