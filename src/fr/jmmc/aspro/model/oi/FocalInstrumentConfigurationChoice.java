
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes the chosen instrument, stations and its parameters
 *       
 * 
 * <p>Java class for FocalInstrumentConfigurationChoice complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FocalInstrumentConfigurationChoice">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="stations" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="pops" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="instrumentMode" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FocalInstrumentConfigurationChoice", propOrder = {
    "name",
    "stations",
    "pops",
    "instrumentMode"
})
public class FocalInstrumentConfigurationChoice
    extends OIBase
{

    @XmlElement(required = true)
    protected String name;
    @XmlElement(required = true)
    protected String stations;
    protected String pops;
    @XmlElement(required = true)
    protected String instrumentMode;

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
     * Gets the value of the stations property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getStations() {
        return stations;
    }

    /**
     * Sets the value of the stations property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setStations(String value) {
        this.stations = value;
    }

    /**
     * Gets the value of the pops property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPops() {
        return pops;
    }

    /**
     * Sets the value of the pops property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPops(String value) {
        this.pops = value;
    }

    /**
     * Gets the value of the instrumentMode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInstrumentMode() {
        return instrumentMode;
    }

    /**
     * Sets the value of the instrumentMode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInstrumentMode(String value) {
        this.instrumentMode = value;
    }
    
//--simple--preserve
  /** resolved reference to the focal instrument configuration (read only) */
  @javax.xml.bind.annotation.XmlTransient
  private FocalInstrumentConfiguration instrumentConfiguration = null;

  public FocalInstrumentConfiguration getInstrumentConfiguration() {
    return instrumentConfiguration;
  }

  public void setInstrumentConfiguration(FocalInstrumentConfiguration instrumentConfiguration) {
    this.instrumentConfiguration = instrumentConfiguration;
  }

  /** resolved reference to the stations (read only) */
  @javax.xml.bind.annotation.XmlTransient
  private java.util.List<Station> stationList = null;

  public java.util.List<Station> getStationList() {
    return stationList;
  }

  public void setStationList(java.util.List<Station> stationList) {
    this.stationList = stationList;
  }

  /** resolved reference to the PoPs (read only) */
  @javax.xml.bind.annotation.XmlTransient
  private java.util.List<Pop> popList = null;

  public java.util.List<Pop> getPopList() {
    return popList;
  }

  public void setPopList(java.util.List<Pop> popList) {
    this.popList = popList;
  }

  /** resolved reference to the instrument mode (read only) */
  @javax.xml.bind.annotation.XmlTransient
  private FocalInstrumentMode focalInstrumentMode = null;

  public FocalInstrumentMode getFocalInstrumentMode() {
    return focalInstrumentMode;
  }

  public void setFocalInstrumentMode(FocalInstrumentMode focalInstrumentMode) {
    this.focalInstrumentMode = focalInstrumentMode;
  }
//--simple--preserve

}
