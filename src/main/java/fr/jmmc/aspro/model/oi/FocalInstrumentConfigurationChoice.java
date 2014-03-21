
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the chosen instrument, stations and its parameters
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
 *         &lt;element name="instrumentMode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="samplingPeriod" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="acquisitionTime" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
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
    "instrumentMode",
    "samplingPeriod",
    "acquisitionTime"
})
public class FocalInstrumentConfigurationChoice
    extends OIBase
{

    @XmlElement(required = true)
    protected String name;
    @XmlElement(required = true)
    protected String stations;
    protected String pops;
    protected String instrumentMode;
    protected Double samplingPeriod;
    protected Double acquisitionTime;

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

    /**
     * Gets the value of the samplingPeriod property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getSamplingPeriod() {
        return samplingPeriod;
    }

    /**
     * Sets the value of the samplingPeriod property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setSamplingPeriod(Double value) {
        this.samplingPeriod = value;
    }

    /**
     * Gets the value of the acquisitionTime property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getAcquisitionTime() {
        return acquisitionTime;
    }

    /**
     * Sets the value of the acquisitionTime property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setAcquisitionTime(Double value) {
        this.acquisitionTime = value;
    }
    
//--simple--preserve
  /** resolved reference to the focal instrument configuration (read only) */
  @javax.xml.bind.annotation.XmlTransient
  private FocalInstrumentConfiguration instrumentConfiguration = null;

  /**
   * Return the reference to the focal instrument configuration (read only)
   * @return focal instrument configuration or null
   */
  public final FocalInstrumentConfiguration getInstrumentConfiguration() {
    return instrumentConfiguration;
  }

  /**
   * Define the reference to the focal instrument configuration (read only)
   * @param instrumentConfiguration focal instrument configuration
   */
  public final void setInstrumentConfiguration(final FocalInstrumentConfiguration instrumentConfiguration) {
    this.instrumentConfiguration = instrumentConfiguration;
  }

  /** resolved reference to the list of Stations (read only) */
  @javax.xml.bind.annotation.XmlTransient
  private java.util.List<Station> stationList = null;

  /**
   * Return the reference to the list of Stations (read only)
   * @return list of Stations or null
   */
  public final java.util.List<Station> getStationList() {
    return stationList;
  }

  /**
   * Define the reference to the list of Stations (read only)
   * @param stationList list of Stations
   */
  public final void setStationList(final java.util.List<Station> stationList) {
    this.stationList = stationList;
  }

  /** resolved reference to the list of PoPs (read only) */
  @javax.xml.bind.annotation.XmlTransient
  private java.util.List<Pop> popList = null;

  /**
   * Return the reference to the list of PoPs (read only)
   * @return list of PoPs or null
   */
  public final java.util.List<Pop> getPopList() {
    return popList;
  }

  /**
   * Define the reference to the list of PoPs (read only)
   * @param popList list of PoPs
   */
  public final void setPopList(final java.util.List<Pop> popList) {
    this.popList = popList;
  }

  /** resolved reference to the instrument mode (read only) */
  @javax.xml.bind.annotation.XmlTransient
  private FocalInstrumentMode focalInstrumentMode = null;

  /**
   * Return the reference to the instrument mode (read only)
   * @return instrument mode or null
   */
  public final FocalInstrumentMode getFocalInstrumentMode() {
    return focalInstrumentMode;
  }

  /**
   * Define the reference to the instrument mode (read only)
   * @param focalInstrumentMode instrument mode
   */
  public final void setFocalInstrumentMode(final FocalInstrumentMode focalInstrumentMode) {
    this.focalInstrumentMode = focalInstrumentMode;
  }
  
//--simple--preserve

}
