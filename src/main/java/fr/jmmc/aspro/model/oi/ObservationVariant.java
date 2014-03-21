
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the overriden observation settings to form an observation variant i.e. multi configuration support :
 *                 - list of stations (U1 U2 U4)
 *             
 * 
 * <p>Java class for ObservationVariant complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ObservationVariant">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="stations" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ObservationVariant", propOrder = {
    "stations"
})
public class ObservationVariant
    extends OIBase
{

    @XmlElement(required = true)
    protected String stations;

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
    
//--simple--preserve

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
    
  @Override
  public final String toString() {
    return "ObservationVariant : " + getStations();
  }
//--simple--preserve

}
