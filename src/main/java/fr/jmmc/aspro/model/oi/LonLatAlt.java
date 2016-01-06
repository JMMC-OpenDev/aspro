
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This data type describes a geographic position (longitude,latitude,altitude)
 *             
 * 
 * <p>Java class for LonLatAlt complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="LonLatAlt">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="longitude" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="latitude" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="altitude" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "LonLatAlt", propOrder = {
    "longitude",
    "latitude",
    "altitude"
})
public class LonLatAlt
    extends OIBase
{

    protected double longitude;
    protected double latitude;
    protected double altitude;

    /**
     * Gets the value of the longitude property.
     * 
     */
    public double getLongitude() {
        return longitude;
    }

    /**
     * Sets the value of the longitude property.
     * 
     */
    public void setLongitude(double value) {
        this.longitude = value;
    }

    /**
     * Gets the value of the latitude property.
     * 
     */
    public double getLatitude() {
        return latitude;
    }

    /**
     * Sets the value of the latitude property.
     * 
     */
    public void setLatitude(double value) {
        this.latitude = value;
    }

    /**
     * Gets the value of the altitude property.
     * 
     */
    public double getAltitude() {
        return altitude;
    }

    /**
     * Sets the value of the altitude property.
     * 
     */
    public void setAltitude(double value) {
        this.altitude = value;
    }
    
//--simple--preserve
  /**
   * Empty constructor for JAXB
   */
  public LonLatAlt() {
    super();
  }

  /**
   * Custom constructor
   * @param lon longitude in radians
   * @param lat latitude in radians
   * @param alt altitude in meters
   */
  public LonLatAlt(final double lon, final double lat, final double alt) {
    super();
    this.longitude = lon;
    this.latitude = lat;
    this.altitude = alt;
  }

  @Override
  public final String toString() {
    return "[" + fr.jmmc.aspro.service.GeocentricCoords.toString(getLongitude(), getLatitude(), getAltitude()) + "]["
            + net.jafama.FastMath.toDegrees(getLongitude()) + ", " + net.jafama.FastMath.toDegrees(getLatitude()) + "]";
  }
//--simple--preserve

}
