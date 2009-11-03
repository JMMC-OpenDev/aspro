
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This data type describes a celestial coordinates in the horizontal frame (azimuth, altitude)
 *       
 * 
 * <p>Java class for AzAlt complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="AzAlt">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="azimuth" type="{http://www.w3.org/2001/XMLSchema}double"/>
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
@XmlType(name = "AzAlt", propOrder = {
    "azimuth",
    "altitude"
})
public class AzAlt
    extends OIBase
{

    protected double azimuth;
    protected double altitude;

    /**
     * Gets the value of the azimuth property.
     * 
     */
    public double getAzimuth() {
        return azimuth;
    }

    /**
     * Sets the value of the azimuth property.
     * 
     */
    public void setAzimuth(double value) {
        this.azimuth = value;
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
    public AzAlt() {
      super();
    }

    public AzAlt(final double az, final double alt) {
      super();
      this.azimuth = az;
      this.altitude = alt;
    }

//--simple--preserve

}
