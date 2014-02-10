
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This data type describes a position in the horizontal frame (azimuth, elevation)
 *       
 * 
 * <p>Java class for AzEl complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="AzEl">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="azimuth" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="elevation" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "AzEl", propOrder = {
    "azimuth",
    "elevation"
})
public class AzEl
    extends OIBase
{

    protected double azimuth;
    protected double elevation;

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
     * Gets the value of the elevation property.
     * 
     */
    public double getElevation() {
        return elevation;
    }

    /**
     * Sets the value of the elevation property.
     * 
     */
    public void setElevation(double value) {
        this.elevation = value;
    }
    
//--simple--preserve
  /**
   * Empty constructor for JAXB
   */
  public AzEl() {
    super();
  }

  /**
   * constructor with azimuth and elevation
   * @param az azimuth in degrees
   * @param el elevation in degrees
   */
  public AzEl(final double az, final double el) {
    super();
    this.azimuth = az;
    this.elevation = el;
  }

  /**
   * Define azimuth and elevation
   * @param az azimuth in degrees
   * @param el elevation in degrees
   */
  public void setAzEl(final double az, final double el) {
    this.azimuth = az;
    this.elevation = el;
  }

  /**
   * Return a string representation
   * @return (az, el)
   */
  @Override
  public String toString() {
    return "(" + this.azimuth + ", " + this.elevation + ")";
  }
//--simple--preserve

}
