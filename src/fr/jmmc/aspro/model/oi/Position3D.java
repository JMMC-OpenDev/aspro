
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This data type describes a carthesian position (x,y,z) in meters
 *       
 * 
 * <p>Java class for Position3D complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Position3D">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="posX" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="posY" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="posZ" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Position3D", propOrder = {
    "posX",
    "posY",
    "posZ"
})
public class Position3D
    extends OIBase
{

    protected double posX;
    protected double posY;
    protected double posZ;

    /**
     * Gets the value of the posX property.
     * 
     */
    public double getPosX() {
        return posX;
    }

    /**
     * Sets the value of the posX property.
     * 
     */
    public void setPosX(double value) {
        this.posX = value;
    }

    /**
     * Gets the value of the posY property.
     * 
     */
    public double getPosY() {
        return posY;
    }

    /**
     * Sets the value of the posY property.
     * 
     */
    public void setPosY(double value) {
        this.posY = value;
    }

    /**
     * Gets the value of the posZ property.
     * 
     */
    public double getPosZ() {
        return posZ;
    }

    /**
     * Sets the value of the posZ property.
     * 
     */
    public void setPosZ(double value) {
        this.posZ = value;
    }

}
