
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes a target
 *       
 * 
 * <p>Java class for Target complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Target">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="RA" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="DEC" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="EQUINOX" type="{http://www.w3.org/2001/XMLSchema}float"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Target", propOrder = {
    "name",
    "ra",
    "dec",
    "equinox"
})
public class Target
    extends OIBase
{

    @XmlElement(required = true)
    protected String name;
    @XmlElement(name = "RA")
    protected double ra;
    @XmlElement(name = "DEC")
    protected double dec;
    @XmlElement(name = "EQUINOX")
    protected float equinox;

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
     * Gets the value of the ra property.
     * 
     */
    public double getRA() {
        return ra;
    }

    /**
     * Sets the value of the ra property.
     * 
     */
    public void setRA(double value) {
        this.ra = value;
    }

    /**
     * Gets the value of the dec property.
     * 
     */
    public double getDEC() {
        return dec;
    }

    /**
     * Sets the value of the dec property.
     * 
     */
    public void setDEC(double value) {
        this.dec = value;
    }

    /**
     * Gets the value of the equinox property.
     * 
     */
    public float getEQUINOX() {
        return equinox;
    }

    /**
     * Sets the value of the equinox property.
     * 
     */
    public void setEQUINOX(float value) {
        this.equinox = value;
    }

//--simple--preserve

    @Override
    public String toString() {
      return "Target [" + ((this.name != null) ? this.name : "undefined") + "]"
              + " RA = " + getRA() + " DEC = " + getDEC();
    }

//--simple--preserve

}
