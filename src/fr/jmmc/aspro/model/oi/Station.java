
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes a station for a given telescope
 *       
 * 
 * <p>Java class for Station complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Station">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}ID"/>
 *         &lt;element name="telescope" type="{http://www.w3.org/2001/XMLSchema}IDREF"/>
 *         &lt;element name="relativePosition" type="{http://www.jmmc.fr/aspro-oi/0.1}Position3D"/>
 *         &lt;element name="posSph" type="{http://www.jmmc.fr/aspro-oi/0.1}LonLatAlt" minOccurs="0"/>
 *         &lt;element name="delayLineFixedOffset" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="horizon" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Station", propOrder = {
    "name",
    "telescope",
    "relativePosition",
    "posSph",
    "delayLineFixedOffset",
    "horizon"
})
public class Station
    extends OIBase
{

    @XmlElement(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String name;
    @XmlElement(required = true, type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected Telescope telescope;
    @XmlElement(required = true)
    protected Position3D relativePosition;
    protected LonLatAlt posSph;
    protected Double delayLineFixedOffset;
    protected String horizon;

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
     * Gets the value of the telescope property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public Telescope getTelescope() {
        return telescope;
    }

    /**
     * Sets the value of the telescope property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setTelescope(Telescope value) {
        this.telescope = value;
    }

    /**
     * Gets the value of the relativePosition property.
     * 
     * @return
     *     possible object is
     *     {@link Position3D }
     *     
     */
    public Position3D getRelativePosition() {
        return relativePosition;
    }

    /**
     * Sets the value of the relativePosition property.
     * 
     * @param value
     *     allowed object is
     *     {@link Position3D }
     *     
     */
    public void setRelativePosition(Position3D value) {
        this.relativePosition = value;
    }

    /**
     * Gets the value of the posSph property.
     * 
     * @return
     *     possible object is
     *     {@link LonLatAlt }
     *     
     */
    public LonLatAlt getPosSph() {
        return posSph;
    }

    /**
     * Sets the value of the posSph property.
     * 
     * @param value
     *     allowed object is
     *     {@link LonLatAlt }
     *     
     */
    public void setPosSph(LonLatAlt value) {
        this.posSph = value;
    }

    /**
     * Gets the value of the delayLineFixedOffset property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getDelayLineFixedOffset() {
        return delayLineFixedOffset;
    }

    /**
     * Sets the value of the delayLineFixedOffset property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setDelayLineFixedOffset(Double value) {
        this.delayLineFixedOffset = value;
    }

    /**
     * Gets the value of the horizon property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHorizon() {
        return horizon;
    }

    /**
     * Sets the value of the horizon property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHorizon(String value) {
        this.horizon = value;
    }

//--simple--preserve

  @Override
  public String toString() {
    return "Station : " + ((this.name != null) ? this.name : "undefined");
  }

//--simple--preserve

}
