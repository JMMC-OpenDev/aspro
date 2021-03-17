
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes a bias value corresponding to the (optional) criteria: telescope, atmosphere quality
 *             
 * 
 * 
 *                         The bias value
 *                     
 * 
 * <p>Java class for BiasValue complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="BiasValue"&gt;
 *   &lt;simpleContent&gt;
 *     &lt;extension base="&lt;http://www.w3.org/2001/XMLSchema&gt;double"&gt;
 *       &lt;attribute name="type" use="required" type="{http://www.jmmc.fr/aspro-oi/0.1}BiasType" /&gt;
 *       &lt;attribute name="unit" type="{http://www.jmmc.fr/aspro-oi/0.1}BiasUnit" /&gt;
 *       &lt;attribute name="band" type="{http://www.jmmc.fr/aspro-oi/0.1}SpectralBand" /&gt;
 *       &lt;attribute name="atmosphereQuality" type="{http://www.jmmc.fr/aspro-oi/0.1}AtmosphereQuality" /&gt;
 *       &lt;attribute name="telescope" type="{http://www.w3.org/2001/XMLSchema}IDREF" /&gt;
 *     &lt;/extension&gt;
 *   &lt;/simpleContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "BiasValue", propOrder = {
    "value"
})
public class BiasValue
    extends OIBase
{

    @XmlValue
    protected double value;
    @XmlAttribute(name = "type", required = true)
    protected BiasType type;
    @XmlAttribute(name = "unit")
    protected BiasUnit unit;
    @XmlAttribute(name = "band")
    protected SpectralBand band;
    @XmlAttribute(name = "atmosphereQuality")
    protected AtmosphereQuality atmosphereQuality;
    @XmlAttribute(name = "telescope")
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected Telescope telescope;

    /**
     * Gets the value of the value property.
     * 
     */
    public double getValue() {
        return value;
    }

    /**
     * Sets the value of the value property.
     * 
     */
    public void setValue(double value) {
        this.value = value;
    }

    /**
     * Gets the value of the type property.
     * 
     * @return
     *     possible object is
     *     {@link BiasType }
     *     
     */
    public BiasType getType() {
        return type;
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value
     *     allowed object is
     *     {@link BiasType }
     *     
     */
    public void setType(BiasType value) {
        this.type = value;
    }

    /**
     * Gets the value of the unit property.
     * 
     * @return
     *     possible object is
     *     {@link BiasUnit }
     *     
     */
    public BiasUnit getUnit() {
        return unit;
    }

    /**
     * Sets the value of the unit property.
     * 
     * @param value
     *     allowed object is
     *     {@link BiasUnit }
     *     
     */
    public void setUnit(BiasUnit value) {
        this.unit = value;
    }

    /**
     * Gets the value of the band property.
     * 
     * @return
     *     possible object is
     *     {@link SpectralBand }
     *     
     */
    public SpectralBand getBand() {
        return band;
    }

    /**
     * Sets the value of the band property.
     * 
     * @param value
     *     allowed object is
     *     {@link SpectralBand }
     *     
     */
    public void setBand(SpectralBand value) {
        this.band = value;
    }

    /**
     * Gets the value of the atmosphereQuality property.
     * 
     * @return
     *     possible object is
     *     {@link AtmosphereQuality }
     *     
     */
    public AtmosphereQuality getAtmosphereQuality() {
        return atmosphereQuality;
    }

    /**
     * Sets the value of the atmosphereQuality property.
     * 
     * @param value
     *     allowed object is
     *     {@link AtmosphereQuality }
     *     
     */
    public void setAtmosphereQuality(AtmosphereQuality value) {
        this.atmosphereQuality = value;
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
    
//--simple--preserve
    
    /**
     * Gets the squared value of the value property.
     */
    public double getSquaredValue() {
        return value * value;
    }

    @Override
    public String toString() {
        return "BiasValue{" + "value=" + value + ", type=" + type + ", unit=" + unit + ", band=" + band + ", atmosphereQuality=" + atmosphereQuality + ", telescope=" + telescope + '}';
    }

    /**
     * Initialize and check this instance
     * @param logger logger to use
     * @throws IllegalStateException if the configuration is severly invalid !
     */
    public void init(final org.slf4j.Logger logger) throws IllegalStateException {
        // TODO: implement validation ?
    }
    
    boolean match(final BiasType type, final SpectralBand band, final AtmosphereQuality atmQual, final Telescope tel) {
        if (this.type != type) {
            return false;
        }
        if ((this.band != null) && (this.band != band)) {
            return false;
        }
        if ((this.atmosphereQuality != null) && (this.atmosphereQuality != atmQual)) {
            return false;
        }
        if ((this.telescope != null) && (this.telescope != tel)) {
            return false;
        }
        return true;
    }
    
//--simple--preserve

}
