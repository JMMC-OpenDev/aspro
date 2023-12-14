
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
 *                 This type describes a value associated to (optional) atmosphere quality and telescope tuple
 *             
 * 
 * 
 *                         The value
 *                     
 * 
 * <p>Java class for AtmTelValue complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="AtmTelValue"&gt;
 *   &lt;simpleContent&gt;
 *     &lt;extension base="&lt;http://www.w3.org/2001/XMLSchema&gt;double"&gt;
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
@XmlType(name = "AtmTelValue", propOrder = {
    "value"
})
public class AtmTelValue
    extends OIBase
{

    @XmlValue
    protected double value;
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
    @Override
    public String toString() {
        return "AtmTelValue{" + "value=" + value + ", atmosphereQuality=" + atmosphereQuality + ", telescope=" + telescope + '}';
    }

    boolean match(final AtmosphereQuality atmQual, final Telescope tel) {
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
