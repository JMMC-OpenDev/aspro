
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes a value corresponding to the (optional) atmosphere quality
 *             
 * 
 * 
 *                         The value
 *                     
 * 
 * <p>Java class for AtmQualValue complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="AtmQualValue"&gt;
 *   &lt;simpleContent&gt;
 *     &lt;extension base="&lt;http://www.w3.org/2001/XMLSchema&gt;double"&gt;
 *       &lt;attribute name="atmosphereQuality" type="{http://www.jmmc.fr/aspro-oi/0.1}AtmosphereQuality" /&gt;
 *     &lt;/extension&gt;
 *   &lt;/simpleContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "AtmQualValue", propOrder = {
    "value"
})
public class AtmQualValue
    extends OIBase
{

    @XmlValue
    protected double value;
    @XmlAttribute(name = "atmosphereQuality")
    protected AtmosphereQuality atmosphereQuality;

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
    
//--simple--preserve
    @Override
    public String toString() {
        return "AtmQualValue{atmosphereQuality=" + atmosphereQuality + "value=" + value + '}';
    }

    boolean match(final AtmosphereQuality atmQual) {
        if ((this.atmosphereQuality != null) && (this.atmosphereQuality != atmQual)) {
            return false;
        }
        return true;
    }
//--simple--preserve

}
