
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes a flux condition (band, operation and value)
 *             
 * 
 * <p>Java class for FluxCondition complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FluxCondition"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="band" type="{http://www.jmmc.fr/aspro-oi/0.1}SpectralBand"/&gt;
 *         &lt;element name="op" type="{http://www.jmmc.fr/aspro-oi/0.1}Operator"/&gt;
 *         &lt;element name="value" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FluxCondition", propOrder = {
    "band",
    "op",
    "value"
})
public class FluxCondition
    extends OIBase
{

    @XmlElement(required = true)
    
    protected SpectralBand band;
    @XmlElement(required = true)
    
    protected Operator op;
    protected double value;

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
     * Gets the value of the op property.
     * 
     * @return
     *     possible object is
     *     {@link Operator }
     *     
     */
    public Operator getOp() {
        return op;
    }

    /**
     * Sets the value of the op property.
     * 
     * @param value
     *     allowed object is
     *     {@link Operator }
     *     
     */
    public void setOp(Operator value) {
        this.op = value;
    }

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
    
//--simple--preserve

    @Override
    public String toString() {
        return "FluxCondition{" + "band=" + band + ", op=" + op + ", value=" + value + '}';
    }
    
//--simple--preserve

}
