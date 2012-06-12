
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlType;


/**
 * 
 *         A number value
 *       
 * 
 * <p>Java class for NumberValue complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="NumberValue">
 *   &lt;complexContent>
 *     &lt;extension base="{http://www.jmmc.fr/aspro-oi/0.1}BaseValue">
 *       &lt;attribute name="value" type="{http://www.w3.org/2001/XMLSchema}double" />
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "NumberValue")
public class NumberValue
    extends BaseValue
{

    @XmlAttribute(name = "value")
    protected Double value;

    /**
     * Gets the value of the value property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getValue() {
        return value;
    }

    /**
     * Sets the value of the value property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setValue(Double value) {
        this.value = value;
    }

}
