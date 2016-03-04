
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
 *                 This type describes the specific delay line throw associated to a delay line restriction
 *             
 * 
 * 
 *                         Maximum throw in m (optical round drip i.e. 2 times the delay line position)
 *                     
 * 
 * <p>Java class for DelayLineRestrictionThrow complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="DelayLineRestrictionThrow"&gt;
 *   &lt;simpleContent&gt;
 *     &lt;extension base="&lt;http://www.w3.org/2001/XMLSchema&gt;double"&gt;
 *       &lt;attribute name="restriction" use="required" type="{http://www.w3.org/2001/XMLSchema}IDREF" /&gt;
 *     &lt;/extension&gt;
 *   &lt;/simpleContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DelayLineRestrictionThrow", propOrder = {
    "value"
})
public class DelayLineRestrictionThrow
    extends OIBase
{

    @XmlValue
    protected double value;
    @XmlAttribute(name = "restriction", required = true)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected DelayLineRestriction restriction;

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
     * Gets the value of the restriction property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public DelayLineRestriction getRestriction() {
        return restriction;
    }

    /**
     * Sets the value of the restriction property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setRestriction(DelayLineRestriction value) {
        this.restriction = value;
    }
    
//--simple--preserve
  @Override
  public final String toString() {
    return "DLRestThrow : " + value;
  }
//--simple--preserve

}
