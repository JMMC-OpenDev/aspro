
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         A generic value type
 *       
 * 
 * <p>Java class for BaseValue complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="BaseValue">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;attribute name="name" use="required" type="{http://www.w3.org/2001/XMLSchema}string" />
 *       &lt;attribute name="unit" type="{http://www.w3.org/2001/XMLSchema}string" />
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "BaseValue")
@XmlSeeAlso({
    BooleanValue.class,
    StringValue.class,
    NumberValue.class
})
public class BaseValue
    extends OIBase
{

    @XmlAttribute(name = "name", required = true)
    protected String name;
    @XmlAttribute(name = "unit")
    protected String unit;

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
     * Gets the value of the unit property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getUnit() {
        return unit;
    }

    /**
     * Sets the value of the unit property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setUnit(String value) {
        this.unit = value;
    }
    
//--simple--preserve
  /**
   * Return the value (implemented by child classes)
   * @return value
   */
  public Object getValue() {
    return null;
  }

  /**
   * Return the string value or null if this is not a StringValue instance
   * @return string value or null
   */
  public final String getString() {
    if (this instanceof StringValue) {
      return ((StringValue) this).getValue();
    }
    return null;
  }

  /**
   * Return the number value or null if this is not a NumberValue instance
   * @return number value or null
   */
  public final Double getNumber() {
    if (this instanceof NumberValue) {
      return ((NumberValue) this).getValue();
    }
    return null;
  }

  /**
   * Return the boolean value or null if this is not a BooleanValue instance
   * @return string value or null
   */
  public final Boolean getBoolean() {
    if (this instanceof BooleanValue) {
      return ((BooleanValue) this).isValue();
    }
    return null;
  }

  /**
   * Return a string representation containing (name, value and optional unit)
   * @return string representation
   */
  @Override
  public final String toString() {
    return "[name=" + getName() + ", value=" + getValue() + ((getUnit() != null) ? ", unit=" + getUnit() : "") + "]";
  }
//--simple--preserve

}
