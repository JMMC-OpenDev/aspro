package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;

/**
 * 
 *         This type describes the list of calibrators associated to a target
 *       
 * 
 * <p>Java class for TargetInformation complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="TargetInformation">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="calibrators" type="{http://www.w3.org/2001/XMLSchema}string" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "TargetInformation", propOrder = {
  "name",
  "description",
  "calibrators"
})
public class TargetInformation
        extends OIBase {

  @XmlElement(required = true)
  protected String name;
  protected String description;
  protected List<String> calibrators;

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
   * Gets the value of the description property.
   *
   * @return
   *     possible object is
   *     {@link String }
   *
   */
  public String getDescription() {
    return description;
  }

  /**
   * Sets the value of the description property.
   *
   * @param value
   *     allowed object is
   *     {@link String }
   *
   */
  public void setDescription(String value) {
    this.description = value;
  }

  /**
   * Gets the value of the calibrators property.
   *
   * <p>
   * This accessor method returns a reference to the live list,
   * not a snapshot. Therefore any modification you make to the
   * returned list will be present inside the JAXB object.
   * This is why there is not a <CODE>set</CODE> method for the calibrators property.
   *
   * <p>
   * For example, to add a new item, do as follows:
   * <pre>
   *    getCalibrators().add(newItem);
   * </pre>
   *
   *
   * <p>
   * Objects of the following type(s) are allowed in the list
   * {@link String }
   *
   *
   */
  public List<String> getCalibrators() {
    if (calibrators == null) {
      calibrators = new ArrayList<String>();
    }
    return this.calibrators;
  }

//--simple--preserve

  @Override
  public final String toString() {
    return "TargetInformation [" + ((this.name != null) ? this.name : "undefined") + "]" + " : " + getCalibrators();
  }

  /**
   * Return a deep "copy" of this instance
   * @return deep "copy" of this instance
   */
  @Override
  public final Object clone() {
    final TargetInformation copy = (TargetInformation) super.clone();

    // Deep copy of calibrators :
    final List<String> oldCalibrators = copy.getCalibrators();
    final List<String> newCalibrators = new ArrayList<String>(oldCalibrators.size());
    for (String cal : oldCalibrators) {
      newCalibrators.add(cal);
    }
    copy.calibrators = newCalibrators;

    return copy;
  }
//--simple--preserve
}
