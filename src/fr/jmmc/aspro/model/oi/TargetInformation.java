
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlSchemaType;
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
 *         &lt;element name="target" type="{http://www.w3.org/2001/XMLSchema}IDREF"/>
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="calibrators" type="{http://www.w3.org/2001/XMLSchema}IDREFS" maxOccurs="unbounded" minOccurs="0"/>
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
    "target",
    "description",
    "calibrators"
})
public class TargetInformation
    extends OIBase
{

    @XmlElement(required = true, type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected Target target;
    protected String description;
    @XmlElementRef(name = "calibrators", type = JAXBElement.class)
    protected List<Target> calibrators;

    /**
     * Gets the value of the target property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public Target getTarget() {
        return target;
    }

    /**
     * Sets the value of the target property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setTarget(Target value) {
        this.target = value;
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
     * {@link JAXBElement }{@code <}{@link List }{@code <}{@link Object }{@code >}{@code >}
     * 
     * 
     */
    public List<Target> getCalibrators() {
        if (calibrators == null) {
            calibrators = new ArrayList<Target>();
        }
        return this.calibrators;
    }
    
//--simple--preserve

  @Override
  public final String toString() {
    return "TargetInformation [" + ((this.getTarget() != null) ? this.getTarget() : "undefined") + "]" + " : " + getCalibrators();
  }

  /**
   * Return a deep "copy" of this instance
   * @return deep "copy" of this instance
   */
  @Override
  public final Object clone() {
    final TargetInformation copy = (TargetInformation) super.clone();

    // note : targets are not cloned again as only there (immutable) identifier is needed

    // Deep copy of calibrators :
    final List<Target> oldCalibrators = copy.getCalibrators();
    final List<Target> newCalibrators = new ArrayList<Target>(oldCalibrators.size());
    for (Target cal : oldCalibrators) {
      newCalibrators.add(cal);
    }
    copy.calibrators = newCalibrators;

    return copy;
  }
//--simple--preserve

}
