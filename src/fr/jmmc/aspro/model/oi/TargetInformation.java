
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlList;
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
 *         &lt;element name="targetRef" type="{http://www.w3.org/2001/XMLSchema}IDREF"/>
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="calibrators" type="{http://www.w3.org/2001/XMLSchema}IDREFS" minOccurs="0"/>
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
    "targetRef",
    "description",
    "calibrators"
})
public class TargetInformation
    extends OIBase
{

    @XmlElement(required = true, type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected Target targetRef;
    protected String description;
    @XmlList
    @XmlElement(type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREFS")
    protected List<Target> calibrators;

    /**
     * Gets the value of the targetRef property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public Target getTargetRef() {
        return targetRef;
    }

    /**
     * Sets the value of the targetRef property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setTargetRef(Target value) {
        this.targetRef = value;
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
     * {@link Object }
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
    return "TargetInformation [" + ((this.getTargetRef() != null) ? this.getTargetRef() : "undefined") + "]" + " : " + getCalibrators();
  }

  /**
   * Return true if the calibrator list is not empty
   * @return true if the calibrator list is not empty
   */
  public final boolean hasCalibrators() {
    if (this.calibrators != null) {
      return !this.calibrators.isEmpty();
    }
    return false;
  }

  /**
   * Return true if the given target is a calibrator
   * i.e. the calibrator list contains the given target
   * @param target target to use
   * @return true if the given target is a calibrator
   */
  public final boolean isCalibrator(final Target target) {
    return getCalibrators().contains(target);
  }

  /**
   * Append the given target to the calibrator list if not present.
   * @param target calibrator to be added to the calibrator list, if absent
   * @return <tt>true</tt> if the calibrator was added
   */
  public boolean addCalibrator(final Target target) {
    if (!isCalibrator(target)) {
      return getCalibrators().add(target);
    }
    return false;
  }

  /**
   * Remove the given target to the calibrator list if present.
   * @param target calibrator to be removed from the calibrator list, if present
   * @return <tt>true</tt> if the calibrator was removed
   */
  public boolean removeCalibrator(final Target target) {
    if (isCalibrator(target)) {
      return getCalibrators().remove(target);
    }
    return false;
  }

  /**
   * Return a deep "copy" of this instance
   * @return deep "copy" of this instance
   */
  @Override
  public final Object clone() {
    final TargetInformation copy = (TargetInformation) super.clone();

    // note : targets are not cloned as only there (immutable) identifier is useful
    // see  : updateTargetReferences(Map<ID, Target>) to replace target instances to have a clean object graph
    // i.e. (no leaking references)

    // Simple copy of calibrators (Target instances) :
    if (copy.calibrators != null) {
      copy.calibrators = OIBase.copyList(copy.calibrators);
    }

    return copy;
  }

  /**
   * Check bad references and update target references in this instance using the given Map<ID, Target> index
   * @param mapIDTargets Map<ID, Target> index
   */
  protected final void updateTargetReferences(final java.util.Map<String, Target> mapIDTargets) {
    // note : targetRef is already updated in TargetUserInformations

    if (this.calibrators != null) {
      Target target, newTarget;

      for (final java.util.ListIterator<Target> it = this.calibrators.listIterator(); it.hasNext();) {
        target = it.next();

        newTarget = mapIDTargets.get(target.getIdentifier());
        if (newTarget != null) {
          if (newTarget != target) {
            it.set(newTarget);
          }
        } else {
          logger.info("Removing missing target reference '" + target.getIdentifier() + "'.");
          it.remove();
        }
      }
    }

  }
//--simple--preserve

}
