
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes the user information related to all targets.
 *       
 * 
 * <p>Java class for TargetUserInformations complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="TargetUserInformations">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="calibrators" type="{http://www.w3.org/2001/XMLSchema}IDREFS" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="targetInfo" type="{http://www.jmmc.fr/aspro-oi/0.1}TargetInformation" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "TargetUserInformations", propOrder = {
    "calibrators",
    "targetInfos"
})
public class TargetUserInformations
    extends OIBase
{

    @XmlElementRef(name = "calibrators", type = JAXBElement.class)
    protected List<Target> calibrators;
    @XmlElement(name = "targetInfo")
    protected List<TargetInformation> targetInfos;

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

    /**
     * Gets the value of the targetInfos property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the targetInfos property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getTargetInfos().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link TargetInformation }
     * 
     * 
     */
    public List<TargetInformation> getTargetInfos() {
        if (targetInfos == null) {
            targetInfos = new ArrayList<TargetInformation>();
        }
        return this.targetInfos;
    }
    
//--simple--preserve
  @Override
  public final String toString() {
    return "TargetUserInformations : \ncalibrators : " + getCalibrators() + "\ntargets : " + getTargetInfos();
  }

  /**
   * Return the target user information corresponding to the target
   * or create a new instance if the target is missing
   * @param target target
   * @return target user information
   */
  public final TargetInformation getTargetUserInformation(final Target target) {
    for (TargetInformation targetInfo : getTargetInfos()) {
      if (targetInfo.getTarget().equals(target)) {
        return targetInfo;
      }
    }
    // create a new instance if the target is not found :
    final TargetInformation targetInfo = new TargetInformation();
    targetInfo.setTarget(target);
    getTargetInfos().add(targetInfo);
    return targetInfo;
  }

  /*
   TODO : prune TargetInformation orphans
   */

  /**
   * Return a deep "copy" of this instance
   * @return deep "copy" of this instance
   */
  @Override
  public final Object clone() {
    final TargetUserInformations copy = (TargetUserInformations) super.clone();

    // note : targets are not cloned again as only there (immutable) identifier is needed

    // Deep copy of calibrators :
    final List<Target> oldCalibrators = copy.getCalibrators();
    final List<Target> newCalibrators = new ArrayList<Target>(oldCalibrators.size());
    for (Target cal : oldCalibrators) {
      newCalibrators.add(cal);
    }
    copy.calibrators = newCalibrators;

    // Deep copy of target informations :
    final List<TargetInformation> oldTargetInfos = copy.getTargetInfos();
    final List<TargetInformation> newTargetInfos = new ArrayList<TargetInformation>(oldTargetInfos.size());
    for (TargetInformation info : oldTargetInfos) {
      newTargetInfos.add((TargetInformation) info.clone());
    }
    copy.targetInfos = newTargetInfos;

    return copy;
  }
//--simple--preserve

}
