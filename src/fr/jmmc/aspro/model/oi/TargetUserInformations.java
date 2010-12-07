
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
 *         &lt;element name="calibrators" type="{http://www.w3.org/2001/XMLSchema}IDREFS" minOccurs="0"/>
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

    @XmlList
    @XmlElement(type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREFS")
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
   * Return true if the given target is a calibrator 
   * i.e. the calibrator list contains the given target 
   * @param target target to use
   * @return true if the given target is a calibrator 
   */
  public final boolean isCalibrator(final Target target) {
    return getCalibrators().contains(target);
  }

  /**
   * Return the display name of the given target using the format 'name' ( ' (cal)')
   * @param target target to use
   * @return display name
   */
  public final String getTargetDisplayName(final Target target) {

    String sValue = target.getName();

    if (isCalibrator(target)) {
      sValue += fr.jmmc.aspro.AsproConstants.CAL_SUFFIX;
    }
    return sValue;
  }

  /**
   * Append the given target to the calibrator list if not present.
   * @param target calibrator to be added to the calibrator list, if absent
   * @return <tt>true</tt> if the calibrator was added
   */
  public boolean addCalibratorIfAbsent(final Target target) {
    if (!isCalibrator(target)) {
      return getCalibrators().add(target);
    }
    return false;
  }

  /**
   * Return the target user information corresponding to the target
   * or create a new instance if the target is missing
   * @param target target
   * @return target user information
   */
  public final TargetInformation getOrCreateTargetInformation(final Target target) {
    TargetInformation targetInfo = getTargetInformation(target);
    if (targetInfo == null) {
      // create a new instance if the target is not found :
      targetInfo = new TargetInformation();
      targetInfo.setTargetRef(target);
      getTargetInfos().add(targetInfo);
    }
    return targetInfo;
  }

  /**
   * Return the existing target information corresponding to the target
   * @param target target
   * @return target user information
   * @see #getOrCreateTargetInformation(Target)
   */
  public final TargetInformation getTargetInformation(final Target target) {
    for (TargetInformation targetInfo : getTargetInfos()) {
      if (targetInfo.getTargetRef().equals(target)) {
        return targetInfo;
      }
    }
    return null;
  }

  /**
   * Return true if the given target has calibrators
   * @param target target to use
   * @return true if the given target has calibrators
   */
  public final boolean hasCalibrators(final Target target) {
    final TargetInformation targetInfo = getTargetInformation(target);
    if (targetInfo != null) {
      return targetInfo.hasCalibrators();
    }
    return false;
  }

  /**
   * Add the given calibrator target to the given science target
   * @param target science target to use
   * @param calibrator calibrator target
   * @return true if the calibrator was added
   */
  public final boolean addCalibratorToTarget(final Target target, final Target calibrator) {
    // check that the target is a science target :
    if (!isCalibrator(target) && isCalibrator(calibrator)) {
      final TargetInformation targetInfo = getOrCreateTargetInformation(target);

      return targetInfo.addCalibrator(calibrator);
    }
    return false;
  }

  /**
   * Remove the given calibrator target to the given science target
   * @param target science target to use
   * @param calibrator calibrator target
   * @return true if the calibrator was removed
   */
  public final boolean removeCalibratorFromTarget(final Target target, final Target calibrator) {
    // check that the target is a science target :
    if (!isCalibrator(target) && isCalibrator(calibrator)) {
      final TargetInformation targetInfo = getOrCreateTargetInformation(target);

      return targetInfo.removeCalibrator(calibrator);
    }
    return false;
  }

  /**
   * Return a deep "copy" of this instance
   * @return deep "copy" of this instance
   */
  @Override
  public final Object clone() {
    final TargetUserInformations copy = (TargetUserInformations) super.clone();

    // note : targets are not cloned as only there (immutable) identifier is useful
    // see  : updateTargetReferences(Map<ID, Target>) to replace target instances to have a clean object graph
    // i.e. (no leaking references)

    // Simple copy of calibrators (Target instances) :
    if (copy.calibrators != null) {
      copy.calibrators = OIBase.copyList(copy.calibrators);
    }

    // Deep copy of target informations :
    if (copy.targetInfos != null) {
      copy.targetInfos = OIBase.deepCopyList(copy.targetInfos);
    }

    return copy;
  }

  /**
   * Check bad references and update target references in this instance using the given Map<ID, Target> index
   * @param mapIDTargets Map<ID, Target> index
   */
  protected final void updateTargetReferences(final java.util.Map<String, Target> mapIDTargets) {

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

    if (this.targetInfos != null) {
      TargetInformation targetInfo;
      Target target, newTarget;

      for (final java.util.ListIterator<TargetInformation> it = this.targetInfos.listIterator(); it.hasNext();) {
        targetInfo = it.next();

        target = targetInfo.getTargetRef();

        if (target == null) {
          logger.info("Removing invalid target reference.");
          it.remove();
        } else {
          newTarget = mapIDTargets.get(target.getIdentifier());
          if (newTarget != null) {
            if (newTarget != target) {
              targetInfo.setTargetRef(newTarget);
            }

            targetInfo.updateTargetReferences(mapIDTargets);

            // remove if empty :
            if (targetInfo.isEmpty()) {
              if (logger.isLoggable(java.util.logging.Level.FINE)) {
                logger.fine("Removing empty target information '" + target.getIdentifier() + "'.");
              }
              it.remove();
            }

          } else {
            logger.info("Removing missing target reference '" + target.getIdentifier() + "'.");
            it.remove();
          }
        }
      }
    }
  }
//--simple--preserve

}
