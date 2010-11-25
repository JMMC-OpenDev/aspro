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
 *         &lt;element name="calibrators" type="{http://www.w3.org/2001/XMLSchema}string" maxOccurs="unbounded" minOccurs="0"/>
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
        extends OIBase {

  protected List<String> calibrators;
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
   * Return the target user information corresponding to the target of the given name or create a new instance if the target is missing
   * @param name target name
   * @return target user information
   */
  public final TargetInformation getTargetUserInformation(final String name) {
    for (TargetInformation targetInfo : getTargetInfos()) {
      if (targetInfo.getName().equals(name)) {
        return targetInfo;
      }
    }
    // create a new instance if the target is not found :
    final TargetInformation targetInfo = new TargetInformation();
    targetInfo.setName(name);
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

    // Deep copy of calibrators :
    final List<String> oldCalibrators = copy.getCalibrators();
    final List<String> newCalibrators = new ArrayList<String>(oldCalibrators.size());
    for (String cal : oldCalibrators) {
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
