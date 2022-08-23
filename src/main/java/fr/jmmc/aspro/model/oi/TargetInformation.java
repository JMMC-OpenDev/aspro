
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
 *                 This type describes the list of calibrators associated to a target
 *             
 * 
 * <p>Java class for TargetInformation complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="TargetInformation"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="targetRef" type="{http://www.w3.org/2001/XMLSchema}IDREF"/&gt;
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="calibrators" type="{http://www.w3.org/2001/XMLSchema}IDREFS" minOccurs="0"/&gt;
 *         &lt;element name="groupMembers" type="{http://www.jmmc.fr/aspro-oi/0.1}TargetGroupMembers" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "TargetInformation", propOrder = {
    "targetRef",
    "description",
    "calibrators",
    "groupMembers"
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
    protected List<TargetGroupMembers> groupMembers;

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

    /**
     * Gets the value of the groupMembers property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the groupMembers property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getGroupMembers().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link TargetGroupMembers }
     * 
     * 
     */
    public List<TargetGroupMembers> getGroupMembers() {
        if (groupMembers == null) {
            groupMembers = new ArrayList<TargetGroupMembers>();
        }
        return this.groupMembers;
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
        return !isEmpty(this.calibrators);
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

        // note : targetRef is not cloned as only there (immutable) identifier is useful
        // see  : updateTargetReferences(Map<ID, Target>) to replace target instances to have a clean object graph
        // i.e. (no leaking references)
        
        // Simple copy of calibrators (Target instances) :
        if (copy.calibrators != null) {
            copy.calibrators = OIBase.copyList(copy.calibrators);
        }

        // Deep copy of target group members :
        if (copy.groupMembers != null) {
            copy.groupMembers = OIBase.deepCopyList(copy.groupMembers);
        }        

        return copy;
    }

    @Override
    protected boolean areEquals(final OIBase o) {
        if (!super.areEquals(o)) {
            return false;
        }
        final TargetInformation other = (TargetInformation) o;
        return (areEqualsStrict(this.targetRef, other.getTargetRef()) // just identifiers
                && areEquals(this.description, other.getDescription())
                && areEqualsStrict(getCalibrators(), other.getCalibrators()) // just identifiers, may create lists
                && areEquals(getGroupMembers(), other.getGroupMembers()) // may create lists
                );
    }

    protected static void updateTargetReferences(final java.util.List<TargetInformation> targetInfos,
                                                 final java.util.Map<String, Target> mapIDTargets,
                                                 final java.util.Map<String, TargetGroup> mapIDGroups,
                                                 final java.util.Map<String, Target> mapIDCalibrators,
                                                 final java.util.Map<String, java.util.Map<String, Target>> mapIDGroupMembers) {

        TargetInformation targetInfo;
        Target target, newTarget;

        for (final java.util.ListIterator<TargetInformation> it = targetInfos.listIterator(); it.hasNext();) {
            targetInfo = it.next();

            target = targetInfo.getTargetRef();

            if (target == null) {
                logger.debug("Removing invalid target reference.");
                it.remove();
            } else {
                newTarget = mapIDTargets.get(target.getIdentifier());
                if (newTarget != null) {
                    if (newTarget != target) {
                        targetInfo.setTargetRef(newTarget);
                    }

                    targetInfo.updateTargetReferences(mapIDCalibrators);

                    targetInfo.updateGroupReferences(mapIDTargets, mapIDGroups, mapIDGroupMembers);

                    // remove if empty :
                    if (targetInfo.isEmpty()) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("Removing empty target information: {}", target.getIdentifier());
                        }
                        it.remove();
                    }

                } else {
                    if (logger.isDebugEnabled()) {
                        logger.debug("Removing missing target reference: {}", target.getIdentifier());
                    }
                    it.remove();
                }
            }
        }
    }
    
    
    /**
     * Check bad references and update target references 
     * and check if referenced calibrators are present in the given mapIDCalibrators
     * @param mapIDCalibrators Map<ID, Target> index
     */
    protected final void updateTargetReferences(final java.util.Map<String, Target> mapIDCalibrators) {
        // note : targetRef is already updated in TargetUserInformations

        if (this.description != null) {
            this.description = this.description.trim();
            if (this.description.length() == 0) {
                this.description = null;
            }
        }

        if (this.calibrators != null) {
            Target.updateTargetReferences(this.calibrators, mapIDCalibrators);

            if (this.calibrators.isEmpty()) {
                this.calibrators = null;
            }
        }
    }
    
    /**
     * Check bad references and update group and target references in this instance using the given Map indexes
     * @param mapIDGroups Map<ID, TargetGroup> index
     * @param mapIDTargets Map<ID, Target> index
     * @param mapIDGroupMembers Map<ID TargetGroup, Map<ID, Target> > index (optional)
     */
    protected final void updateGroupReferences(final java.util.Map<String, Target> mapIDTargets,
                                               final java.util.Map<String, TargetGroup> mapIDGroups,
                                               final java.util.Map<String, java.util.Map<String, Target>> mapIDGroupMembers) {
        if (this.groupMembers != null) {
            // fix group ref & target refs in group members:
            TargetGroupMembers.updateGroupReferences(this.groupMembers, mapIDTargets, mapIDGroups, mapIDGroupMembers);
            
            if (this.groupMembers.isEmpty()) {
                this.groupMembers = null;
            }
        }
    }
    
    /**
     * Return true if this target information is empty :
     * description is empty and calibrator list is empty
     * @return true if this target information is empty
     */
    protected final boolean isEmpty() {
        if (!isEmpty(this.description)) {
            return false;
        }
        if (!isEmpty(this.calibrators)) {
            return false;
        }
        return isEmpty(this.groupMembers);
    }

    /**
     * Return the existing TargetInformation corresponding to the target
     * @param target target to look up
     * @param targetInfos list of TargetInformation instances
     * @return TargetInformation or null
     * @see #getOrCreateTargetInformation(target)
     */
    protected static final TargetInformation getTargetInformation(final Target target,
                                                                  final java.util.List<TargetInformation> targetInfos) {
        for (int i = 0, len = targetInfos.size(); i < len; i++) {
            TargetInformation targetInfo = targetInfos.get(i);
            if (targetInfo.getTargetRef().equals(target)) {
                return targetInfo;
            }
        }
        return null;
    }      

    /**
     * Return the TargetGroupMembers corresponding to the group
     * or create a new instance if the TargetGroupMembers is missing
     * @param group group
     * @return TargetGroupMembers
     */
    public final TargetGroupMembers getOrCreateGroupMembers(final TargetGroup group) {
        if (group == null) {
            throw new NullPointerException("Group can not be null !");
        }        
        TargetGroupMembers gm = getGroupMembers(group);
        if (gm == null) {
            // create a new instance if the group is not found :
            gm = new TargetGroupMembers();
            gm.setGroupRef(group);
            getGroupMembers().add(gm);
        }
        return gm;
    }

    public final boolean hasGroupMembers() {
        for (TargetGroupMembers tgm : getGroupMembers()) {
            if (!tgm.isEmpty()) {
                return true;
            }
        }
        return false;
    }
    
    public final void fillGroupsHavingTargetInAnyGroupMembers(final Target target, final java.util.Set<TargetGroup> usedGroups) {
        for (TargetGroupMembers tgm : getGroupMembers()) {
            if (tgm.hasTarget(target)) {
                usedGroups.add(tgm.getGroupRef());
            }
        }
    }

    public final boolean hasTargetInGroupMembers(final TargetGroup group, final Target target) {
        final TargetGroupMembers gm = getOrCreateGroupMembers(group);
        return gm.hasTarget(target);
    }
    
    public final boolean addTargetInGroupMembers(final TargetGroup group, final Target target) {
        final TargetGroupMembers gm = getOrCreateGroupMembers(group);
        return gm.addTarget(target); // avoid duplicates
    }
    
    public final boolean removeTargetInGroupMembers(final TargetGroup group, final Target target) {
        final TargetGroupMembers gm = getGroupMembers(group);
        if (gm != null) {
            return gm.removeTarget(target);
        }
        return false;
    }
    
    /**
     * Return the existing TargetGroupMembers corresponding to the group
     * @param group group to look up
     * @return TargetGroupMembers or null
     * @see #getOrCreateTargetGroupMembers(group)
     */
    public final TargetGroupMembers getGroupMembers(final TargetGroup group) {
        return TargetGroupMembers.getGroupMembers(group, getGroupMembers());
    }  
//--simple--preserve

}
