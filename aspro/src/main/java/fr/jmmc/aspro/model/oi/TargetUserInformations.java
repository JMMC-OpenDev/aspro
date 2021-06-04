
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
 *                 This type describes the user information related to all targets.
 *             
 * 
 * <p>Java class for TargetUserInformations complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="TargetUserInformations"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="calibrators" type="{http://www.w3.org/2001/XMLSchema}IDREFS" minOccurs="0"/&gt;
 *         &lt;element name="group" type="{http://www.jmmc.fr/aspro-oi/0.1}TargetGroup" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="groupMembers" type="{http://www.jmmc.fr/aspro-oi/0.1}TargetGroupMembers" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="targetInfo" type="{http://www.jmmc.fr/aspro-oi/0.1}TargetInformation" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "TargetUserInformations", propOrder = {
    "calibrators",
    "groups",
    "groupMembers",
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
    @XmlElement(name = "group")
    protected List<TargetGroup> groups;
    protected List<TargetGroupMembers> groupMembers;
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
     * Gets the value of the groups property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the groups property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getGroups().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link TargetGroup }
     * 
     * 
     */
    public List<TargetGroup> getGroups() {
        if (groups == null) {
            groups = new ArrayList<TargetGroup>();
        }
        return this.groups;
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
     * Return the calibrator list as a Set
     * @return calibrator list as a Set
     */
    public final java.util.Set<Target> getCalibratorSet() {
        final List<Target> calibratorList = getCalibrators();
        if (calibratorList.isEmpty()) {
            return java.util.Collections.emptySet();
        }
        final java.util.Set<Target> calibratorSet = new java.util.HashSet<Target>(calibratorList.size());
        calibratorSet.addAll(calibratorList);
        return calibratorSet;
    }

    /**
     * Return the display name of the given target using the format 'name' ( ' (cal)')
     * @param target target to use
     * @param sb string buffer to append into
     * @return display name
     */
    public final StringBuilder getTargetDisplayName(final Target target, final StringBuilder sb) {
        sb.append(target.getName());

        if (isCalibrator(target)) {
            sb.append(fr.jmmc.aspro.AsproConstants.CAL_SUFFIX);
        }
        return sb;
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
     * Remove the given target from the calibrator list if present.
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
     * Return the target user information corresponding to the target
     * or create a new instance if the TargetInformation is missing
     * @param target target
     * @return target user information
     */
    public final TargetInformation getOrCreateTargetInformation(final Target target) {
        if (target == null) {
            throw new NullPointerException("Target can not be null !");
        }
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
     * @return target user information or null
     * @see #getOrCreateTargetInformation(Target)
     */
    public final TargetInformation getTargetInformation(final Target target) {
        return TargetInformation.getTargetInformation(target, getTargetInfos());
    }

    /**
     * Return the user target description or null if undefined
     * @param target target to use
     * @return user target description or null if undefined
     */
    public final String getDescription(final Target target) {
        final TargetInformation targetInfo = getTargetInformation(target);
        if (targetInfo != null) {
            final String description = targetInfo.getDescription();
            if (description != null && description.length() != 0) {
                return description;
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
     * Return the calibrator list for the given target
     * @param target target to use
     * @return calibrator list
     */
    public final List<Target> getCalibrators(final Target target) {
        final TargetInformation targetInfo = getOrCreateTargetInformation(target);

        return targetInfo.getCalibrators();
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

        // Deep copy of target groups :
        if (copy.groups != null) {
            copy.groups = OIBase.deepCopyList(copy.groups);
        }

        // Deep copy of target group members :
        if (copy.groupMembers != null) {
            copy.groupMembers = OIBase.deepCopyList(copy.groupMembers);
        }

        // Deep copy of target informations :
        if (copy.targetInfos != null) {
            copy.targetInfos = OIBase.deepCopyList(copy.targetInfos);
        }

        return copy;
    }

    @Override
    protected boolean areEquals(final OIBase o) {
        if (!super.areEquals(o)) {
            return false;
        }
        final TargetUserInformations other = (TargetUserInformations) o;
        return (areEqualsStrict(getCalibrators(), other.getCalibrators()) // just identifiers, may create lists
                && areEquals(getGroups(), other.getGroups()) // may create lists
                && areEquals(getGroupMembers(), other.getGroupMembers()) // may create lists
                && areEquals(getTargetInfos(), other.getTargetInfos()) // may create lists
                );
    }

    /**
     * Check bad references and update target references in this instance using the given Map<ID, Target> index
     * @param mapIDTargets Map<ID, Target> index
     */
    protected final void updateTargetReferences(final java.util.Map<String, Target> mapIDTargets) {
        // create the Map<ID, TargetGroup> index for groups:
        final java.util.Map<String, TargetGroup> mapIDGroups = TargetGroup.createTargetGroupIndex(getGroups());

        if (this.groupMembers != null) {
            // Fix group ref & target refs in group members:
            TargetGroupMembers.updateGroupReferences(this.groupMembers, mapIDTargets, mapIDGroups, null);

            if (this.groupMembers.isEmpty()) {
                this.groupMembers = null;
            }
        }

        if (this.calibrators != null) {
            // Fix target refs in calibrators:
            Target.updateTargetReferences(this.calibrators, mapIDTargets);

            if (this.calibrators.isEmpty()) {
                this.calibrators = null;
            }
        }

        if (this.targetInfos != null) {
            // create the Map<ID, Target> index for calibrators:
            final java.util.Map<String, Target> mapIDCalibrators = Target.createTargetIndex(this.calibrators);

            // create the Map<ID TargetGroup, Map<ID, Target> > index for group members:
            final java.util.Map<String, java.util.Map<String, Target>> mapIDGroupMembers
                                                                       = TargetGroupMembers.createGroupMemberIndex(this.groupMembers);

            // Fix target refs in calibrators & group members:
            TargetInformation.updateTargetReferences(this.targetInfos, mapIDTargets, mapIDGroups, mapIDCalibrators, mapIDGroupMembers);

            if (this.targetInfos.isEmpty()) {
                this.targetInfos = null;
            }
        }
    }

    /**
     * Return true if this target user informations is empty :
     * calibrators, groups & members, target info list are all empty
     * @return true if this target user informations is empty
     */
    protected final boolean isEmpty() {
        if (!isEmpty(this.calibrators)) {
            return false;
        }
        if (!isEmpty(this.groups)) {
            return false;
        }
        if (!isEmpty(this.groupMembers)) {
            return false;
        }
        return isEmpty(this.targetInfos);
    }

    /** computed displayable list of target groups (read only) */
    @javax.xml.bind.annotation.XmlTransient
    private List<TargetGroup> cachedDisplayTargetGroups = null;

    /**
     * Clear any cached value related to target groups
     */
    public void clearCacheGroups() {
        cachedDisplayTargetGroups = null;
    }

    /**
     * Return the displayable list of targets containing
     * - science targets followed by their calibrators
     * - calibrator orphans
     * @return displayable list of targets
     */
    public List<TargetGroup> getDisplayGroups() {
        if (this.cachedDisplayTargetGroups != null) {
            return this.cachedDisplayTargetGroups;
        }
        computeDisplayTargetGroups();

        return this.cachedDisplayTargetGroups;
    }

    /**
     * Compute the displayable list of target groups
     */
    private void computeDisplayTargetGroups() {
        final List<TargetGroup> innerGroups = getGroups();

        final List<TargetGroup> displayGroups;

        if (innerGroups.isEmpty()) {
            displayGroups = java.util.Collections.emptyList();
        } else {
            displayGroups = new ArrayList<TargetGroup>(innerGroups);
            
            java.util.Collections.sort(displayGroups, fr.jmmc.aspro.model.util.TargetGroupComparator.getInstance());
        }

        // cache the computed collections :
        this.cachedDisplayTargetGroups = displayGroups;
    }
    
    /**
     * Return the group of the given identifier in the given list of groups
     * @param id group identifier
     * @return group or null if the group was not found
     */
    public final TargetGroup getGroupById(final String id) {
        return TargetGroup.getGroupById(id, getGroups());
    }

    /**
     * Return the group of the given name
     * @param name group name
     * @return group or null if the group was not found
     */
    public final TargetGroup getGroupByName(final String name) {
        return TargetGroup.getGroup(name, getGroups());
    }

    public void addGroup(final TargetGroup group) {
        final TargetGroup match = getGroupById(group.getIdentifier());
        if (match != null) {
            throw new IllegalStateException("TargetGroup[" + group.getIdentifier() + "] already present !");
        }
        getGroups().add(group);
    }

    public boolean removeGroup(final TargetGroup group) {
        if (group != null && !group.isCategoryOB()) {
            // check if used ?
            TargetGroupMembers tgm = getGroupMembers(group);
            if (tgm == null || tgm.isEmpty()) {
                // can remove empty group:
                if (tgm != null) {
                    getGroupMembers().remove(tgm);
                }
                getGroups().remove(group);
                return true;
            }
        }
        return false;
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

    public final java.util.Set<TargetGroup> getGroupsUsedByTargetGroupMembers(final Target target) {
        final java.util.Set<TargetGroup> usedGroups = new java.util.HashSet<TargetGroup>();

        for (TargetInformation targetInfo : getTargetInfos()) {
            targetInfo.fillGroupsHavingTargetInAnyGroupMembers(target, usedGroups);

        }
        return usedGroups;
    }

    public final TargetGroup getFirstTargetGroup(final Target target,
                                                 final List<TargetGroup> preferedGroupList) {
        if (preferedGroupList != null) {
            for (TargetGroup group : preferedGroupList) {
                final TargetGroupMembers gm = getGroupMembers(group);

                if (gm != null && gm.hasTarget(target)) {
                    return group;
                }
            }
        }
        for (TargetGroup group : getGroups()) {
            final TargetGroupMembers gm = getGroupMembers(group);

            if (gm != null && gm.hasTarget(target)) {
                return group;
            }
        }
        return null;
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

    public boolean hasTargetInTargetGroup(final String groupId, final Target target) {
        final TargetGroup g = getGroupById(groupId);
        if (g != null) {
            return hasTargetInTargetGroup(g, target);
        }
        return false;
    }

    public final boolean hasTargetInTargetGroup(final TargetGroup group, final Target target) {
        TargetGroupMembers gm = getGroupMembers(group);
        if (gm != null) {
            return gm.hasTarget(target);
        }
        return false;
    }

    public final boolean addTargetToTargetGroup(final TargetGroup group, final Target target) {
        return getOrCreateGroupMembers(group).addTarget(target);
    }

    public final boolean removeTargetFromTargetGroup(final TargetGroup group, final Target target) {
        if (getOrCreateGroupMembers(group).removeTarget(target)) {
            // remove target links:
            return removeTargetFromTargetGroupMembers(group, target);
        }
        return false;
    }

    public final boolean hasTargetInTargetGroupMembers(final TargetGroup group, final Target target) {
        for (TargetInformation targetInfo : getTargetInfos()) {
            return targetInfo.hasTargetInGroupMembers(group, target);
        }
        return false;
    }

    public final boolean removeTargetFromTargetGroupMembers(final TargetGroup group, final Target target) {
        boolean removed = false;
        for (TargetInformation targetInfo : getTargetInfos()) {
            removed |= targetInfo.removeTargetInGroupMembers(group, target);
        }
        return removed;
    }

//--simple--preserve

}
