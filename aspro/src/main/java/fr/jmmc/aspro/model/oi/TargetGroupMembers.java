
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
 *                 This type describes the list of targets for the associated target group.
 *             
 * 
 * <p>Java class for TargetGroupMembers complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="TargetGroupMembers"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="groupRef" type="{http://www.w3.org/2001/XMLSchema}IDREF"/&gt;
 *         &lt;element name="targets" type="{http://www.w3.org/2001/XMLSchema}IDREFS" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "TargetGroupMembers", propOrder = {
    "groupRef",
    "targets"
})
public class TargetGroupMembers
    extends OIBase
{

    @XmlElement(required = true, type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected TargetGroup groupRef;
    @XmlList
    @XmlElement(type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREFS")
    protected List<Target> targets;

    /**
     * Gets the value of the groupRef property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public TargetGroup getGroupRef() {
        return groupRef;
    }

    /**
     * Sets the value of the groupRef property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setGroupRef(TargetGroup value) {
        this.groupRef = value;
    }

    /**
     * Gets the value of the targets property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the targets property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getTargets().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Object }
     * 
     * 
     */
    public List<Target> getTargets() {
        if (targets == null) {
            targets = new ArrayList<Target>();
        }
        return this.targets;
    }
    
//--simple--preserve

    @Override
    public final String toString() {
        return "TargetGroupMembers [" + ((this.getGroupRef() != null) ? this.getGroupRef() : "undefined") + "]" + " : " + getTargets();
    }

    /**
     * Return a deep "copy" of this instance
     * @return deep "copy" of this instance
     */
    @Override
    public final Object clone() {
        final TargetGroupMembers copy = (TargetGroupMembers) super.clone();

        // note : groups & targets are not cloned as only there (immutable) identifier is useful
        // see  : updateTargetReferences(Map<ID, Target>) to replace target instances to have a clean object graph
        // i.e. (no leaking references)
        
        // Simple copy of targets (Target instances) :
        if (copy.targets != null) {
            copy.targets = OIBase.copyList(copy.targets);
        }

        return copy;
    }

    @Override
    protected boolean areEquals(final OIBase o) {
        if (!super.areEquals(o)) {
            return false;
        }
        final TargetGroupMembers other = (TargetGroupMembers) o;
        return (areEqualsStrict(this.groupRef, other.getGroupRef()) // just identifiers
                && areEqualsStrict(getTargets(), other.getTargets()) // just identifiers, may create lists
                );
    }

    /**
     * Return the Map<ID TargetGroup, Map<ID, Target> > index
     * @param groupMembers
     * @return Map<ID TargetGroup, Map<ID, Target> > index
     */
    static java.util.Map<String, java.util.Map<String, Target>> createGroupMemberIndex(final List<TargetGroupMembers> groupMembers) {
        // create the Map<ID, Target> index:
        if (groupMembers == null) {
            return java.util.Collections.emptyMap();
        }
        final java.util.Map<String, java.util.Map<String, Target>> mapIDGroupMembers
            = new java.util.HashMap<String, java.util.Map<String, Target>>(groupMembers.size());
        
        for (TargetGroupMembers tgm : groupMembers) {
            // create the Map<ID, Target> index for members:
            mapIDGroupMembers.put(tgm.getGroupRef().getIdentifier(), Target.createTargetIndex(tgm.targets));
        }
        return mapIDGroupMembers;
    }
    
    protected static void updateGroupReferences(final List<TargetGroupMembers> groupMembers,
                                                final java.util.Map<String, Target> mapIDTargets,
                                                final java.util.Map<String, TargetGroup> mapIDGroups,
                                                final java.util.Map<String, java.util.Map<String, Target>> mapIDGroupMembers) {

        java.util.Map<String, Target> index;
        TargetGroup group, newGroup;
        
        for (final java.util.ListIterator<TargetGroupMembers> it = groupMembers.listIterator(); it.hasNext();) {
            TargetGroupMembers tgm = it.next();

            group = tgm.getGroupRef();

            if (group == null) {
                logger.info("Removing invalid group reference.");
                it.remove();
            } else {
                newGroup = mapIDGroups.get(group.getIdentifier());
                if (newGroup != null) {
                    if (newGroup != group) {
                        tgm.setGroupRef(newGroup);
                    }
                    
                    // consider only target references corresponding to the group members or all target references:
                    index = (mapIDGroupMembers != null) ? mapIDGroupMembers.get(group.getIdentifier()) : mapIDTargets;

                    tgm.updateTargetReferences(index);

                    // remove if empty :
                    if (tgm.isEmpty()) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("Removing empty group member: {}", group.getIdentifier());
                        }
                        it.remove();
                    }

                } else {
                    logger.info("Removing missing group reference: {}", group.getIdentifier());
                    it.remove();
                }
            }
        }
    }
    
    /**
     * Check bad references and update target references in this instance using the given Map<ID, Target> index
     * @param mapIDTargets Map<ID, Target> index
     */
    private final void updateTargetReferences(final java.util.Map<String, Target> mapIDTargets) {
        // note : groupRef is already updated in updateGroupReferences()
        
        if (this.targets != null) {
            Target.updateTargetReferences(this.targets, mapIDTargets);
            
            if (this.targets.isEmpty()) {
                this.targets = null;
            }
        }
    }

    /**
     * Return true if this group members is empty :
     * target list is empty
     * @return true if this group members is empty
     */
    public final boolean isEmpty() {
        return isEmpty(this.targets);
    }

    /**
     * Return the existing TargetGroupMembers corresponding to the group
     * @param group group to look up
     * @param groupMembers list of TargetGroupMembers instances
     * @return TargetGroupMembers or null
     * @see #getOrCreateTargetGroupMembers(group)
     */
    protected static final TargetGroupMembers getGroupMembers(final TargetGroup group,
                                                              final java.util.List<TargetGroupMembers> groupMembers) {
        for (int i = 0, len = groupMembers.size(); i < len; i++) {
            TargetGroupMembers gm = groupMembers.get(i);
            if (gm.getGroupRef().equals(group)) {
                return gm;
            }
        }
        return null;
    }
    
    /**
     * Return true if the given target is present in the group members
     * @param target target to test
     * @return true if the given target is present
     */
    public boolean hasTarget(final Target target) {
        return getTargets().contains(target);
    }
    
    /**
     * Add the given target to the group members if not present (avoid duplicates)
     * @param target target to add
     * @return true if the given target was added
     */
    public boolean addTarget(final Target target) {
        if (!hasTarget(target)) {
            return getTargets().add(target);
        }
        return false;
    }

    /**
     * Remove the given target from the group members if present
     * @param target target to remove
     * @return true if the given target was removed
     */
    public boolean removeTarget(final Target target) {
        return getTargets().remove(target);
    }

//--simple--preserve

}
