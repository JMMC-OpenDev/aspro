
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;
import fr.jmmc.aspro.model.rawobs.RawObservation;


/**
 * 
 *                 This type describes the list of raw observations associated to a target
 *             
 * 
 * <p>Java class for TargetRawObservation complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="TargetRawObservation"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="targetRef" type="{http://www.w3.org/2001/XMLSchema}IDREF"/&gt;
 *         &lt;element name="observation" type="{http://www.jmmc.fr/aspro-raw-obs/0.1}RawObservation" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "TargetRawObservation", propOrder = {
    "targetRef",
    "observations"
})
public class TargetRawObservation
    extends OIBase
{

    @XmlElement(required = true, type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected Target targetRef;
    @XmlElement(name = "observation")
    protected List<RawObservation> observations;

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
     * Gets the value of the observations property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the observations property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getObservations().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link RawObservation }
     * 
     * 
     */
    public List<RawObservation> getObservations() {
        if (observations == null) {
            observations = new ArrayList<RawObservation>();
        }
        return this.observations;
    }
    
//--simple--preserve
    
    public final void setObservations(final List<RawObservation> newObservations) {
        final List<RawObservation> obsList = getObservations();
        obsList.clear();
        obsList.addAll(newObservations);
        // reset the preparation flag:
        prepared = false;
    }

    /** preparation flag (read only) */
    @javax.xml.bind.annotation.XmlTransient
    boolean prepared = false;

    /** preparation flag (read only) */
    @javax.xml.bind.annotation.XmlTransient
    List<fr.jmmc.aspro.model.rawobs.Observations> obsGroups = null;
    
    /**
     * Return the observations as Observation Blocks
     * @return observations as Observation Blocks
     */
    public final List<fr.jmmc.aspro.model.rawobs.Observations> getGroups() {
        return this.obsGroups;
    }
    
    public final void prepare() {
        if (prepared) {
            return;
        }
        prepared = true;
        
        // Perform preparation and analysis:
        this.obsGroups = fr.jmmc.aspro.model.RawObsManager.getInstance().analyze(getTargetRef().getIdentifier(), getObservations());
    }
    
    @Override
    public final String toString() {
        return "TargetRawObservation [" + ((this.getTargetRef() != null) ? this.getTargetRef() : "undefined") + "]" + " : " + getObservations();
    }

    /**
     * Return a deep "copy" of this instance
     * @return deep "copy" of this instance
     */
    @Override
    public final Object clone() {
        final TargetRawObservation copy = (TargetRawObservation) super.clone();

        // note : targetRef is not cloned as only there (immutable) identifier is useful
        // see  : updateTargetReferences(Map<ID, Target>) to replace target instances to have a clean object graph
        // i.e. (no leaking references)
        
        // Simple copy of observations:
        if (copy.observations != null) {
            copy.observations = OIBase.copyList(copy.observations);
        }

        return copy;
    }

    @Override
    protected boolean areEquals(final OIBase o) {
        if (!super.areEquals(o)) {
            return false;
        }
        final TargetRawObservation other = (TargetRawObservation) o;
        return (areEqualsStrict(getObservations(), other.getObservations()));
    }

    /**
     * Check bad references and update target references in this instance using the given Map<ID, Target> index
     * @param targetObservations list to process
     * @param mapIDTargets Map<ID, Target> index
     */
    protected static final void updateTargetReferences(final List<TargetRawObservation> targetObservations,
                                                       final java.util.Map<String, Target> mapIDTargets) {

        Target target, newTarget;

        for (final java.util.ListIterator<TargetRawObservation> it = targetObservations.listIterator(); it.hasNext();) {
            final TargetRawObservation targetRawObs = it.next();

            target = targetRawObs.getTargetRef();

            if (target == null) {
                logger.debug("Removing invalid target reference.");
                it.remove();
            } else {
                newTarget = mapIDTargets.get(target.getIdentifier());
                if (newTarget != null) {
                    if (newTarget != target) {
                        targetRawObs.setTargetRef(newTarget);
                    }

                    // remove if empty :
                    if (targetRawObs.getObservations().isEmpty()) {
                        if (logger.isDebugEnabled()) {
                            logger.debug("Removing empty target raw obs: {}", target.getIdentifier());
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
     * Return the existing TargetRawObservation corresponding to the target
     * @param target target to look up
     * @param targetObservations list of TargetRawObservation instances
     * @return TargetRawObservation or null
     */
    public static final TargetRawObservation getTargetRawObservation(final Target target,
                                                                     final java.util.List<TargetRawObservation> targetObservations) {
        for (int i = 0, len = targetObservations.size(); i < len; i++) {
            TargetRawObservation targetRawObs = targetObservations.get(i);
            if (targetRawObs.getTargetRef().equals(target)) {
                return targetRawObs;
            }
        }
        return null;
    }      
    
//--simple--preserve

}
