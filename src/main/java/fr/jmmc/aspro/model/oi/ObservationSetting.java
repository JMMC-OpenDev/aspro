
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlList;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the observation settings (when, where, what, how)
 *             
 * 
 * <p>Java class for ObservationSetting complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ObservationSetting"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="schemaVersion" type="{http://www.w3.org/2001/XMLSchema}float"/&gt;
 *         &lt;element name="targetVersion" type="{http://www.w3.org/2001/XMLSchema}float"/&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="when" type="{http://www.jmmc.fr/aspro-oi/0.1}WhenSetting"/&gt;
 *         &lt;element name="interferometerConfiguration" type="{http://www.jmmc.fr/aspro-oi/0.1}InterferometerConfigurationChoice"/&gt;
 *         &lt;element name="instrumentConfiguration" type="{http://www.jmmc.fr/aspro-oi/0.1}FocalInstrumentConfigurationChoice"/&gt;
 *         &lt;element name="target" type="{http://www.jmmc.fr/aspro-oi/0.1}Target" maxOccurs="unbounded"/&gt;
 *         &lt;element name="selectedTargets" type="{http://www.w3.org/2001/XMLSchema}IDREFS" minOccurs="0"/&gt;
 *         &lt;element name="targetUserInfos" type="{http://www.jmmc.fr/aspro-oi/0.1}TargetUserInformations" minOccurs="0"/&gt;
 *         &lt;element name="targetObservations" type="{http://www.jmmc.fr/aspro-oi/0.1}TargetRawObservation" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="variant" type="{http://www.jmmc.fr/aspro-oi/0.1}ObservationVariant" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="context" type="{http://www.jmmc.fr/aspro-oi/0.1}ObservationContext" minOccurs="0"/&gt;
 *         &lt;element name="extendedConfiguration" type="{http://www.jmmc.fr/aspro-oi/0.1}InterferometerConfiguration" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ObservationSetting", propOrder = {
    "schemaVersion",
    "targetVersion",
    "name",
    "description",
    "when",
    "interferometerConfiguration",
    "instrumentConfiguration",
    "targets",
    "selectedTargets",
    "targetUserInfos",
    "targetObservations",
    "variants",
    "context",
    "extendedConfiguration"
})
@XmlRootElement(name = "observationSetting")
public class ObservationSetting
    extends OIBase
{

    protected float schemaVersion;
    protected float targetVersion;
    @XmlElement(required = true)
    protected String name;
    protected String description;
    @XmlElement(required = true)
    protected WhenSetting when;
    @XmlElement(required = true)
    protected InterferometerConfigurationChoice interferometerConfiguration;
    @XmlElement(required = true)
    protected FocalInstrumentConfigurationChoice instrumentConfiguration;
    @XmlElement(name = "target", required = true)
    protected List<fr.jmmc.aspro.model.oi.Target> targets;
    @XmlList
    @XmlElement(type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREFS")
    protected List<Target> selectedTargets;
    protected TargetUserInformations targetUserInfos;
    protected List<TargetRawObservation> targetObservations;
    @XmlElement(name = "variant")
    protected List<ObservationVariant> variants;
    protected ObservationContext context;
    protected InterferometerConfiguration extendedConfiguration;

    /**
     * Gets the value of the schemaVersion property.
     * 
     */
    public float getSchemaVersion() {
        return schemaVersion;
    }

    /**
     * Sets the value of the schemaVersion property.
     * 
     */
    public void setSchemaVersion(float value) {
        this.schemaVersion = value;
    }

    /**
     * Gets the value of the targetVersion property.
     * 
     */
    public float getTargetVersion() {
        return targetVersion;
    }

    /**
     * Sets the value of the targetVersion property.
     * 
     */
    public void setTargetVersion(float value) {
        this.targetVersion = value;
    }

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
     * Gets the value of the when property.
     * 
     * @return
     *     possible object is
     *     {@link WhenSetting }
     *     
     */
    public WhenSetting getWhen() {
        return when;
    }

    /**
     * Sets the value of the when property.
     * 
     * @param value
     *     allowed object is
     *     {@link WhenSetting }
     *     
     */
    public void setWhen(WhenSetting value) {
        this.when = value;
    }

    /**
     * Gets the value of the interferometerConfiguration property.
     * 
     * @return
     *     possible object is
     *     {@link InterferometerConfigurationChoice }
     *     
     */
    public InterferometerConfigurationChoice getInterferometerConfiguration() {
        return interferometerConfiguration;
    }

    /**
     * Sets the value of the interferometerConfiguration property.
     * 
     * @param value
     *     allowed object is
     *     {@link InterferometerConfigurationChoice }
     *     
     */
    public void setInterferometerConfiguration(InterferometerConfigurationChoice value) {
        this.interferometerConfiguration = value;
    }

    /**
     * Gets the value of the instrumentConfiguration property.
     * 
     * @return
     *     possible object is
     *     {@link FocalInstrumentConfigurationChoice }
     *     
     */
    public FocalInstrumentConfigurationChoice getInstrumentConfiguration() {
        return instrumentConfiguration;
    }

    /**
     * Sets the value of the instrumentConfiguration property.
     * 
     * @param value
     *     allowed object is
     *     {@link FocalInstrumentConfigurationChoice }
     *     
     */
    public void setInstrumentConfiguration(FocalInstrumentConfigurationChoice value) {
        this.instrumentConfiguration = value;
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
     * {@link fr.jmmc.aspro.model.oi.Target }
     * 
     * 
     */
    public List<fr.jmmc.aspro.model.oi.Target> getTargets() {
        if (targets == null) {
            targets = new ArrayList<fr.jmmc.aspro.model.oi.Target>();
        }
        return this.targets;
    }

    /**
     * Gets the value of the selectedTargets property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the selectedTargets property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getSelectedTargets().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Object }
     * 
     * 
     */
    public List<Target> getSelectedTargets() {
        if (selectedTargets == null) {
            selectedTargets = new ArrayList<Target>();
        }
        return this.selectedTargets;
    }

    /**
     * Gets the value of the targetUserInfos property.
     * 
     * @return
     *     possible object is
     *     {@link TargetUserInformations }
     *     
     */
    public TargetUserInformations getTargetUserInfos() {
        return targetUserInfos;
    }

    /**
     * Sets the value of the targetUserInfos property.
     * 
     * @param value
     *     allowed object is
     *     {@link TargetUserInformations }
     *     
     */
    public void setTargetUserInfos(TargetUserInformations value) {
        this.targetUserInfos = value;
    }

    /**
     * Gets the value of the targetObservations property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the targetObservations property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getTargetObservations().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link TargetRawObservation }
     * 
     * 
     */
    public List<TargetRawObservation> getTargetObservations() {
        if (targetObservations == null) {
            targetObservations = new ArrayList<TargetRawObservation>();
        }
        return this.targetObservations;
    }

    /**
     * Gets the value of the variants property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the variants property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getVariants().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ObservationVariant }
     * 
     * 
     */
    public List<ObservationVariant> getVariants() {
        if (variants == null) {
            variants = new ArrayList<ObservationVariant>();
        }
        return this.variants;
    }

    /**
     * Gets the value of the context property.
     * 
     * @return
     *     possible object is
     *     {@link ObservationContext }
     *     
     */
    public ObservationContext getContext() {
        return context;
    }

    /**
     * Sets the value of the context property.
     * 
     * @param value
     *     allowed object is
     *     {@link ObservationContext }
     *     
     */
    public void setContext(ObservationContext value) {
        this.context = value;
    }

    /**
     * Gets the value of the extendedConfiguration property.
     * 
     * @return
     *     possible object is
     *     {@link InterferometerConfiguration }
     *     
     */
    public InterferometerConfiguration getExtendedConfiguration() {
        return extendedConfiguration;
    }

    /**
     * Sets the value of the extendedConfiguration property.
     * 
     * @param value
     *     allowed object is
     *     {@link InterferometerConfiguration }
     *     
     */
    public void setExtendedConfiguration(InterferometerConfiguration value) {
        this.extendedConfiguration = value;
    }

//--simple--preserve
    /**
     * Return the number of variants
     * @return number of variants
     */
    public final int getVariantSize() {
        return getVariants().size();
    }

    /**
     * Return true if this observation has only one variant
     * @return true if this observation has only one variant
     */
    public final boolean isSingle() {
        return getVariantSize() == 1;
    }

    /**
     * Return true if the target list is not empty
     * @return true if the target list is not empty
     */
    public final boolean hasTargets() {
        return !isEmpty(targets);
    }

    /**
     * Return the target of the given name
     * @param name target name
     * @return target or null if the target was not found
     */
    public final Target getTarget(final String name) {
        return Target.getTarget(name, getTargets());
    }

    /**
     * Return the target of the given identifier
     * @param id target identifier
     * @return target or null if the target was not found
     */
    public final Target getTargetById(final String id) {
        return Target.getTargetById(id, getTargets());
    }

    /**
     * Return the target user informations (create a new one if needed)
     * @return target user informations
     */
    public final TargetUserInformations getOrCreateTargetUserInfos() {
        TargetUserInformations userInfos = getTargetUserInfos();
        if (userInfos == null) {
            userInfos = new TargetUserInformations();
            setTargetUserInfos(userInfos);
        }
        return userInfos;
    }

    /**
     * Return the target configuration of the target given by its name 
     * @param name target name
     * @return target configuration or null if the target was not found
     */
    public final TargetConfiguration getTargetConfiguration(final String name) {
        final Target target = getTarget(name);
        if (target != null) {
            TargetConfiguration targetConf = target.getConfiguration();
            if (targetConf == null) {
                targetConf = new TargetConfiguration();
                target.setConfiguration(targetConf);
            }
            return targetConf;
        }
        return null;
    }
    /** observation version (read only) */
    @javax.xml.bind.annotation.XmlTransient
    fr.jmmc.aspro.model.ObservationVersion version = new fr.jmmc.aspro.model.ObservationVersion();

    /**
     * Return the observation version
     * @return observation version
     */
    public final fr.jmmc.aspro.model.ObservationVersion getVersion() {
        return this.version;
    }

    /** 
     * @return Target instance corresponding to the first selected target or null
     */
    public Target getSelectedTarget() {
        if (!isEmpty(selectedTargets)) {
            // ensure consistency by resolving reference:
            return getTargetById(selectedTargets.get(0).getIdentifier());
        }
        return null;
    }

    @Override
    public final String toString() {
        return "Observation : " + ((this.name != null) ? this.name : "undefined");
    }

    /**
     * Return a partial deep "copy" of this instance excluding target / models / target user informations
     * and clear computed fields and observation variants
     *
     * @return deep "copy" of this instance
     */
    @Override
    public final Object clone() {
        final ObservationSetting copy = (ObservationSetting) super.clone();

        // copy version :
        copy.version = new fr.jmmc.aspro.model.ObservationVersion(copy.version);

        // clear observation variants :
        copy.variants = null;

        // Deep copy child objects :
        if (copy.when != null) {
            copy.when = (WhenSetting) copy.when.clone();
        }
        if (copy.interferometerConfiguration != null) {
            copy.interferometerConfiguration = (InterferometerConfigurationChoice) copy.interferometerConfiguration.clone();
        }
        if (copy.instrumentConfiguration != null) {
            copy.instrumentConfiguration = (FocalInstrumentConfigurationChoice) copy.instrumentConfiguration.clone();
        }

        // Just copy list of targets (do not clone targets as it is only used by target editor) :
        if (copy.targets != null) {
            copy.targets = OIBase.copyList(copy.targets);
        }
        if (copy.selectedTargets != null) {
            copy.selectedTargets = OIBase.copyList(copy.selectedTargets);
        }
        // Do not copy target user infos (only used by target editor)
        return copy;
    }

    /**
     * Return a deep "copy" of this instance including target / models / target user informations
     * @return deep "copy" of this instance
     */
    public final ObservationSetting deepClone() {
        final ObservationSetting copy = (ObservationSetting) clone();

        // Deep copy of observation variants :
        copy.variants = OIBase.deepCopyList(getVariants());

        // Deep copy of targets :
        if (copy.targets != null) {
            copy.targets = OIBase.deepCopyList(copy.targets);
        }

        // Deep copy of target user infos :
        if (copy.targetUserInfos != null) {
            // deep copy objects but not targets (already done previously, only used for id / idref) :
            copy.targetUserInfos = (TargetUserInformations) copy.targetUserInfos.clone();

            // replace old target instances by cloned target instances :
            copy.targetUserInfos.updateTargetReferences(Target.createTargetIndex(copy.targets));
        }

        return copy;
    }

    /**
     * Return a TargetEditContext to edit target information (deeply cloned List of Target + TargetUserInformations)
     * @return new TargetEditContext instance
     */
    public final fr.jmmc.aspro.model.TargetEditContext createTargetEditContext() {
        // Deep copy of targets :
        final List<Target> editTargets = OIBase.deepCopyList(this.getTargets());
        final TargetUserInformations editTargetUserInfos;

        // Deep copy of target user infos :
        if (targetUserInfos == null) {
            editTargetUserInfos = new TargetUserInformations();
        } else {
            // deep copy objects but not targets (already done previously, only used for id / idref) :
            editTargetUserInfos = (TargetUserInformations) targetUserInfos.clone();

            // replace old target instances by cloned target instances :
            editTargetUserInfos.updateTargetReferences(Target.createTargetIndex(editTargets));
        }

        return new fr.jmmc.aspro.model.TargetEditContext(editTargets, editTargetUserInfos);
    }

    @Override
    protected boolean areEquals(final OIBase o) {
        if (!super.areEquals(o)) {
            return false;
        }
        final ObservationSetting other = (ObservationSetting) o;
        // note: ignore schemaVersion, context, extendedConfiguration:
        return (areEquals(this.name, other.getName())
                && areEquals(this.description, other.getDescription())
                && areEquals(this.when, other.getWhen())
                && areEquals(this.interferometerConfiguration, other.getInterferometerConfiguration())
                && areEquals(this.instrumentConfiguration, other.getInstrumentConfiguration())
                && areEquals(this.getTargets(), other.getTargets()) // may create lists
                && areEquals(this.getSelectedTargets(), other.getSelectedTargets()) // may create lists
                && areEquals(this.targetUserInfos, other.getTargetUserInfos())
                && areEquals(this.getVariants(), other.getVariants())); // may create lists
    }

    /**
     * Check this object for bad reference(s) and removes them if needed.
     * For now it checks target ID/IDREF consistency (targetUserInformations...)
     */
    public void checkReferences() {
        checkReferences(getTargets(), this.selectedTargets, this.targetUserInfos, this.targetObservations);

        final fr.jmmc.jmal.model.ModelManager mm = fr.jmmc.jmal.model.ModelManager.getInstance();

        // check targets:
        for (Target target : getTargets()) {
            target.checkValues();

            if (target.hasModel() && target.hasAnalyticalModel()) {
                // validate only analytical models for target using them:
                mm.checkModelValues(target.getModels());
            }
        }

        // check target user infos :
        if (this.targetUserInfos != null && this.targetUserInfos.isEmpty()) {
            logger.debug("Removing empty target user informations.");
            this.targetUserInfos = null;
        }
        // check target's raw observations:
        if (this.targetObservations != null && this.targetObservations.isEmpty()) {
            logger.debug("Removing empty target raw observations.");
            this.targetObservations = null;
        }
    }

    /**
     * Check this object for bad reference(s) and removes them if needed.
     * @param targets list of targets
     * @param selectedTargets list of selected targets
     * @param targetUserInfos target user infos
     * @param targetObservations target raw observations
     */
    public static void checkReferences(final List<Target> targets,
                                       final List<Target> selectedTargets,
                                       final TargetUserInformations targetUserInfos,
                                       final List<TargetRawObservation> targetObservations) {

        java.util.Map<String, Target> mapIDTargets = null;
        final java.util.Set<String> usedIds = new java.util.HashSet<String>(16);

        // check selected targets:
        if (selectedTargets != null) {
            logger.debug("checkReferences = {}", selectedTargets);

            if (mapIDTargets == null) {
                // create the Map<ID, Target> index for targets:
                mapIDTargets = Target.createTargetIndex(targets);
            }
            Target.updateTargetReferences(selectedTargets, mapIDTargets, usedIds, "obs");
        }
        // check target user infos:
        if (targetUserInfos != null) {
            logger.debug("checkReferences = {}", targetUserInfos);

            if (mapIDTargets == null) {
                // create the Map<ID, Target> index for targets:
                mapIDTargets = Target.createTargetIndex(targets);
            }
            targetUserInfos.updateTargetReferences(mapIDTargets, usedIds);
        }
        // check target's raw observations:
        if (targetObservations != null) {
            logger.debug("checkReferences = {}", targetObservations);

            if (mapIDTargets == null) {
                // create the Map<ID, Target> index for targets:
                mapIDTargets = Target.createTargetIndex(targets);
            }
            TargetRawObservation.updateTargetReferences(targetObservations, mapIDTargets, usedIds);
        }
    }

    /** computed displayable list of targets (read only) */
    @javax.xml.bind.annotation.XmlTransient
    private List<Target> cachedDisplayTargets = null;

    /** computed set of orphan calibrators (read only) */
    @javax.xml.bind.annotation.XmlTransient
    private java.util.Set<Target> cachedOrphanCalibrators = null;

    /**
     * Clear any cached value related to targets
     */
    public void clearCacheTargets() {
        this.cachedDisplayTargets = null;
        this.cachedOrphanCalibrators = null;

        if (getTargetUserInfos() != null) {
            getTargetUserInfos().clearCacheGroups();
        }
        clearCacheFilteredTargets();
    }

    /**
     * Return the displayable list of targets containing
     * - science targets followed by their calibrators
     * - calibrator orphans
     * @return displayable list of targets
     */
    public List<Target> getDisplayTargets() {
        if (this.cachedDisplayTargets != null) {
            return this.cachedDisplayTargets;
        }
        computeDisplayTargets(false);

        return this.cachedDisplayTargets;
    }

    /**
     * Return the set of orphan calibrators
     * @return set of orphan calibrators
     */
    public java.util.Set<Target> getOrphanCalibrators() {
        if (this.cachedOrphanCalibrators != null) {
            return this.cachedOrphanCalibrators;
        }
        computeDisplayTargets(false);

        return this.cachedOrphanCalibrators;
    }

    @javax.xml.bind.annotation.XmlTransient
    private List<Target> filteredTargets = null;

    public List<Target> getFilteredTargets() {
        return filteredTargets;
    }

    public void setFilteredTargets(final List<Target> filteredTargets) {
        this.filteredTargets = filteredTargets;
        clearCacheFilteredTargets();
    }

    /** computed displayable list of targets (read only) */
    @javax.xml.bind.annotation.XmlTransient
    private List<Target> cachedFilteredDisplayTargets = null;

    /**
     * Clear any cached value related to targets
     */
    public void clearCacheFilteredTargets() {
        this.cachedFilteredDisplayTargets = null;
    }

    /**
     * Return the displayable list of targets containing
     * - science targets followed by their calibrators
     * - calibrator orphans
     * @return displayable list of targets
     */
    public List<Target> getFilteredDisplayTargets() {
        if (this.cachedFilteredDisplayTargets != null) {
            return this.cachedFilteredDisplayTargets;
        }
        computeDisplayTargets(true);

        return this.cachedFilteredDisplayTargets;
    }

    /**
     * Compute the displayable list of targets containing
     * - science targets followed by their calibrators
     * - calibrator orphans
     * And the set of orphan calibrators
     * @param filtered
     */
    private void computeDisplayTargets(final boolean filtered) {
        final List<Target> innerTargets = (filtered) ? getFilteredTargets() : getTargets();

        final List<Target> displayTargets;
        final java.util.Set<Target> orphans;

        if ((innerTargets == null) || innerTargets.isEmpty()) {
            displayTargets = java.util.Collections.emptyList();
            orphans = (filtered) ? null : java.util.Collections.emptySet();
        } else {
            final int len = innerTargets.size();

            displayTargets = new ArrayList<Target>(len);
            orphans = (filtered) ? null : new java.util.HashSet<Target>(4);

            // map of used calibrators :
            final java.util.Map<Target, Target> usedCalibrators = new java.util.IdentityHashMap<Target, Target>(8);

            // get the existing target user informations (can be null) :
            final TargetUserInformations localTargetUserInfos = getTargetUserInfos();

            for (Target target : innerTargets) {
                if (localTargetUserInfos == null) {
                    // no calibrator defined, all targets are science targets :
                    displayTargets.add(target);
                } else if (!localTargetUserInfos.isCalibrator(target)) {
                    // science targets :
                    displayTargets.add(target);

                    // add calibrators related to the science target :
                    for (Target calibrator : localTargetUserInfos.getCalibrators(target)) {
                        // calibrator targets :
                        displayTargets.add(calibrator);

                        if (orphans != null) {
                            usedCalibrators.put(calibrator, calibrator);
                        }
                    }
                }
            }

            if ((orphans != null) && (localTargetUserInfos != null)) {
                // add calibrator orphans i.e. not associated to a target :
                for (Target calibrator : localTargetUserInfos.getCalibrators()) {
                    if (!usedCalibrators.containsKey(calibrator)) {
                        displayTargets.add(calibrator);
                        orphans.add(calibrator);
                    }
                }
            }
        }

        // cache the computed collections:
        if (filtered) {
            this.cachedFilteredDisplayTargets = displayTargets;
        } else {
            this.cachedDisplayTargets = displayTargets;
        }
        if (orphans != null) {
            this.cachedOrphanCalibrators = orphans;
        }
    }

    /**
     * Return the target raw observation (create a new one if needed)
     * @param target target
     * @return target raw observation
     */
    public final TargetRawObservation getOrCreateTargetRawObservation(final Target target) {
        if (target == null) {
            throw new NullPointerException("Target can not be null !");
        }
        TargetRawObservation rawObs = getTargetRawObservation(target);
        if (rawObs == null) {
            rawObs = new TargetRawObservation();
            rawObs.setTargetRef(target);
            getTargetObservations().add(rawObs);
        }
        return rawObs;
    }

    /**
     * Return the existing target raw observation corresponding to the target
     * @param target target
     * @return target raw observation or null
     */
    public final TargetRawObservation getTargetRawObservation(final Target target) {
        return TargetRawObservation.getTargetRawObservation(target, getTargetObservations());
    }

    /** TODO: fix temporary storage for instrument filter (in-memory) */
    @javax.xml.bind.annotation.XmlTransient
    private List<String> rawObsFilterInsNames = null;

    public List<String> getRawObsFilterInsNames() {
        return rawObsFilterInsNames;
    }

    public void setRawObsFilterInsNames(List<String> rawObsFilterInsNames) {
        this.rawObsFilterInsNames = rawObsFilterInsNames;
    }

//--simple--preserve

}
