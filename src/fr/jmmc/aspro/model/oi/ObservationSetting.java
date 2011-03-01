
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes the observation settings (when, where, what, how)
 *       
 * 
 * <p>Java class for ObservationSetting complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ObservationSetting">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="schemaVersion" type="{http://www.w3.org/2001/XMLSchema}float"/>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="when" type="{http://www.jmmc.fr/aspro-oi/0.1}WhenSetting"/>
 *         &lt;element name="interferometerConfiguration" type="{http://www.jmmc.fr/aspro-oi/0.1}InterferometerConfigurationChoice"/>
 *         &lt;element name="instrumentConfiguration" type="{http://www.jmmc.fr/aspro-oi/0.1}FocalInstrumentConfigurationChoice"/>
 *         &lt;element name="target" type="{http://www.jmmc.fr/aspro-oi/0.1}Target" maxOccurs="unbounded"/>
 *         &lt;element name="targetUserInfos" type="{http://www.jmmc.fr/aspro-oi/0.1}TargetUserInformations" minOccurs="0"/>
 *         &lt;element name="variant" type="{http://www.jmmc.fr/aspro-oi/0.1}ObservationVariant" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ObservationSetting", propOrder = {
    "schemaVersion",
    "name",
    "description",
    "when",
    "interferometerConfiguration",
    "instrumentConfiguration",
    "targets",
    "targetUserInfos",
    "variants"
})
@XmlRootElement(name = "observationSetting")
public class ObservationSetting
    extends OIBase
{

    protected float schemaVersion;
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
    protected List<Target> targets;
    protected TargetUserInformations targetUserInfos;
    @XmlElement(name = "variant")
    protected List<ObservationVariant> variants;

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
     * {@link Target }
     * 
     * 
     */
    public List<Target> getTargets() {
        if (targets == null) {
            targets = new ArrayList<Target>();
        }
        return this.targets;
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
    return !getTargets().isEmpty();
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
  private fr.jmmc.aspro.model.ObservationVersion version = new fr.jmmc.aspro.model.ObservationVersion();

  /**
   * Return the observation version
   * @return observation version
   */
  public final fr.jmmc.aspro.model.ObservationVersion getVersion() {
    return this.version;
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

    // Do not copy target user infos (only used by target editor)

    return copy;
  }

  /**
   * Return a deep "copy" of this instance including target / models / target user informations
   * @return deep "copy" of this instance
   */
  public final ObservationSetting deepClone() {
    final ObservationSetting copy = (ObservationSetting) clone();

    // Deep copy of targets :
    if (copy.targets != null) {
      copy.targets = OIBase.deepCopyList(copy.targets);
    }

    // Deep copy of target user infos :
    if (copy.targetUserInfos != null) {
      // deep copy objects but not targets (already done previously, only used for id / idref) :
      copy.targetUserInfos = (TargetUserInformations) copy.targetUserInfos.clone();

      // replace old target instances by cloned target instances :
      copy.targetUserInfos.updateTargetReferences(copy.createTargetIndex());
    }

    return copy;
  }

  /**
   * Check this object for bad reference(s) and removes them if needed.
   * For now it checks target ID/IDREF consistency (targetUserInformations...)
   */
  public void checkReferences() {
    // check target user infos :
    if (this.targetUserInfos != null) {
      if (logger.isLoggable(java.util.logging.Level.FINE)) {
        logger.fine("checkReferences = " + this.targetUserInfos);
      }
      this.targetUserInfos.updateTargetReferences(this.createTargetIndex());

      // remove if empty :
      if (this.targetUserInfos.isEmpty()) {
        if (logger.isLoggable(java.util.logging.Level.FINE)) {
          logger.fine("Removing empty target user informations.");
        }
        this.targetUserInfos = null;
      }
    }
  }

  /**
   * Return the Map<ID, Target> index
   * @return Map<ID, Target> index
   */
  private java.util.Map<String, Target> createTargetIndex() {
    // create the Map<ID, Target> index :
    final java.util.Map<String, Target> mapIDTargets = new java.util.HashMap<String, Target>();
    for (Target target : this.targets) {
      mapIDTargets.put(target.getIdentifier(), target);
    }
    return mapIDTargets;
  }
  /** computed displayable list of targets (read only) */
  @javax.xml.bind.annotation.XmlTransient
  private List<Target> cachedDisplayTargets = null;

  /**
   * Clear any cached value related to targets
   */
  public void clearCacheTargets() {
    this.cachedDisplayTargets = null;
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

    computeDisplayTargets();

    return this.cachedDisplayTargets;
  }

  /**
   * Compute the displayable list of targets containing
   * - science targets followed by their calibrators
   * - calibrator orphans
   * And the parent target associated to display target list
   */
  private void computeDisplayTargets() {

    final List<Target> innerTargets = getTargets();

    final List<Target> displayTargets;

    if (innerTargets.isEmpty()) {
      displayTargets = java.util.Collections.emptyList();
    } else {
      displayTargets = new ArrayList<Target>();

      // map of used calibrators :
      final java.util.Map<Target, Target> usedCalibrators = new java.util.IdentityHashMap<Target, Target>();

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

            usedCalibrators.put(calibrator, calibrator);
          }
        }
      }

      if (localTargetUserInfos != null) {
        // add calibrator orphans i.e. not associated to a target :
        for (Target calibrator : localTargetUserInfos.getCalibrators()) {
          if (!usedCalibrators.containsKey(calibrator)) {
            displayTargets.add(calibrator);
          }
        }
      }
    }

    // cache the computed lists :
    this.cachedDisplayTargets = displayTargets;
  }

  /**
   * Generate a standard file name for the selected target using the format :
   * [<prefix>_<TARGET>_<INSTRUMENT>_<CONFIGURATION>_<suffix>_<DATE>.<extension>]
   *
   * @param targetName target name
   * @param prefix prefix (optional) for the file name
   * @param suffix suffix (optional) before the date
   * @param extension file extension
   * @return standard file name
   */
  public String generateFileName(final String targetName, final String prefix, final String suffix, final String extension) {
    final StringBuilder sb = new StringBuilder(32);
    if (prefix != null) {
      sb.append(prefix).append('_');
    }

    // replace invalid characters :
    final String altTargetName = targetName.replaceAll(fr.jmmc.aspro.AsproConstants.REGEXP_INVALID_TEXT_CHARS, "_");

    sb.append(altTargetName).append('_');

    final String instrumentName = this.getInstrumentConfiguration().getName();

    sb.append(instrumentName).append('_');

    final String baseLine = this.getInstrumentConfiguration().getStations().replaceAll(" ", "-");

    sb.append(baseLine).append('_');

    if (suffix != null) {
      sb.append(suffix).append('_');
    }

    final String date = this.getWhen().getDate().toString();

    sb.append(date).append('.').append(extension);

    return sb.toString();
  }
//--simple--preserve

}
