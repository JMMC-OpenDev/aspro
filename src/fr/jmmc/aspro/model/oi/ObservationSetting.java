
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
 *         This type describes the observation settings (when, where, what)
 * 
 *         Information to add :
 * 
 *         - atmosphere Quality (atmoQual) "Excellent" "Good" "Average" "Bad" "Awful"
 * 
 *         - dit "Detector Integration Time" /Choice 25 50 100
 *         - obsDuration "Total Integration Time (s) per calibrated point"
 * 
 *       
 * 
 * <p>Java class for Observation complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Observation">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="schemaVersion" type="{http://www.w3.org/2001/XMLSchema}float"/>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="when" type="{http://www.jmmc.fr/aspro-oi/0.1}WhenSetting"/>
 *         &lt;element name="interferometerConfiguration" type="{http://www.jmmc.fr/aspro-oi/0.1}InterferometerConfigurationChoice"/>
 *         &lt;element name="instrumentConfiguration" type="{http://www.jmmc.fr/aspro-oi/0.1}FocalInstrumentConfigurationChoice"/>
 *         &lt;element name="target" type="{http://www.jmmc.fr/aspro-oi/0.1}Target" maxOccurs="unbounded"/>
 *         &lt;element name="targetUserInfos" type="{http://www.jmmc.fr/aspro-oi/0.1}TargetUserInformations" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Observation", propOrder = {
    "schemaVersion",
    "name",
    "when",
    "interferometerConfiguration",
    "instrumentConfiguration",
    "targets",
    "targetUserInfos"
})
@XmlRootElement(name = "observationSetting")
public class ObservationSetting
    extends OIBase
{

    protected float schemaVersion;
    @XmlElement(required = true)
    protected String name;
    @XmlElement(required = true)
    protected WhenSetting when;
    @XmlElement(required = true)
    protected InterferometerConfigurationChoice interferometerConfiguration;
    @XmlElement(required = true)
    protected FocalInstrumentConfigurationChoice instrumentConfiguration;
    @XmlElement(name = "target", required = true)
    protected List<Target> targets;
    protected TargetUserInformations targetUserInfos;

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
    
//--simple--preserve
  /**
   * Return the target of the given name
   * @param name target name
   * @return target or null if the target was not found
   */
  public final Target getTarget(final String name) {
    return Target.getTarget(name, getTargets());
  }

  /**
   * Return the position of the target having the given name
   * @param name target name
   * @return position of -1 if not found
   */
  public final int getTargetPosition(final String name) {
    int i = 0;
    for (Target t : getTargets()) {
      if (t.getName().equals(name)) {
        return i;
      }
      i++;
    }
    return -1;
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
  /** computed observability data (read only) */
  @javax.xml.bind.annotation.XmlTransient
  private fr.jmmc.aspro.model.observability.ObservabilityData observabilityData = null;

  /**
   * Return the computed observability data (read only)
   * @return computed observability data or null
   */
  public final fr.jmmc.aspro.model.observability.ObservabilityData getObservabilityData() {
    return this.observabilityData;
  }

  /**
   * Define the computed observability data (read only)
   * @param obsData computed observability data
   */
  public final void setObservabilityData(final fr.jmmc.aspro.model.observability.ObservabilityData obsData) {
    this.observabilityData = obsData;
  }
  // TODO : store UVCoverageData also
  /** warning container (read only) */
  @javax.xml.bind.annotation.XmlTransient
  private fr.jmmc.aspro.model.WarningContainer warningContainer = null;

  /**
   * Return the warning container (read only)
   * @return warning container or null
   */
  public final fr.jmmc.aspro.model.WarningContainer getWarningContainer() {
    return this.warningContainer;
  }

  /**
   * Define the warning container (read only)
   * @param warningContainer warning container
   */
  public final void setWarningContainer(final fr.jmmc.aspro.model.WarningContainer warningContainer) {
    this.warningContainer = warningContainer;
  }
  /** computed OIFits structure (read only) */
  @javax.xml.bind.annotation.XmlTransient
  private fr.jmmc.oitools.model.OIFitsFile oiFitsFile = null;

  /**
   * Return the computed OIFits structure (read only)
   * @return OIFits structure or null
   */
  public final fr.jmmc.oitools.model.OIFitsFile getOIFitsFile() {
    return this.oiFitsFile;
  }

  /**
   * Define the computed OIFits structure (read only)
   * @param oiFitsFile computed OIFits structure
   */
  public final void setOIFitsFile(final fr.jmmc.oitools.model.OIFitsFile oiFitsFile) {
    this.oiFitsFile = oiFitsFile;
  }

  @Override
  public final String toString() {
    return "Observation : " + ((this.name != null) ? this.name : "undefined");
  }

  /**
   * Return a deep "copy" of this instance
   * @return deep "copy" of this instance
   */
  @Override
  public final Object clone() {
    final ObservationSetting copy = (ObservationSetting) super.clone();

    // clear computed fields :
    copy.observabilityData = null;
    copy.oiFitsFile = null;
    copy.warningContainer = null;

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
//--simple--preserve

}
