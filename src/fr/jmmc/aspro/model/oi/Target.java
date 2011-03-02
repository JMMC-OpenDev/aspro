
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import fr.jmmc.aspro.model.OIBase;
import fr.jmmc.mcs.model.targetmodel.Model;


/**
 * 
 *         This type describes a target
 *       
 * 
 * <p>Java class for Target complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Target">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="origin" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="RA" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="DEC" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="EQUINOX" type="{http://www.w3.org/2001/XMLSchema}float"/>
 *         &lt;element name="SYSVEL" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="VELTYP" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="PMRA" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="PMDEC" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="PARALLAX" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="PARA_ERR" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="IDS" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="OBJTYP" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="SPECTYP" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="FLUX_V" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="FLUX_I" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="FLUX_J" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="FLUX_H" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="FLUX_K" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="FLUX_N" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element ref="{http://www.jmmc.fr/jmcs/models/0.1}model" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="configuration" type="{http://www.jmmc.fr/aspro-oi/0.1}TargetConfiguration" minOccurs="0"/>
 *         &lt;element name="calibratorInfos" type="{http://www.jmmc.fr/aspro-oi/0.1}CalibratorInformations" minOccurs="0"/>
 *       &lt;/sequence>
 *       &lt;attribute name="id" use="required" type="{http://www.w3.org/2001/XMLSchema}ID" />
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Target", propOrder = {
    "name",
    "origin",
    "ra",
    "dec",
    "equinox",
    "sysvel",
    "veltyp",
    "pmra",
    "pmdec",
    "parallax",
    "paraerr",
    "ids",
    "objtyp",
    "spectyp",
    "fluxv",
    "fluxi",
    "fluxj",
    "fluxh",
    "fluxk",
    "fluxn",
    "models",
    "configuration",
    "calibratorInfos"
})
public class Target
    extends OIBase
{

    @XmlElement(required = true)
    protected String name;
    protected String origin;
    @XmlElement(name = "RA", required = true)
    protected String ra;
    @XmlElement(name = "DEC", required = true)
    protected String dec;
    @XmlElement(name = "EQUINOX")
    protected float equinox;
    @XmlElement(name = "SYSVEL")
    protected Double sysvel;
    @XmlElement(name = "VELTYP")
    protected String veltyp;
    @XmlElement(name = "PMRA")
    protected Double pmra;
    @XmlElement(name = "PMDEC")
    protected Double pmdec;
    @XmlElement(name = "PARALLAX")
    protected Double parallax;
    @XmlElement(name = "PARA_ERR")
    protected Double paraerr;
    @XmlElement(name = "IDS")
    protected String ids;
    @XmlElement(name = "OBJTYP")
    protected String objtyp;
    @XmlElement(name = "SPECTYP")
    protected String spectyp;
    @XmlElement(name = "FLUX_V")
    protected Double fluxv;
    @XmlElement(name = "FLUX_I")
    protected Double fluxi;
    @XmlElement(name = "FLUX_J")
    protected Double fluxj;
    @XmlElement(name = "FLUX_H")
    protected Double fluxh;
    @XmlElement(name = "FLUX_K")
    protected Double fluxk;
    @XmlElement(name = "FLUX_N")
    protected Double fluxn;
    @XmlElement(name = "model", namespace = "http://www.jmmc.fr/jmcs/models/0.1")
    protected List<Model> models;
    protected TargetConfiguration configuration;
    protected CalibratorInformations calibratorInfos;
    @XmlAttribute(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String id;

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
     * Gets the value of the origin property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getOrigin() {
        return origin;
    }

    /**
     * Sets the value of the origin property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setOrigin(String value) {
        this.origin = value;
    }

    /**
     * Gets the value of the ra property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRA() {
        return ra;
    }

    /**
     * Sets the value of the ra property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRA(String value) {
        this.ra = value;
    }

    /**
     * Gets the value of the dec property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDEC() {
        return dec;
    }

    /**
     * Sets the value of the dec property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDEC(String value) {
        this.dec = value;
    }

    /**
     * Gets the value of the equinox property.
     * 
     */
    public float getEQUINOX() {
        return equinox;
    }

    /**
     * Sets the value of the equinox property.
     * 
     */
    public void setEQUINOX(float value) {
        this.equinox = value;
    }

    /**
     * Gets the value of the sysvel property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getSYSVEL() {
        return sysvel;
    }

    /**
     * Sets the value of the sysvel property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setSYSVEL(Double value) {
        this.sysvel = value;
    }

    /**
     * Gets the value of the veltyp property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getVELTYP() {
        return veltyp;
    }

    /**
     * Sets the value of the veltyp property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setVELTYP(String value) {
        this.veltyp = value;
    }

    /**
     * Gets the value of the pmra property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getPMRA() {
        return pmra;
    }

    /**
     * Sets the value of the pmra property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setPMRA(Double value) {
        this.pmra = value;
    }

    /**
     * Gets the value of the pmdec property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getPMDEC() {
        return pmdec;
    }

    /**
     * Sets the value of the pmdec property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setPMDEC(Double value) {
        this.pmdec = value;
    }

    /**
     * Gets the value of the parallax property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getPARALLAX() {
        return parallax;
    }

    /**
     * Sets the value of the parallax property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setPARALLAX(Double value) {
        this.parallax = value;
    }

    /**
     * Gets the value of the paraerr property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getPARAERR() {
        return paraerr;
    }

    /**
     * Sets the value of the paraerr property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setPARAERR(Double value) {
        this.paraerr = value;
    }

    /**
     * Gets the value of the ids property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getIDS() {
        return ids;
    }

    /**
     * Sets the value of the ids property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setIDS(String value) {
        this.ids = value;
    }

    /**
     * Gets the value of the objtyp property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getOBJTYP() {
        return objtyp;
    }

    /**
     * Sets the value of the objtyp property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setOBJTYP(String value) {
        this.objtyp = value;
    }

    /**
     * Gets the value of the spectyp property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSPECTYP() {
        return spectyp;
    }

    /**
     * Sets the value of the spectyp property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSPECTYP(String value) {
        this.spectyp = value;
    }

    /**
     * Gets the value of the fluxv property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFLUXV() {
        return fluxv;
    }

    /**
     * Sets the value of the fluxv property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFLUXV(Double value) {
        this.fluxv = value;
    }

    /**
     * Gets the value of the fluxi property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFLUXI() {
        return fluxi;
    }

    /**
     * Sets the value of the fluxi property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFLUXI(Double value) {
        this.fluxi = value;
    }

    /**
     * Gets the value of the fluxj property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFLUXJ() {
        return fluxj;
    }

    /**
     * Sets the value of the fluxj property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFLUXJ(Double value) {
        this.fluxj = value;
    }

    /**
     * Gets the value of the fluxh property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFLUXH() {
        return fluxh;
    }

    /**
     * Sets the value of the fluxh property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFLUXH(Double value) {
        this.fluxh = value;
    }

    /**
     * Gets the value of the fluxk property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFLUXK() {
        return fluxk;
    }

    /**
     * Sets the value of the fluxk property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFLUXK(Double value) {
        this.fluxk = value;
    }

    /**
     * Gets the value of the fluxn property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFLUXN() {
        return fluxn;
    }

    /**
     * Sets the value of the fluxn property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFLUXN(Double value) {
        this.fluxn = value;
    }

    /**
     * Gets the value of the models property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the models property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getModels().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Model }
     * 
     * 
     */
    public List<Model> getModels() {
        if (models == null) {
            models = new ArrayList<Model>();
        }
        return this.models;
    }

    /**
     * Gets the value of the configuration property.
     * 
     * @return
     *     possible object is
     *     {@link TargetConfiguration }
     *     
     */
    public TargetConfiguration getConfiguration() {
        return configuration;
    }

    /**
     * Sets the value of the configuration property.
     * 
     * @param value
     *     allowed object is
     *     {@link TargetConfiguration }
     *     
     */
    public void setConfiguration(TargetConfiguration value) {
        this.configuration = value;
    }

    /**
     * Gets the value of the calibratorInfos property.
     * 
     * @return
     *     possible object is
     *     {@link CalibratorInformations }
     *     
     */
    public CalibratorInformations getCalibratorInfos() {
        return calibratorInfos;
    }

    /**
     * Sets the value of the calibratorInfos property.
     * 
     * @param value
     *     allowed object is
     *     {@link CalibratorInformations }
     *     
     */
    public void setCalibratorInfos(CalibratorInformations value) {
        this.calibratorInfos = value;
    }

    /**
     * Gets the value of the id property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setId(String value) {
        this.id = value;
    }
    
//--simple--preserve
  /** computed RA in degrees */
  @javax.xml.bind.annotation.XmlTransient
  private double raDeg = Double.NaN;

  /**
   * Return the right ascension (RA) in degrees
   * @return right ascension (RA) in degrees
   */
  public final double getRADeg() {
    if (Double.isNaN(this.raDeg)) {
      this.raDeg = fr.jmmc.mcs.astro.ALX.parseHMS(getRA());
    }
    return this.raDeg;
  }

  /**
   * Define the right ascension (RA) in degrees (read only)
   * @param raDeg right ascension (RA) in degrees
   */
  public final void setRADeg(final double raDeg) {
    this.raDeg = raDeg;
  }
  /** computed DEC in degrees */
  @javax.xml.bind.annotation.XmlTransient
  private double decDeg = Double.NaN;

  /**
   * Return the declination (DEC) in degrees
   * @return declination (DEC) in degrees
   */
  public final double getDECDeg() {
    if (Double.isNaN(this.decDeg)) {
      this.decDeg = fr.jmmc.mcs.astro.ALX.parseDEC(getDEC());
    }
    return this.decDeg;
  }

  /**
   * Define the declination (DEC) in degrees (read only)
   * @param decDeg declination (DEC) in degrees
   */
  public final void setDECDeg(final double decDeg) {
    this.decDeg = decDeg;
  }

  /**
   * This method returns the target identifier.
   * If it is missing, it generates a new identifier from the name field.
   * @return target identifier
   */
  public final String getIdentifier() {
    if (getId() == null) {
      setId(fr.jmmc.aspro.model.util.XmlIdUtils.convert(getName()));
    }
    return getId();
  }

  /**
   * This equals method uses the identifier equality
   * @param obj other object (target)
   * @return true if the identifiers are equals
   */
  @Override
  public final boolean equals(final Object obj) {
    if (obj == null) {
      return false;
    }
    // identity comparison :
    if (this == obj) {
      return true;
    }
    // class check :
    if (getClass() != obj.getClass()) {
      return false;
    }
    final Target other = (Target) obj;

    return areEquals(this.getIdentifier(), other.getIdentifier());
  }

  /**
   * This hashcode implementation uses only the id field
   * @return hashcode 
   */
  @Override
  public final int hashCode() {
    int hash = 3;
    hash = 71 * hash + (this.getIdentifier() != null ? this.getIdentifier().hashCode() : 0);
    return hash;
  }

  @Override
  public final String toString() {
    return "Target [" + ((this.getName() != null) ? this.getName() : "undefined") + "]" + " RA = " + getRA() + " DEC = " + getDEC();
  }

  /**
   * Return an HTML representation of the target used by tooltips
   * @return HTML representation
   */
  public final String toHtml() {
    final StringBuilder sb = new StringBuilder(128);
    sb.append("<html>");
    sb.append("<b>Name</b> : ").append(getName());
    sb.append("<br><b>Coord</b> : ").append(getRA()).append(' ').append(getDEC());
    if (getPMRA() != null) {
      sb.append("<br><b>Proper motion</b> (mas/yr) : ").append(getPMRA()).append(' ').append(getPMDEC());
    }
    if (getPARALLAX() != null) {
      sb.append("<br><b>Parallax</b> (mas) : ").append(getPARALLAX()).append(" [").append(getPARAERR()).append(']');
    }
    if (getSYSVEL() != null) {
      sb.append("<br><b>Radial Velocity</b> (km/s) : ").append(getSYSVEL());
      if (getVELTYP() != null) {
        sb.append(" (").append(getVELTYP()).append(')');
      }
    }
    if (getOBJTYP() != null && getOBJTYP().length() > 0) {
      sb.append("<br><b>Object types</b> : ").append(getOBJTYP());
    }
    if (getSPECTYP() != null && getSPECTYP().length() > 0) {
      sb.append("<br><b>Spectral types</b> : ").append(getSPECTYP());
    }
    // Fluxes :
    if (getFLUXV() != null) {
      sb.append("<br><b>Flux V</b> : ").append(getFLUXV());
    }
    if (getFLUXI() != null) {
      sb.append("<br><b>Flux I</b> : ").append(getFLUXI());
    }
    if (getFLUXJ() != null) {
      sb.append("<br><b>Flux J</b> : ").append(getFLUXJ());
    }
    if (getFLUXH() != null) {
      sb.append("<br><b>Flux H</b> : ").append(getFLUXH());
    }
    if (getFLUXK() != null) {
      sb.append("<br><b>Flux K</b> : ").append(getFLUXK());
    }
    if (getFLUXN() != null) {
      sb.append("<br><b>Flux N</b> : ").append(getFLUXN());
    }

    // Ids ?

    sb.append("</html>");
    return sb.toString();
  }

  /**
   * Return the star identifier corresponding to the catalog identifier
   * @param catalogIdentifier complete catalog identifier (like 'HD' or 'HIP' ...)
   * @return star identifier (HD 31964) or null
   */
  public final String getIdentifier(final String catalogIdentifier) {
    String res = null;
    if (getIDS() != null) {
      final String cat = catalogIdentifier + " ";
      final String[] idArray = getIDS().split(",");
      for (String catEntry : idArray) {
        if (catEntry.startsWith(cat)) {
          res = catEntry;
          break;
        }
      }
    }
    return res;
  }

  /**
   * Return the flux in the given band
   * @param band spectral band
   * @return flux in the given band or null if undefined
   */
  public final Double getFlux(final SpectralBand band) {
    if (band != null) {
      switch (band) {
        case V:
          return getFLUXV();
        case R:
          // missing flux R : return V
          return getFLUXV();
        case I:
          return getFLUXI();
        case J:
          return getFLUXJ();
        case H:
          return getFLUXH();
        case K:
          return getFLUXK();
        case N:
          return getFLUXN();
        default:
      }
    }
    return null;
  }

  /**
   * Return a deep "copy" of this instance
   * @return deep "copy" of this instance
   */
  @Override
  public final Object clone() {
    final Target copy = (Target) super.clone();

    // Deep copy of models :
    if (copy.models != null) {
      copy.models = Target.cloneModels(copy.models);
    }

    // Deep copy of target configuration :
    if (copy.configuration != null) {
      copy.configuration = (TargetConfiguration) copy.configuration.clone();
    }

    return copy;
  }

  /**
   * Return a deep "copy" of the list of models
   * @param models list of models to clone
   * @return cloned model list
   */
  public static final List<Model> cloneModels(final List<Model> models) {
    return fr.jmmc.mcs.model.CloneableObject.deepCopyList(models);
  }

  /**
   * Return the target of the given name in the given list of targets
   * @param name target name
   * @param targets list of targets
   * @return target or null if the target was not found
   */
  public static final Target getTarget(final String name, final List<Target> targets) {
    if (name != null) {
      for (Target t : targets) {
        if (t.getName().equals(name)) {
          return t;
        }
      }
    }
    return null;
  }

  /**
   * Return the target of the given identifier in the given list of targets
   * @param id target identifier
   * @param targets list of targets
   * @return target or null if the target was not found
   */
  public static final Target getTargetById(final String id, final List<Target> targets) {
    if (id != null) {
      for (Target t : targets) {
        if (t.getIdentifier().equals(id)) {
          return t;
        }
      }
    }
    return null;
  }

  /**
   * Format the given star name to become a valid target name (identifier)
   * @param name any star name (not null)
   * @return target name
   */
  public static final String formatName(final String name) {
    // trim and upper case :
    return name.trim().toUpperCase();
  }
//--simple--preserve

}
