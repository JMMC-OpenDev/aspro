
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
import fr.jmmc.jmal.model.targetmodel.Model;


/**
 * 
 *                 This type describes a target
 *             
 * 
 * <p>Java class for Target complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Target"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="origin" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="RA" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="DEC" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="EQUINOX" type="{http://www.w3.org/2001/XMLSchema}float"/&gt;
 *         &lt;element name="SYSVEL" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="VELTYP" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="PMRA" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="PMDEC" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="PARALLAX" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="PARA_ERR" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="IDS" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="OBJTYP" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="SPECTYP" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="FLUX_B" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="FLUX_V" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="FLUX_G" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="FLUX_R" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="FLUX_I" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="FLUX_J" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="FLUX_H" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="FLUX_K" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="FLUX_L" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="FLUX_M" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="FLUX_N" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="useAnalyticalModel" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/&gt;
 *         &lt;element ref="{http://www.jmmc.fr/jmcs/models/0.1}model" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="userModel" type="{http://www.jmmc.fr/aspro-oi/0.1}UserModel" minOccurs="0"/&gt;
 *         &lt;element name="configuration" type="{http://www.jmmc.fr/aspro-oi/0.1}TargetConfiguration" minOccurs="0"/&gt;
 *         &lt;element name="calibratorInfos" type="{http://www.jmmc.fr/aspro-oi/0.1}CalibratorInformations" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="id" use="required" type="{http://www.w3.org/2001/XMLSchema}ID" /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
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
    "fluxb",
    "fluxv",
    "fluxg",
    "fluxr",
    "fluxi",
    "fluxj",
    "fluxh",
    "fluxk",
    "fluxl",
    "fluxm",
    "fluxn",
    "useAnalyticalModel",
    "models",
    "userModel",
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
    @XmlElement(name = "FLUX_B")
    protected Double fluxb;
    @XmlElement(name = "FLUX_V")
    protected Double fluxv;
    @XmlElement(name = "FLUX_G")
    protected Double fluxg;
    @XmlElement(name = "FLUX_R")
    protected Double fluxr;
    @XmlElement(name = "FLUX_I")
    protected Double fluxi;
    @XmlElement(name = "FLUX_J")
    protected Double fluxj;
    @XmlElement(name = "FLUX_H")
    protected Double fluxh;
    @XmlElement(name = "FLUX_K")
    protected Double fluxk;
    @XmlElement(name = "FLUX_L")
    protected Double fluxl;
    @XmlElement(name = "FLUX_M")
    protected Double fluxm;
    @XmlElement(name = "FLUX_N")
    protected Double fluxn;
    protected Boolean useAnalyticalModel;
    @XmlElement(name = "model", namespace = "http://www.jmmc.fr/jmcs/models/0.1")
    protected List<Model> models;
    protected UserModel userModel;
    protected TargetConfiguration configuration;
    protected CalibratorInformations calibratorInfos;
    @XmlAttribute(name = "id", required = true)
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
     * Gets the value of the fluxb property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFLUXB() {
        return fluxb;
    }

    /**
     * Sets the value of the fluxb property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFLUXB(Double value) {
        this.fluxb = value;
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
     * Gets the value of the fluxg property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFLUXG() {
        return fluxg;
    }

    /**
     * Sets the value of the fluxg property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFLUXG(Double value) {
        this.fluxg = value;
    }

    /**
     * Gets the value of the fluxr property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFLUXR() {
        return fluxr;
    }

    /**
     * Sets the value of the fluxr property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFLUXR(Double value) {
        this.fluxr = value;
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
     * Gets the value of the fluxl property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFLUXL() {
        return fluxl;
    }

    /**
     * Sets the value of the fluxl property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFLUXL(Double value) {
        this.fluxl = value;
    }

    /**
     * Gets the value of the fluxm property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFLUXM() {
        return fluxm;
    }

    /**
     * Sets the value of the fluxm property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFLUXM(Double value) {
        this.fluxm = value;
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
     * Gets the value of the useAnalyticalModel property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isUseAnalyticalModel() {
        return useAnalyticalModel;
    }

    /**
     * Sets the value of the useAnalyticalModel property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setUseAnalyticalModel(Boolean value) {
        this.useAnalyticalModel = value;
    }

    /**
     * 
     *                         Optional list of analytical models
     *                     Gets the value of the models property.
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
     * Gets the value of the userModel property.
     * 
     * @return
     *     possible object is
     *     {@link UserModel }
     *     
     */
    public UserModel getUserModel() {
        return userModel;
    }

    /**
     * Sets the value of the userModel property.
     * 
     * @param value
     *     allowed object is
     *     {@link UserModel }
     *     
     */
    public void setUserModel(UserModel value) {
        this.userModel = value;
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
    /** empty Target instance */
    public static final Target EMPTY_TARGET = new Target();
    
    /**
     * Fix coordinates RA and DEC (HMS / DMS formats)
     */
    public final void fixCoords() {
        setCoords(
                fr.jmmc.aspro.model.util.TargetUtils.fixRA(getRA()),
                fr.jmmc.aspro.model.util.TargetUtils.fixDEC(getDEC()),
                getEQUINOX()
        );
    }

    /**
     * Set coordinates RA and DEC (HMS / DMS formats)
     * @param ra RA HMS
     * @param dec DEC DMS
     * @param equinox coordinate equinox
     */
    public final void setCoords(final String ra, final String dec, final float equinox) {
        setRA(ra);
        setDEC(dec);
        setEQUINOX(equinox);
        // reset any cached RA/DEC as degrees:
        setRADeg(Double.NaN);
        setDECDeg(Double.NaN);
    }

    /**
     * @return true if RA or DEC is NaN
     */
    public final boolean isNaNCoords() {
        return Double.isNaN(getRADeg()) || Double.isNaN(getDECDeg());
    }

    /** computed RA in degrees */
    @javax.xml.bind.annotation.XmlTransient
    private double raDeg = Double.NaN;

    /**
     * Return the right ascension (RA) in degrees
     * @return right ascension (RA) in degrees
     */
    public final double getRADeg() {
        if (Double.isNaN(this.raDeg)) {
            setRADeg(fr.jmmc.jmal.ALX.parseHMS(getRA()));
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
            setDECDeg(fr.jmmc.jmal.ALX.parseDEC(getDEC()));
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
        // ensure id is never null:
        if (this.id == null) {
            setIdentifier();
        }
        return this.id;
    }
    
    private final void setIdentifier() {
        setId(fr.jmmc.aspro.model.util.XmlIdUtils.convert(getName()));
    }

    /**
     * Format and update the (internal) name and identifier.
     * Warning: this modified identifier is not updated in relationships
     */
    public final void updateNameAndIdentifier() {
        updateNameAndIdentifier(getName());
    }

    /**
     * Format and update the name and identifier.
     * Warning: this modified identifier is not updated in relationships
     * @param name new name to use
     */
    public final void updateNameAndIdentifier(final String name) {
        this.setName(formatName(name));
        // recompute identifier:
        setIdentifier();
    }

    /**
     * This equals method uses the identifier equality
     * @param obj other object
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
     * Return an HTML representation of the target used by tooltips in the given string buffer
     * @param sb string buffer to fill
     * @param full flag to display full information
     */
    public final void toHtml(final StringBuilder sb, final boolean full) {
        sb.append("<b>Name: ").append(getName()).append("</b>");
        if (getRA() != null && getDEC() != null) {
            // note: generated targets for baseline limits do not have RA/DEC as string (useless):
            sb.append("<br><b>Coords</b>: ").append(getRA()).append(' ').append(getDEC());
        }
        if (full) {
            if (getPMRA() != null && getPMDEC() != null) {
                sb.append("<br><b>Proper motion</b> (mas/yr): ").append(getPMRA()).append(' ').append(getPMDEC());
            }
            if (getPARALLAX() != null && getPARAERR() != null) {
                sb.append("<br><b>Parallax</b> (mas): ").append(getPARALLAX()).append(" [").append(getPARAERR()).append(']');
            }
            if (getSYSVEL() != null) {
                sb.append("<br><b>Radial Velocity</b> (km/s): ").append(getSYSVEL());
                if (getVELTYP() != null) {
                    sb.append(" (").append(getVELTYP()).append(')');
                }
            }
            if (getOBJTYP() != null && getOBJTYP().length() > 0) {
                sb.append("<br><b>Object types</b>: ").append(getOBJTYP());
            }
            if (getSPECTYP() != null && getSPECTYP().length() > 0) {
                sb.append("<br><b>Spectral types</b>: ").append(getSPECTYP());
            }
        }
        // Fluxes :
        sb.append("<br>");
        int n = 0;
        Double flux;
        for (SpectralBand b : SpectralBand.values()) {
            flux = getFlux(b);
            if (flux != null) {
                if (n != 0 && n <= 3) {
                    sb.append(" - ");
                }

                sb.append("<b>").append(b.value()).append("</b>: ").append(flux);

                n++;

                if (n > 3) {
                    sb.append("<br>");
                    n = 0;
                }
            }
        }
        // Ids ?
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
                case B:
                    return getFLUXB();
                case V:
                    return getFLUXV();
                case G:
                    return getFLUXG();
                case R:
                    return getFLUXR();
                case I:
                    return getFLUXI();
                case J:
                    return getFLUXJ();
                case H:
                    return getFLUXH();
                case K:
                    return getFLUXK();
                case L:
                    return getFLUXL();
                case M:
                    return getFLUXM();
                case N:
                    return getFLUXN();
                default:
            }
        }
        return null;
    }

    /**
     * Return the uniform disk diameter from calibrator informations (SearchCal):
     * use first UD_<band>, then alternate diameters UD, LD, UDDK, DIA12 (in order of priority)
     * @param band instrumental band
     * @return uniform disk diameter in the given band or null if undefined
     */
    public final Double getDiameter(final SpectralBand band) {
        final CalibratorInformations calInfos = getCalibratorInfos();
        if (band != null && calInfos != null) {
            // if UD_<band> diameter is missing, use other values ...
            final Double udBand = calInfos.getUDDiameter(band);

            if (udBand != null) {
                return udBand;

            } else {
                // use alternate diameter UD, LD, UDDK, DIA12 (in order of priority) :
                final BaseValue diam = getCalibratorInfos().getAlternateDiameter();

                if (diam != null) {
                    return diam.getNumber();
                }
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
        if (copy.userModel != null) {
            copy.userModel = (UserModel) copy.userModel.clone();
        }

        // Deep copy of target configuration :
        if (copy.configuration != null) {
            copy.configuration = (TargetConfiguration) copy.configuration.clone();
        }
        
        if (copy.calibratorInfos != null) {
            copy.calibratorInfos = (CalibratorInformations) copy.calibratorInfos.clone();
        }

        return copy;
    }

    /**
     * Return true if the model is an analytical model; false an user model
     * @return true if the model is an analytical model; false an user model
     */
    public final boolean hasAnalyticalModel() {
        return (this.useAnalyticalModel == null) ? true : this.useAnalyticalModel.booleanValue();
    }

    /**
     * Return true if this target has one model (analytical models or an user model)
     * @return true if this target has one model
     */
    public final boolean hasModel() {
        if (hasAnalyticalModel()) {
            return !isEmpty(this.models);
        } else {
            return (this.userModel != null && this.userModel.isFileValid());
        }
    }

    /**
     * Get or create a new User Model
     * @return User Model
     */
    public final UserModel getOrCreateUserModel() {
        if (this.userModel == null) {
            this.userModel = new UserModel();
        }
        return this.userModel;
    }

    /* static helper methods */
    @Override
    protected boolean areEquals(final OIBase o) {
        if (!super.areEquals(o)) {
            return false;
        }
        final Target other = (Target) o;
        return (areEquals(this.id, other.getId())
                && areEquals(this.name, other.getName())
                && areEquals(this.origin, other.getOrigin())
                && areEquals(this.ra, other.getRA())
                && areEquals(this.dec, other.getDEC())
                && areEquals(this.equinox, other.getEQUINOX())
                && areEquals(this.sysvel, other.getSYSVEL())
                && areEquals(this.veltyp, other.getVELTYP())
                && areEquals(this.pmra, other.getPMRA())
                && areEquals(this.pmdec, other.getPMDEC())
                && areEquals(this.parallax, other.getPARALLAX())
                && areEquals(this.paraerr, other.getPARAERR())
                && areEquals(this.ids, other.getIDS())
                && areEquals(this.objtyp, other.getOBJTYP())
                && areEquals(this.spectyp, other.getSPECTYP())
                && areEquals(this.fluxb, other.getFLUXB())
                && areEquals(this.fluxv, other.getFLUXV())
                && areEquals(this.fluxg, other.getFLUXG())
                && areEquals(this.fluxr, other.getFLUXR())
                && areEquals(this.fluxi, other.getFLUXI())
                && areEquals(this.fluxj, other.getFLUXJ())
                && areEquals(this.fluxh, other.getFLUXH())
                && areEquals(this.fluxk, other.getFLUXK())
                && areEquals(this.fluxl, other.getFLUXL())
                && areEquals(this.fluxm, other.getFLUXM())
                && areEquals(this.fluxn, other.getFLUXN())
                && areEquals(this.useAnalyticalModel, other.isUseAnalyticalModel())
                && areEquals(this.getModels(), other.getModels()) // Strict equals(), may create lists
                && areEquals(this.userModel, other.getUserModel())
                && areEquals(this.configuration, other.getConfiguration())
                && areEquals(this.calibratorInfos, other.getCalibratorInfos()));
    }

    /**
     * Return a deep "copy" of the list of models
     * @param models list of models to clone
     * @return cloned model list
     */
    public static List<Model> cloneModels(final List<Model> models) {
        return fr.jmmc.jmal.model.CloneableObject.deepCopyList(models);
    }

    /**
     * Check if the given target list contains the given target: same identifier or coordinates (crossmatch)
     * @param srcTarget target to look for
     * @param targets list of targets
     * @return target or null if the target was not found
     */
    public static Target matchTarget(final Target srcTarget, final List<Target> targets) {
        final fr.jmmc.aspro.model.util.TargetMatch match = doMatchTarget(srcTarget, targets);
        return (match != null) ? match.getMatch() : null;
    }

    /**
     * Check if the given target list contains the given target: same identifier or coordinates (crossmatch)
     * @param srcTarget target to look for
     * @param targets list of targets
     * @return target or null if the target was not found
     */
    public static fr.jmmc.aspro.model.util.TargetMatch doMatchTarget(final Target srcTarget, final List<Target> targets) {
        // Is the same identifier ?
        Target t = getTargetById(srcTarget.getIdentifier(), targets);
        if (t != null) {
            return new fr.jmmc.aspro.model.util.TargetMatch(t);
        }
        return fr.jmmc.aspro.model.util.TargetUtils.matchTargetCoordinates(srcTarget, targets);
    }

    /**
     * Return the target of the given name in the given list of targets
     * @param name target name
     * @param targets list of targets
     * @return target or null if the target was not found
     */
    public static Target getTarget(final String name, final List<Target> targets) {
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
    public static Target getTargetById(final String id, final List<Target> targets) {
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
     * Returns true only if the given target instance is present in the given target list
     * @param target target instance to look for
     * @param targets target list
     * @return true if instance found, false otherwise
     */
    public static boolean containsInstance(final Target target, final List<Target> targets) {
        final int pos = targets.indexOf(target);
        if (pos >= 0) {
            // check instance equality (pointer like):
            if (targets.get(pos) == target) {
                return true;
            }
        }
        return false;
    }

    /**
     * Remove the given science target from the given target list and user informations
     * @param target science target to remove
     * @param targets target list to modify
     * @param targetUserInfos target user informations to modify
     * @return true if the target was removed
     */
    public static boolean removeTarget(final Target target, final List<Target> targets, final TargetUserInformations targetUserInfos) {
        if (target != null) {
            // remove calibrators related to the science target :
            targetUserInfos.getCalibrators(target).clear();

            if (targets.remove(target)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Remove the given calibrator target from the given target list and user informations
     * @param calibrator calibrator target to remove its references
     * @param targets target list to check
     * @param targetUserInfos target user informations to modify
     */
    public static void removeCalibratorReferences(final Target calibrator, final List<Target> targets, final TargetUserInformations targetUserInfos) {
        if (calibrator != null) {
            for (Target target : targets) {
                targetUserInfos.removeCalibratorFromTarget(target, calibrator);
            }
            targetUserInfos.removeCalibrator(calibrator);
        }
    }

    /**
     * Return the Map<ID, Target> index
     * @param targets
     * @return Map<ID, Target> index
     */
    static java.util.Map<String, Target> createTargetIndex(final List<Target> targets) {
        // create the Map<ID, Target> index:
        if (targets == null) {
            return java.util.Collections.emptyMap();
        }
        final java.util.Map<String, Target> mapIDTargets = new java.util.HashMap<String, Target>(targets.size());
        for (Target target : targets) {
            mapIDTargets.put(target.getIdentifier(), target);
        }
        return mapIDTargets;
    }

    protected static void updateTargetReferences(final java.util.List<Target> targets,
                                                 final java.util.Map<String, Target> mapIDTargets) {
        if (targets != null) {
            Target target, newTarget;

            for (final java.util.ListIterator<Target> it = targets.listIterator(); it.hasNext();) {
                target = it.next();

                newTarget = (mapIDTargets != null) ? mapIDTargets.get(target.getIdentifier()) : null;
                if (newTarget != null) {
                    if (newTarget != target) {
                        it.set(newTarget);
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
     * Format the given star name to become a valid target name (identifier)
     * @param name any star name (not null)
     * @return target name
     */
    public static String formatName(final String name) {
        // trim and remove redudant white spaces :
        return fr.jmmc.jmcs.util.StringUtils.cleanWhiteSpaces(name);
    }

    /**
     * Merge target information on the given target with information from the simbad target
     * @param target target to update
     * @param source Simbad target where information comes from
     */
    public static void mergeSimbadTarget(final Target target, final Target source) {
        // Reset fields that must be overriden if present in simbad target
        // (coords, PM, Parallax, sptype, Obj types, identifiers)
        target.setCoords(source.getRA(), source.getDEC(), source.getEQUINOX());

        if (source.getSYSVEL() != null) {
            target.setSYSVEL(null);
            target.setVELTYP(null);
        }
        
        if (source.getPMRA() != null && source.getPMDEC() != null) {
            target.setPMRA(null);
            target.setPMDEC(null);
        }

        if (source.getPARALLAX() != null) {
            target.setPARALLAX(null);
            target.setPARAERR(null);
        }
        
        if (!isEmpty(source.getIDS())) {
            target.setIDS(null);
        }
        if (!isEmpty(source.getOBJTYP())) {
            target.setOBJTYP(null);
        }
        if (!isEmpty(source.getSPECTYP())) {
            target.setSPECTYP(null);
        }        
        
        merge(target, source);
    }
    
    /**
     * Merge target information on the given target with information from the source target
     * @param target target to update
     * @param source target where information comes from
     */
    public static void merge(final Target target, final Target source) {
        // skip name, id
        // skip useAnalyticalModel, model, userModel, configuration, calibratorInfos

        // only update RA/DEC if NaN:
        if (target.isNaNCoords() && !source.isNaNCoords()) {
            target.setCoords(source.getRA(), source.getDEC(), source.getEQUINOX());
        }

        if (target.getSYSVEL() == null && source.getSYSVEL() != null) {
            target.setSYSVEL(source.getSYSVEL());
        }
        if (isEmpty(target.getVELTYP()) && !isEmpty(source.getVELTYP())) {
            target.setVELTYP(source.getVELTYP());
        }

        if (target.getPMRA() == null && source.getPMRA() != null) {
            target.setPMRA(source.getPMRA());
        }
        if (target.getPMDEC() == null && source.getPMDEC() != null) {
            target.setPMDEC(source.getPMDEC());
        }

        if (target.getPARALLAX() == null && source.getPARALLAX() != null) {
            target.setPARALLAX(source.getPARALLAX());
        }
        if (target.getPARAERR() == null && source.getPARAERR() != null) {
            target.setPARAERR(source.getPARAERR());
        }

        if (isEmpty(target.getIDS()) && !isEmpty(source.getIDS())) {
            target.setIDS(source.getIDS());
        }
        if (isEmpty(target.getOBJTYP()) && !isEmpty(source.getOBJTYP())) {
            target.setOBJTYP(source.getOBJTYP());
        }
        if (isEmpty(target.getSPECTYP()) && !isEmpty(source.getSPECTYP())) {
            target.setSPECTYP(source.getSPECTYP());
        }

        if (target.getFLUXB() == null && source.getFLUXB() != null) {
            target.setFLUXB(source.getFLUXB());
        }
        if (target.getFLUXV() == null && source.getFLUXV() != null) {
            target.setFLUXV(source.getFLUXV());
        }
        if (target.getFLUXG() == null && source.getFLUXG() != null) {
            target.setFLUXG(source.getFLUXG());
        }
        if (target.getFLUXR() == null && source.getFLUXR() != null) {
            target.setFLUXR(source.getFLUXR());
        }
        if (target.getFLUXI() == null && source.getFLUXI() != null) {
            target.setFLUXI(source.getFLUXI());
        }
        if (target.getFLUXJ() == null && source.getFLUXJ() != null) {
            target.setFLUXJ(source.getFLUXJ());
        }
        if (target.getFLUXH() == null && source.getFLUXH() != null) {
            target.setFLUXH(source.getFLUXH());
        }
        if (target.getFLUXK() == null && source.getFLUXK() != null) {
            target.setFLUXK(source.getFLUXK());
        }
        if (target.getFLUXL() == null && source.getFLUXL() != null) {
            target.setFLUXL(source.getFLUXL());
        }
        if (target.getFLUXM() == null && source.getFLUXM() != null) {
            target.setFLUXM(source.getFLUXM());
        }
        if (target.getFLUXN() == null && source.getFLUXN() != null) {
            target.setFLUXN(source.getFLUXN());
        }
        if (source.getCalibratorInfos() != null) {
            // merge calibratorInfos:
            if (target.getCalibratorInfos() == null) {
                target.setCalibratorInfos(new CalibratorInformations());
            }
            CalibratorInformations.merge(target.getCalibratorInfos(), source.getCalibratorInfos());
        }
    }

    /**
     * Check values:
     * - optional number values: replace NaN by null values
     * - string values: replace "" by null values
     */
    public void checkValues() {
        if (isEmpty(veltyp)) {
            veltyp = null;
        }
        if (isEmpty(ids)) {
            ids = null;
        }
        if (isEmpty(objtyp)) {
            objtyp = null;
        }
        if (isEmpty(spectyp)) {
            spectyp = null;
        }
        if (isNaN(sysvel)) {
            sysvel = null;
        }
        if (isNaN(pmra)) {
            pmra = null;
        }
        if (isNaN(pmdec)) {
            pmdec = null;
        }
        if (isNaN(parallax)) {
            parallax = null;
        }
        if (isNaN(paraerr)) {
            paraerr = null;
        }
        if (isNaN(fluxb)) {
            fluxb = null;
        }
        if (isNaN(fluxv)) {
            fluxv = null;
        }
        if (isNaN(fluxr)) {
            fluxr = null;
        }
        if (isNaN(fluxg)) {
            fluxg = null;
        }
        if (isNaN(fluxi)) {
            fluxi = null;
        }
        if (isNaN(fluxj)) {
            fluxj = null;
        }
        if (isNaN(fluxh)) {
            fluxh = null;
        }
        if (isNaN(fluxk)) {
            fluxk = null;
        }
        if (isNaN(fluxl)) {
            fluxl = null;
        }
        if (isNaN(fluxm)) {
            fluxm = null;
        }
        if (isNaN(fluxn)) {
            fluxn = null;
        }
    }

//--simple--preserve

}
