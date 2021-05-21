
package fr.jmmc.aspro.model.ob;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


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
 *         &lt;element name="FLUX_L_JY" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="FLUX_M_JY" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="FLUX_N_JY" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="DIAMETER" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
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
    "fluxljy",
    "fluxmjy",
    "fluxnjy",
    "diameter"
})
public class Target
    extends OIBase
{

    @XmlElement(required = true)
    protected String name;
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
    @XmlElement(name = "FLUX_L_JY")
    protected Double fluxljy;
    @XmlElement(name = "FLUX_M_JY")
    protected Double fluxmjy;
    @XmlElement(name = "FLUX_N_JY")
    protected Double fluxnjy;
    @XmlElement(name = "DIAMETER")
    protected Double diameter;

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
     * Gets the value of the fluxljy property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFLUXLJY() {
        return fluxljy;
    }

    /**
     * Sets the value of the fluxljy property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFLUXLJY(Double value) {
        this.fluxljy = value;
    }

    /**
     * Gets the value of the fluxmjy property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFLUXMJY() {
        return fluxmjy;
    }

    /**
     * Sets the value of the fluxmjy property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFLUXMJY(Double value) {
        this.fluxmjy = value;
    }

    /**
     * Gets the value of the fluxnjy property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFLUXNJY() {
        return fluxnjy;
    }

    /**
     * Sets the value of the fluxnjy property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFLUXNJY(Double value) {
        this.fluxnjy = value;
    }

    /**
     * Gets the value of the diameter property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getDIAMETER() {
        return diameter;
    }

    /**
     * Sets the value of the diameter property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setDIAMETER(Double value) {
        this.diameter = value;
    }

}
