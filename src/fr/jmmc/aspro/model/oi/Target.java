
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
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
 *         &lt;element name="RA" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="DEC" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="EQUINOX" type="{http://www.w3.org/2001/XMLSchema}float"/>
 *         &lt;element name="PMRA" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="PMDEC" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="PARALLAX" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="PARA_ERR" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="SPECTYP" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="FLUX_V" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="FLUX_I" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="FLUX_J" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="FLUX_H" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="FLUX_K" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="FLUX_N" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element ref="{http://www.jmmc.fr/jmcs/models/0.1}model" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
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
    "ra",
    "dec",
    "equinox",
    "pmra",
    "pmdec",
    "parallax",
    "paraerr",
    "spectyp",
    "fluxv",
    "fluxi",
    "fluxj",
    "fluxh",
    "fluxk",
    "fluxn",
    "models"
})
public class Target
    extends OIBase
{

    @XmlElement(required = true)
    protected String name;
    @XmlElement(name = "RA")
    protected double ra;
    @XmlElement(name = "DEC")
    protected double dec;
    @XmlElement(name = "EQUINOX")
    protected float equinox;
    @XmlElement(name = "PMRA")
    protected Double pmra;
    @XmlElement(name = "PMDEC")
    protected Double pmdec;
    @XmlElement(name = "PARALLAX")
    protected Double parallax;
    @XmlElement(name = "PARA_ERR")
    protected Double paraerr;
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
     */
    public double getRA() {
        return ra;
    }

    /**
     * Sets the value of the ra property.
     * 
     */
    public void setRA(double value) {
        this.ra = value;
    }

    /**
     * Gets the value of the dec property.
     * 
     */
    public double getDEC() {
        return dec;
    }

    /**
     * Sets the value of the dec property.
     * 
     */
    public void setDEC(double value) {
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
    
//--simple--preserve

    @Override
    public String toString() {
      return "Target [" + ((this.name != null) ? this.name : "undefined") + "]"
              + " RA = " + getRA() + " DEC = " + getDEC();
    }

//--simple--preserve

}
