
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes the optional calibrator information (uniform disk diameters).
 *       
 * 
 * <p>Java class for CalibratorInformations complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CalibratorInformations">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="UD_U" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="UD_B" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="UD_V" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="UD_R" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="UD_I" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="UD_J" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="UD_H" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="UD_K" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="UD_L" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="UD_N" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="LD" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CalibratorInformations", propOrder = {
    "udu",
    "udb",
    "udv",
    "udr",
    "udi",
    "udj",
    "udh",
    "udk",
    "udl",
    "udn",
    "ld"
})
public class CalibratorInformations
    extends OIBase
{

    @XmlElement(name = "UD_U")
    protected Double udu;
    @XmlElement(name = "UD_B")
    protected Double udb;
    @XmlElement(name = "UD_V")
    protected Double udv;
    @XmlElement(name = "UD_R")
    protected Double udr;
    @XmlElement(name = "UD_I")
    protected Double udi;
    @XmlElement(name = "UD_J")
    protected Double udj;
    @XmlElement(name = "UD_H")
    protected Double udh;
    @XmlElement(name = "UD_K")
    protected Double udk;
    @XmlElement(name = "UD_L")
    protected Double udl;
    @XmlElement(name = "UD_N")
    protected Double udn;
    @XmlElement(name = "LD")
    protected Double ld;

    /**
     * Gets the value of the udu property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getUDU() {
        return udu;
    }

    /**
     * Sets the value of the udu property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setUDU(Double value) {
        this.udu = value;
    }

    /**
     * Gets the value of the udb property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getUDB() {
        return udb;
    }

    /**
     * Sets the value of the udb property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setUDB(Double value) {
        this.udb = value;
    }

    /**
     * Gets the value of the udv property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getUDV() {
        return udv;
    }

    /**
     * Sets the value of the udv property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setUDV(Double value) {
        this.udv = value;
    }

    /**
     * Gets the value of the udr property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getUDR() {
        return udr;
    }

    /**
     * Sets the value of the udr property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setUDR(Double value) {
        this.udr = value;
    }

    /**
     * Gets the value of the udi property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getUDI() {
        return udi;
    }

    /**
     * Sets the value of the udi property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setUDI(Double value) {
        this.udi = value;
    }

    /**
     * Gets the value of the udj property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getUDJ() {
        return udj;
    }

    /**
     * Sets the value of the udj property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setUDJ(Double value) {
        this.udj = value;
    }

    /**
     * Gets the value of the udh property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getUDH() {
        return udh;
    }

    /**
     * Sets the value of the udh property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setUDH(Double value) {
        this.udh = value;
    }

    /**
     * Gets the value of the udk property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getUDK() {
        return udk;
    }

    /**
     * Sets the value of the udk property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setUDK(Double value) {
        this.udk = value;
    }

    /**
     * Gets the value of the udl property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getUDL() {
        return udl;
    }

    /**
     * Sets the value of the udl property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setUDL(Double value) {
        this.udl = value;
    }

    /**
     * Gets the value of the udn property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getUDN() {
        return udn;
    }

    /**
     * Sets the value of the udn property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setUDN(Double value) {
        this.udn = value;
    }

    /**
     * Gets the value of the ld property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getLD() {
        return ld;
    }

    /**
     * Sets the value of the ld property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setLD(Double value) {
        this.ld = value;
    }

}
