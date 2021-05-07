
package fr.jmmc.aspro.model.ob;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the observation constraints (LST or HA intervals)
 *             
 * 
 * <p>Java class for ObservationConstraints complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ObservationConstraints"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="atmosphereQuality" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="seeing" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="tau0" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="HAinterval" type="{http://www.w3.org/2001/XMLSchema}string" maxOccurs="unbounded"/&gt;
 *         &lt;element name="LSTinterval" type="{http://www.w3.org/2001/XMLSchema}string" maxOccurs="unbounded"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ObservationConstraints", propOrder = {
    "atmosphereQuality",
    "seeing",
    "tau0",
    "hAintervals",
    "lsTintervals"
})
public class ObservationConstraints
    extends OIBase
{

    @XmlElement(required = true)
    protected String atmosphereQuality;
    protected double seeing;
    protected double tau0;
    @XmlElement(name = "HAinterval", required = true)
    protected List<String> hAintervals;
    @XmlElement(name = "LSTinterval", required = true)
    protected List<String> lsTintervals;

    /**
     * Gets the value of the atmosphereQuality property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAtmosphereQuality() {
        return atmosphereQuality;
    }

    /**
     * Sets the value of the atmosphereQuality property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAtmosphereQuality(String value) {
        this.atmosphereQuality = value;
    }

    /**
     * Gets the value of the seeing property.
     * 
     */
    public double getSeeing() {
        return seeing;
    }

    /**
     * Sets the value of the seeing property.
     * 
     */
    public void setSeeing(double value) {
        this.seeing = value;
    }

    /**
     * Gets the value of the tau0 property.
     * 
     */
    public double getTau0() {
        return tau0;
    }

    /**
     * Sets the value of the tau0 property.
     * 
     */
    public void setTau0(double value) {
        this.tau0 = value;
    }

    /**
     * Gets the value of the hAintervals property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the hAintervals property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getHAintervals().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getHAintervals() {
        if (hAintervals == null) {
            hAintervals = new ArrayList<String>();
        }
        return this.hAintervals;
    }

    /**
     * Gets the value of the lsTintervals property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the lsTintervals property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getLSTintervals().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link String }
     * 
     * 
     */
    public List<String> getLSTintervals() {
        if (lsTintervals == null) {
            lsTintervals = new ArrayList<String>();
        }
        return this.lsTintervals;
    }

}
