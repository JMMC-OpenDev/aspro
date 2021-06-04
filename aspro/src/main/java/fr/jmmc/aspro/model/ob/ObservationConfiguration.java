
package fr.jmmc.aspro.model.ob;

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


/**
 * 
 *                 This type describes an atomic observation
 *             
 * 
 * <p>Java class for ObservationConfiguration complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ObservationConfiguration"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="type" type="{http://www.jmmc.fr/aspro-ob/0.1}ObservationType"/&gt;
 *         &lt;element name="SCTarget" type="{http://www.jmmc.fr/aspro-ob/0.1}Target"/&gt;
 *         &lt;element name="FTTarget" type="{http://www.jmmc.fr/aspro-ob/0.1}Target" minOccurs="0"/&gt;
 *         &lt;element name="AOTarget" type="{http://www.jmmc.fr/aspro-ob/0.1}Target" minOccurs="0"/&gt;
 *         &lt;element name="GSTarget" type="{http://www.jmmc.fr/aspro-ob/0.1}Target" minOccurs="0"/&gt;
 *         &lt;element name="observationConstraints" type="{http://www.jmmc.fr/aspro-ob/0.1}ObservationConstraints"/&gt;
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
@XmlType(name = "ObservationConfiguration", propOrder = {
    "type",
    "scTarget",
    "ftTarget",
    "aoTarget",
    "gsTarget",
    "observationConstraints"
})
public class ObservationConfiguration
    extends OIBase
{

    @XmlElement(required = true)
    
    protected ObservationType type;
    @XmlElement(name = "SCTarget", required = true)
    protected Target scTarget;
    @XmlElement(name = "FTTarget")
    protected Target ftTarget;
    @XmlElement(name = "AOTarget")
    protected Target aoTarget;
    @XmlElement(name = "GSTarget")
    protected Target gsTarget;
    @XmlElement(required = true)
    protected ObservationConstraints observationConstraints;
    @XmlAttribute(name = "id", required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String id;

    /**
     * Gets the value of the type property.
     * 
     * @return
     *     possible object is
     *     {@link ObservationType }
     *     
     */
    public ObservationType getType() {
        return type;
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value
     *     allowed object is
     *     {@link ObservationType }
     *     
     */
    public void setType(ObservationType value) {
        this.type = value;
    }

    /**
     * Gets the value of the scTarget property.
     * 
     * @return
     *     possible object is
     *     {@link Target }
     *     
     */
    public Target getSCTarget() {
        return scTarget;
    }

    /**
     * Sets the value of the scTarget property.
     * 
     * @param value
     *     allowed object is
     *     {@link Target }
     *     
     */
    public void setSCTarget(Target value) {
        this.scTarget = value;
    }

    /**
     * Gets the value of the ftTarget property.
     * 
     * @return
     *     possible object is
     *     {@link Target }
     *     
     */
    public Target getFTTarget() {
        return ftTarget;
    }

    /**
     * Sets the value of the ftTarget property.
     * 
     * @param value
     *     allowed object is
     *     {@link Target }
     *     
     */
    public void setFTTarget(Target value) {
        this.ftTarget = value;
    }

    /**
     * Gets the value of the aoTarget property.
     * 
     * @return
     *     possible object is
     *     {@link Target }
     *     
     */
    public Target getAOTarget() {
        return aoTarget;
    }

    /**
     * Sets the value of the aoTarget property.
     * 
     * @param value
     *     allowed object is
     *     {@link Target }
     *     
     */
    public void setAOTarget(Target value) {
        this.aoTarget = value;
    }

    /**
     * Gets the value of the gsTarget property.
     * 
     * @return
     *     possible object is
     *     {@link Target }
     *     
     */
    public Target getGSTarget() {
        return gsTarget;
    }

    /**
     * Sets the value of the gsTarget property.
     * 
     * @param value
     *     allowed object is
     *     {@link Target }
     *     
     */
    public void setGSTarget(Target value) {
        this.gsTarget = value;
    }

    /**
     * Gets the value of the observationConstraints property.
     * 
     * @return
     *     possible object is
     *     {@link ObservationConstraints }
     *     
     */
    public ObservationConstraints getObservationConstraints() {
        return observationConstraints;
    }

    /**
     * Sets the value of the observationConstraints property.
     * 
     * @param value
     *     allowed object is
     *     {@link ObservationConstraints }
     *     
     */
    public void setObservationConstraints(ObservationConstraints value) {
        this.observationConstraints = value;
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

}
