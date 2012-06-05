
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
 *         This type describes the interferometer and its configurations
 *       
 * 
 * <p>Java class for InterferometerSetting complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="InterferometerSetting">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="description" type="{http://www.jmmc.fr/aspro-oi/0.1}InterferometerDescription"/>
 *         &lt;element name="configuration" type="{http://www.jmmc.fr/aspro-oi/0.1}InterferometerConfiguration" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "InterferometerSetting", propOrder = {
    "description",
    "configurations"
})
@XmlRootElement(name = "interferometerSetting")
public class InterferometerSetting
    extends OIBase
{

    @XmlElement(required = true)
    protected InterferometerDescription description;
    @XmlElement(name = "configuration", required = true)
    protected List<InterferometerConfiguration> configurations;

    /**
     * Gets the value of the description property.
     * 
     * @return
     *     possible object is
     *     {@link InterferometerDescription }
     *     
     */
    public InterferometerDescription getDescription() {
        return description;
    }

    /**
     * Sets the value of the description property.
     * 
     * @param value
     *     allowed object is
     *     {@link InterferometerDescription }
     *     
     */
    public void setDescription(InterferometerDescription value) {
        this.description = value;
    }

    /**
     * Gets the value of the configurations property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the configurations property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getConfigurations().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link InterferometerConfiguration }
     * 
     * 
     */
    public List<InterferometerConfiguration> getConfigurations() {
        if (configurations == null) {
            configurations = new ArrayList<InterferometerConfiguration>();
        }
        return this.configurations;
    }

}
