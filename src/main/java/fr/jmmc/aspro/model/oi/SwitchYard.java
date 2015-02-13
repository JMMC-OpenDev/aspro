
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes a possible switchyard for the interferometer
 *                 i.e. the fixed distance between a station and a channel in the focal plane
 *                 (opd = 0, time reference)
 *             
 * 
 * <p>Java class for SwitchYard complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="SwitchYard">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}ID"/>
 *         &lt;element name="useDelayLineRestriction" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
 *         &lt;element name="stationLinks" type="{http://www.jmmc.fr/aspro-oi/0.1}StationLinks" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "SwitchYard", propOrder = {
    "name",
    "useDelayLineRestriction",
    "stationLinks"
})
public class SwitchYard
    extends OIBase
{

    @XmlElement(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String name;
    protected boolean useDelayLineRestriction;
    @XmlElement(required = true)
    protected List<StationLinks> stationLinks;

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
     * Gets the value of the useDelayLineRestriction property.
     * 
     */
    public boolean isUseDelayLineRestriction() {
        return useDelayLineRestriction;
    }

    /**
     * Sets the value of the useDelayLineRestriction property.
     * 
     */
    public void setUseDelayLineRestriction(boolean value) {
        this.useDelayLineRestriction = value;
    }

    /**
     * Gets the value of the stationLinks property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the stationLinks property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getStationLinks().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link StationLinks }
     * 
     * 
     */
    public List<StationLinks> getStationLinks() {
        if (stationLinks == null) {
            stationLinks = new ArrayList<StationLinks>();
        }
        return this.stationLinks;
    }
    
//--simple--preserve

    @Override
    public final String toString() {
        return "SwitchYard [" + ((this.getName() != null) ? this.getName() : "undefined") + ']';
    }

//--simple--preserve

}
