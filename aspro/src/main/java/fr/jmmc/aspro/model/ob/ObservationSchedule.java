
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
 *                 This type describes the observation schedule
 *             
 * 
 * <p>Java class for ObservationSchedule complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ObservationSchedule"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="OB" type="{http://www.jmmc.fr/aspro-ob/0.1}OBItem" maxOccurs="unbounded"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ObservationSchedule", propOrder = {
    "obs"
})
public class ObservationSchedule
    extends OIBase
{

    @XmlElement(name = "OB", required = true)
    protected List<OBItem> obs;

    /**
     * Gets the value of the obs property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the obs property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getOBS().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link OBItem }
     * 
     * 
     */
    public List<OBItem> getOBS() {
        if (obs == null) {
            obs = new ArrayList<OBItem>();
        }
        return this.obs;
    }

}
