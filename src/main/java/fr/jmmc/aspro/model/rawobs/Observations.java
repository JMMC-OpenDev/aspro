
package fr.jmmc.aspro.model.rawobs;

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
 *                 This type describes a list of raw observations
 *             
 * 
 * <p>Java class for RawObservationList complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="RawObservationList"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="observation" type="{http://www.jmmc.fr/aspro-raw-obs/0.1}RawObservation" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "RawObservationList", propOrder = {
    "observations"
})
@XmlRootElement(name = "observations")
public class Observations
    extends OIBase
{

    @XmlElement(name = "observation")
    protected List<RawObservation> observations;

    /**
     * Gets the value of the observations property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the observations property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getObservations().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link RawObservation }
     * 
     * 
     */
    public List<RawObservation> getObservations() {
        if (observations == null) {
            observations = new ArrayList<RawObservation>();
        }
        return this.observations;
    }
    
//--simple--preserve
    
    public RawObservation first() {
        if (observations.isEmpty()) {
            return null;
        }
        return observations.get(0);
    }

    public RawObservation last() {
        if (observations.isEmpty()) {
            return null;
        }
        return observations.get(observations.size() - 1);
    }

    @Override
    public final String toString() {
        return "Observations [" + getObservations().size() + "] {"
                + "observations: " + getObservations()
                + "}";
    }

//--simple--preserve

}
