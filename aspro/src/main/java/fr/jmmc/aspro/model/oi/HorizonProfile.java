
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the profile of the horizon seen from a station.
 *                 The profile is described as a polygon (list of points) defined
 *                 in azimuth [0..360 deg] and elevation [0..90 deg]
 * 
 *                 cf : http://www.eso.org/sci/facilities/paranal/telescopes/vlti/pointing/index.html
 *             
 * 
 * <p>Java class for HorizonProfile complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="HorizonProfile"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="point" type="{http://www.jmmc.fr/aspro-oi/0.1}AzEl" maxOccurs="unbounded"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "HorizonProfile", propOrder = {
    "points"
})
public class HorizonProfile
    extends OIBase
{

    @XmlElement(name = "point", required = true)
    protected List<AzEl> points;

    /**
     * Gets the value of the points property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the points property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getPoints().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link AzEl }
     * 
     * 
     */
    public List<AzEl> getPoints() {
        if (points == null) {
            points = new ArrayList<AzEl>();
        }
        return this.points;
    }

}
