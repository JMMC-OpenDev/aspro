
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
 *                 This type describes the Moon Pointing Restrictions (angular separation depending on FLI ...)
 *             
 * 
 * <p>Java class for MoonPointingRestriction complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="MoonPointingRestriction"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="warningThreshold" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="restriction" type="{http://www.jmmc.fr/aspro-oi/0.1}MoonRestriction" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "MoonPointingRestriction", propOrder = {
    "warningThreshold",
    "restrictions"
})
public class MoonPointingRestriction
    extends OIBase
{

    protected double warningThreshold;
    @XmlElement(name = "restriction")
    protected List<MoonRestriction> restrictions;

    /**
     * Gets the value of the warningThreshold property.
     * 
     */
    public double getWarningThreshold() {
        return warningThreshold;
    }

    /**
     * Sets the value of the warningThreshold property.
     * 
     */
    public void setWarningThreshold(double value) {
        this.warningThreshold = value;
    }

    /**
     * Gets the value of the restrictions property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the restrictions property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getRestrictions().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link MoonRestriction }
     * 
     * 
     */
    public List<MoonRestriction> getRestrictions() {
        if (restrictions == null) {
            restrictions = new ArrayList<MoonRestriction>();
        }
        return this.restrictions;
    }

}
