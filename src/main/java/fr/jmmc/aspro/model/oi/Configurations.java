
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
 *                 This type describes the list of interferometer files
 *             
 * 
 * <p>Java class for InterferometerFileCollection complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="InterferometerFileCollection">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="minVersion" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="interferometerFile" type="{http://www.jmmc.fr/aspro-oi/0.1}InterferometerFile" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "InterferometerFileCollection", propOrder = {
    "minVersion",
    "interferometerFiles"
})
@XmlRootElement(name = "configurations")
public class Configurations
    extends OIBase
{

    @XmlElement(required = true)
    protected String minVersion;
    @XmlElement(name = "interferometerFile", required = true)
    protected List<InterferometerFile> interferometerFiles;

    /**
     * Gets the value of the minVersion property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMinVersion() {
        return minVersion;
    }

    /**
     * Sets the value of the minVersion property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMinVersion(String value) {
        this.minVersion = value;
    }

    /**
     * Gets the value of the interferometerFiles property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the interferometerFiles property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getInterferometerFiles().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link InterferometerFile }
     * 
     * 
     */
    public List<InterferometerFile> getInterferometerFiles() {
        if (interferometerFiles == null) {
            interferometerFiles = new ArrayList<InterferometerFile>();
        }
        return this.interferometerFiles;
    }

}
