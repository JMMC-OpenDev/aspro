
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
 *         This type describes a observation collection
 *       
 * 
 * <p>Java class for ObservationCollection complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ObservationCollection">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="schemaVersion" type="{http://www.w3.org/2001/XMLSchema}float"/>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="observation" type="{http://www.jmmc.fr/aspro-oi/0.1}ObservationSetting" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ObservationCollection", propOrder = {
    "schemaVersion",
    "name",
    "description",
    "observations"
})
@XmlRootElement(name = "observationCollection")
public class ObservationCollection
    extends OIBase
{

    protected float schemaVersion;
    @XmlElement(required = true)
    protected String name;
    protected String description;
    @XmlElement(name = "observation", required = true)
    protected List<ObservationSetting> observations;

    /**
     * Gets the value of the schemaVersion property.
     * 
     */
    public float getSchemaVersion() {
        return schemaVersion;
    }

    /**
     * Sets the value of the schemaVersion property.
     * 
     */
    public void setSchemaVersion(float value) {
        this.schemaVersion = value;
    }

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
     * Gets the value of the description property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDescription() {
        return description;
    }

    /**
     * Sets the value of the description property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDescription(String value) {
        this.description = value;
    }

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
     * {@link ObservationSetting }
     * 
     * 
     */
    public List<ObservationSetting> getObservations() {
        if (observations == null) {
            observations = new ArrayList<ObservationSetting>();
        }
        return this.observations;
    }
    
//--simple--preserve
  /**
   * Return the size of this observation collection
   * @return number of observations
   */
  public int size() {
    return getObservations().size();
  }

  /**
   * Return true if there is only 1 observation in this observation collection
   * @return true if there is only 1 observation
   */
  public boolean isSingle() {
    return size() == 1;
  }

  /**
   * Return the first observation
   * @return first observation
   */
  public ObservationSetting getFirstObservation() {
    return getObservations().get(0);
  }

  /** observation version (read only) */
  @javax.xml.bind.annotation.XmlTransient
  private fr.jmmc.aspro.model.ObservationVersion version = null;

  /**
   * Return the observation collection version
   * @return observation collection version
   */
  public final fr.jmmc.aspro.model.ObservationVersion getVersion() {
    return this.version;
  }

  /**
   * Return the observation collection version
   * @param version observation collection version
   */
  public final void setVersion(final fr.jmmc.aspro.model.ObservationVersion version) {
    this.version = version;
  }

  @Override
  public final String toString() {
    return "ObservationCollection[" + ((this.name != null) ? this.name : "undefined") + "] (" + size() + ")";
  }
//--simple--preserve

}
