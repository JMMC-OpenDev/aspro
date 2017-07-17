
package fr.jmmc.aspro.model.ob;

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
 *                 This type describes the observing block
 *             
 * 
 * <p>Java class for ObservingBlockDefinition complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ObservingBlockDefinition"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="schemaVersion" type="{http://www.w3.org/2001/XMLSchema}float"/&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="interferometerConfiguration" type="{http://www.jmmc.fr/aspro-ob/0.1}InterferometerConfiguration"/&gt;
 *         &lt;element name="instrumentConfiguration" type="{http://www.jmmc.fr/aspro-ob/0.1}InstrumentConfiguration"/&gt;
 *         &lt;element name="observationConfiguration" type="{http://www.jmmc.fr/aspro-ob/0.1}ObservationConfiguration" maxOccurs="unbounded"/&gt;
 *         &lt;element name="observationSchedule" type="{http://www.jmmc.fr/aspro-ob/0.1}ObservationSchedule"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ObservingBlockDefinition", propOrder = {
    "schemaVersion",
    "name",
    "interferometerConfiguration",
    "instrumentConfiguration",
    "observationConfigurations",
    "observationSchedule"
})
@XmlRootElement(name = "observingBlockDefinition")
public class ObservingBlockDefinition
    extends OIBase
{

    protected float schemaVersion;
    @XmlElement(required = true)
    protected String name;
    @XmlElement(required = true)
    protected InterferometerConfiguration interferometerConfiguration;
    @XmlElement(required = true)
    protected InstrumentConfiguration instrumentConfiguration;
    @XmlElement(name = "observationConfiguration", required = true)
    protected List<ObservationConfiguration> observationConfigurations;
    @XmlElement(required = true)
    protected ObservationSchedule observationSchedule;

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
     * Gets the value of the interferometerConfiguration property.
     * 
     * @return
     *     possible object is
     *     {@link InterferometerConfiguration }
     *     
     */
    public InterferometerConfiguration getInterferometerConfiguration() {
        return interferometerConfiguration;
    }

    /**
     * Sets the value of the interferometerConfiguration property.
     * 
     * @param value
     *     allowed object is
     *     {@link InterferometerConfiguration }
     *     
     */
    public void setInterferometerConfiguration(InterferometerConfiguration value) {
        this.interferometerConfiguration = value;
    }

    /**
     * Gets the value of the instrumentConfiguration property.
     * 
     * @return
     *     possible object is
     *     {@link InstrumentConfiguration }
     *     
     */
    public InstrumentConfiguration getInstrumentConfiguration() {
        return instrumentConfiguration;
    }

    /**
     * Sets the value of the instrumentConfiguration property.
     * 
     * @param value
     *     allowed object is
     *     {@link InstrumentConfiguration }
     *     
     */
    public void setInstrumentConfiguration(InstrumentConfiguration value) {
        this.instrumentConfiguration = value;
    }

    /**
     * Gets the value of the observationConfigurations property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the observationConfigurations property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getObservationConfigurations().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ObservationConfiguration }
     * 
     * 
     */
    public List<ObservationConfiguration> getObservationConfigurations() {
        if (observationConfigurations == null) {
            observationConfigurations = new ArrayList<ObservationConfiguration>();
        }
        return this.observationConfigurations;
    }

    /**
     * Gets the value of the observationSchedule property.
     * 
     * @return
     *     possible object is
     *     {@link ObservationSchedule }
     *     
     */
    public ObservationSchedule getObservationSchedule() {
        return observationSchedule;
    }

    /**
     * Sets the value of the observationSchedule property.
     * 
     * @param value
     *     allowed object is
     *     {@link ObservationSchedule }
     *     
     */
    public void setObservationSchedule(ObservationSchedule value) {
        this.observationSchedule = value;
    }
    
//--simple--preserve

        
    public ObservingBlockDefinition() {
        // default values:
        setName("default");
        setSchemaVersion(2017.7f);
    }

    
//--simple--preserve

}
