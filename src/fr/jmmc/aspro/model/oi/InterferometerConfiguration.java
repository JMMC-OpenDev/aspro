
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes the configuration for a given interferometer and its instruments
 *       
 * 
 * <p>Java class for InterferometerConfiguration complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="InterferometerConfiguration">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="version" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="interferometer" type="{http://www.w3.org/2001/XMLSchema}IDREF"/>
 *         &lt;element name="instrument" type="{http://www.jmmc.fr/aspro-oi/0.1}FocalInstrumentConfiguration" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "InterferometerConfiguration", propOrder = {
    "name",
    "version",
    "interferometer",
    "instruments"
})
public class InterferometerConfiguration
    extends OIBase
{

    protected String name;
    protected String version;
    @XmlElement(required = true, type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected InterferometerDescription interferometer;
    @XmlElement(name = "instrument")
    protected List<FocalInstrumentConfiguration> instruments;

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
     * Gets the value of the version property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getVersion() {
        return version;
    }

    /**
     * Sets the value of the version property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setVersion(String value) {
        this.version = value;
    }

    /**
     * Gets the value of the interferometer property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public InterferometerDescription getInterferometer() {
        return interferometer;
    }

    /**
     * Sets the value of the interferometer property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setInterferometer(InterferometerDescription value) {
        this.interferometer = value;
    }

    /**
     * Gets the value of the instruments property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the instruments property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getInstruments().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link FocalInstrumentConfiguration }
     * 
     * 
     */
    public List<FocalInstrumentConfiguration> getInstruments() {
        if (instruments == null) {
            instruments = new ArrayList<FocalInstrumentConfiguration>();
        }
        return this.instruments;
    }
    
//--simple--preserve

    @Override
    public String toString() {
      return "InterferometerConfiguration : " + ((this.name != null) ? this.name : "undefined");
    }

//--simple--preserve

}
