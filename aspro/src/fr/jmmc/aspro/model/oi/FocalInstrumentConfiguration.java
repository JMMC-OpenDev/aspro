
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
 *         This type describes a focal instrument configuration (AMBER : UT1-UT2-UT4, A0-K0-G1, MIDI : UT1-UT3-102m, E0-I1-68.8m)
 *       
 * 
 * <p>Java class for FocalInstrumentConfiguration complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FocalInstrumentConfiguration">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="focalInstrument" type="{http://www.w3.org/2001/XMLSchema}IDREF"/>
 *         &lt;element name="configuration" type="{http://www.jmmc.fr/aspro-oi/0.1}FocalInstrumentConfigurationItem" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FocalInstrumentConfiguration", propOrder = {
    "focalInstrument",
    "configurations"
})
public class FocalInstrumentConfiguration
    extends OIBase
{

    @XmlElement(required = true, type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected FocalInstrument focalInstrument;
    @XmlElement(name = "configuration")
    protected List<FocalInstrumentConfigurationItem> configurations;

    /**
     * Gets the value of the focalInstrument property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public FocalInstrument getFocalInstrument() {
        return focalInstrument;
    }

    /**
     * Sets the value of the focalInstrument property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setFocalInstrument(FocalInstrument value) {
        this.focalInstrument = value;
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
     * {@link FocalInstrumentConfigurationItem }
     * 
     * 
     */
    public List<FocalInstrumentConfigurationItem> getConfigurations() {
        if (configurations == null) {
            configurations = new ArrayList<FocalInstrumentConfigurationItem>();
        }
        return this.configurations;
    }

}
