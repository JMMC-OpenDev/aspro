
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlList;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes a focal instrument configuration item (AMBER : U1-U2-U4)
 *       
 * 
 * <p>Java class for FocalInstrumentConfigurationItem complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FocalInstrumentConfigurationItem">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="stations" type="{http://www.w3.org/2001/XMLSchema}IDREFS"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FocalInstrumentConfigurationItem", propOrder = {
    "name",
    "stations"
})
public class FocalInstrumentConfigurationItem
    extends OIBase
{

    protected String name;
    @XmlList
    @XmlElement(required = true, type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREFS")
    protected List<Station> stations;

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
     * Gets the value of the stations property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the stations property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getStations().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Object }
     * 
     * 
     */
    public List<Station> getStations() {
        if (stations == null) {
            stations = new ArrayList<Station>();
        }
        return this.stations;
    }
    
//--simple--preserve

    @Override
    public String toString() {
      if (getName() == null) {
        generateName();
      }
      return "FocalInstrumentConfigurationItem [" + getName() + "]";
    }

    private void generateName() {
      synchronized(this) {
        if (getName() == null) {
          final StringBuilder sb = new StringBuilder();
          for (Station s : getStations()) {
            sb.append(s.getName()).append(" ");
          }
          sb.deleteCharAt(sb.length() - 1);
          setName(sb.toString());
        }
      }
    }

//--simple--preserve

}
