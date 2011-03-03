
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
 *         This type describes the optional calibrator information (uniform disk diameters).
 *       
 * 
 * <p>Java class for CalibratorInformations complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CalibratorInformations">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="parameter" type="{http://www.jmmc.fr/aspro-oi/0.1}BaseValue" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element name="field" type="{http://www.jmmc.fr/aspro-oi/0.1}BaseValue" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CalibratorInformations", propOrder = {
    "parameters",
    "fields"
})
public class CalibratorInformations
    extends OIBase
{

    @XmlElement(name = "parameter")
    protected List<BaseValue> parameters;
    @XmlElement(name = "field")
    protected List<BaseValue> fields;

    /**
     * Gets the value of the parameters property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the parameters property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getParameters().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link BaseValue }
     * 
     * 
     */
    public List<BaseValue> getParameters() {
        if (parameters == null) {
            parameters = new ArrayList<BaseValue>();
        }
        return this.parameters;
    }

    /**
     * Gets the value of the fields property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the fields property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getFields().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link BaseValue }
     * 
     * 
     */
    public List<BaseValue> getFields() {
        if (fields == null) {
            fields = new ArrayList<BaseValue>();
        }
        return this.fields;
    }
    
//--simple--preserve

  @Override
  public final String toString() {
    return "CalibratorInformations[\nParameters : " + getParameters() + "\nFields: " + getFields() + "\n]";
  }

//--simple--preserve

}
