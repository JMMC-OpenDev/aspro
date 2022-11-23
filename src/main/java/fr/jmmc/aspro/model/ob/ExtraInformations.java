
package fr.jmmc.aspro.model.ob;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the optional informations.
 *             
 * 
 * <p>Java class for ExtraInformations complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ExtraInformations"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="parameter" type="{http://www.jmmc.fr/aspro-ob/0.1}BaseValue" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="field" type="{http://www.jmmc.fr/aspro-ob/0.1}BaseValue" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ExtraInformations", propOrder = {
    "parameters",
    "fields"
})
public class ExtraInformations
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

}
