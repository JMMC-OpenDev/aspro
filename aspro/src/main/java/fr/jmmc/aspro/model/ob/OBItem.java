
package fr.jmmc.aspro.model.ob;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes an OB in the observation schedule
 *             
 * 
 * <p>Java class for OBItem complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="OBItem"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="ref" type="{http://www.w3.org/2001/XMLSchema}IDREF" /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "OBItem")
public class OBItem
    extends OIBase
{

    @XmlAttribute(name = "ref")
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected ObservationConfiguration ref;

    /**
     * Gets the value of the ref property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public ObservationConfiguration getRef() {
        return ref;
    }

    /**
     * Sets the value of the ref property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setRef(ObservationConfiguration value) {
        this.ref = value;
    }
    
//--simple--preserve
    
    public OBItem() {
        super();
    }
    
    public OBItem(ObservationConfiguration obsConf) {
        super();
        setRef(obsConf);
    }
    
//--simple--preserve

}
