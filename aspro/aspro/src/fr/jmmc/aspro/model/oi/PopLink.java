
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes a link to a PoP with its optical length
 *       
 * 
 * <p>Java class for PopLink complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="PopLink">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="pop" type="{http://www.w3.org/2001/XMLSchema}IDREF"/>
 *         &lt;element name="opticalLength" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PopLink", propOrder = {
    "pop",
    "opticalLength"
})
public class PopLink
    extends OIBase
{

    @XmlElement(required = true, type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected Pop pop;
    protected double opticalLength;

    /**
     * Gets the value of the pop property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public Pop getPop() {
        return pop;
    }

    /**
     * Sets the value of the pop property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setPop(Pop value) {
        this.pop = value;
    }

    /**
     * Gets the value of the opticalLength property.
     * 
     */
    public double getOpticalLength() {
        return opticalLength;
    }

    /**
     * Sets the value of the opticalLength property.
     * 
     */
    public void setOpticalLength(double value) {
        this.opticalLength = value;
    }

}
