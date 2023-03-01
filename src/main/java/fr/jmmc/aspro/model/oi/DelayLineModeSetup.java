
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the specific delay line setup associated to a delay line mode
 *             
 * 
 * <p>Java class for DelayLineModeSetup complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="DelayLineModeSetup"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="maximumThrow" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="mode" use="required" type="{http://www.w3.org/2001/XMLSchema}IDREF" /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DelayLineModeSetup", propOrder = {
    "maximumThrow"
})
public class DelayLineModeSetup
    extends OIBase
{

    protected double maximumThrow;
    @XmlAttribute(name = "mode", required = true)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected DelayLineMode mode;

    /**
     * Gets the value of the maximumThrow property.
     * 
     */
    public double getMaximumThrow() {
        return maximumThrow;
    }

    /**
     * Sets the value of the maximumThrow property.
     * 
     */
    public void setMaximumThrow(double value) {
        this.maximumThrow = value;
    }

    /**
     * Gets the value of the mode property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public DelayLineMode getMode() {
        return mode;
    }

    /**
     * Sets the value of the mode property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setMode(DelayLineMode value) {
        this.mode = value;
    }
    
//--simple--preserve
    @Override
    public String toString() {
        return "DelayLineModeSetup{" + "maximumThrow=" + maximumThrow + ", mode=" + mode + '}';
    }

//--simple--preserve

}
