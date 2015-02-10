
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the runtime context containing PIVOT parameters
 *             
 * 
 * <p>Java class for ObservationContext complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ObservationContext">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="operation" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="interferometerEditable" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
 *         &lt;element name="periodEditable" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
 *         &lt;element name="instrumentEditable" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
 *         &lt;element name="popsEditable" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
 *         &lt;element name="configurationsEditable" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
 *         &lt;element name="nightEditable" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
 *         &lt;element name="dateEditable" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
 *         &lt;element name="minElevationEditable" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
 *         &lt;element name="targetsEditable" type="{http://www.w3.org/2001/XMLSchema}boolean"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ObservationContext", propOrder = {
    "operation",
    "interferometerEditable",
    "periodEditable",
    "instrumentEditable",
    "popsEditable",
    "configurationsEditable",
    "nightEditable",
    "dateEditable",
    "minElevationEditable",
    "targetsEditable"
})
public class ObservationContext
    extends OIBase
{

    @XmlElement(required = true)
    protected String operation;
    protected boolean interferometerEditable;
    protected boolean periodEditable;
    protected boolean instrumentEditable;
    protected boolean popsEditable;
    protected boolean configurationsEditable;
    protected boolean nightEditable;
    protected boolean dateEditable;
    protected boolean minElevationEditable;
    protected boolean targetsEditable;

    /**
     * Gets the value of the operation property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getOperation() {
        return operation;
    }

    /**
     * Sets the value of the operation property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setOperation(String value) {
        this.operation = value;
    }

    /**
     * Gets the value of the interferometerEditable property.
     * 
     */
    public boolean isInterferometerEditable() {
        return interferometerEditable;
    }

    /**
     * Sets the value of the interferometerEditable property.
     * 
     */
    public void setInterferometerEditable(boolean value) {
        this.interferometerEditable = value;
    }

    /**
     * Gets the value of the periodEditable property.
     * 
     */
    public boolean isPeriodEditable() {
        return periodEditable;
    }

    /**
     * Sets the value of the periodEditable property.
     * 
     */
    public void setPeriodEditable(boolean value) {
        this.periodEditable = value;
    }

    /**
     * Gets the value of the instrumentEditable property.
     * 
     */
    public boolean isInstrumentEditable() {
        return instrumentEditable;
    }

    /**
     * Sets the value of the instrumentEditable property.
     * 
     */
    public void setInstrumentEditable(boolean value) {
        this.instrumentEditable = value;
    }

    /**
     * Gets the value of the popsEditable property.
     * 
     */
    public boolean isPopsEditable() {
        return popsEditable;
    }

    /**
     * Sets the value of the popsEditable property.
     * 
     */
    public void setPopsEditable(boolean value) {
        this.popsEditable = value;
    }

    /**
     * Gets the value of the configurationsEditable property.
     * 
     */
    public boolean isConfigurationsEditable() {
        return configurationsEditable;
    }

    /**
     * Sets the value of the configurationsEditable property.
     * 
     */
    public void setConfigurationsEditable(boolean value) {
        this.configurationsEditable = value;
    }

    /**
     * Gets the value of the nightEditable property.
     * 
     */
    public boolean isNightEditable() {
        return nightEditable;
    }

    /**
     * Sets the value of the nightEditable property.
     * 
     */
    public void setNightEditable(boolean value) {
        this.nightEditable = value;
    }

    /**
     * Gets the value of the dateEditable property.
     * 
     */
    public boolean isDateEditable() {
        return dateEditable;
    }

    /**
     * Sets the value of the dateEditable property.
     * 
     */
    public void setDateEditable(boolean value) {
        this.dateEditable = value;
    }

    /**
     * Gets the value of the minElevationEditable property.
     * 
     */
    public boolean isMinElevationEditable() {
        return minElevationEditable;
    }

    /**
     * Sets the value of the minElevationEditable property.
     * 
     */
    public void setMinElevationEditable(boolean value) {
        this.minElevationEditable = value;
    }

    /**
     * Gets the value of the targetsEditable property.
     * 
     */
    public boolean isTargetsEditable() {
        return targetsEditable;
    }

    /**
     * Sets the value of the targetsEditable property.
     * 
     */
    public void setTargetsEditable(boolean value) {
        this.targetsEditable = value;
    }
    
//--simple--preserve

    /** Operation value 'NEW' = new observation */
    public final static String OPERATION_NEW = "NEW";
    /** Operation value 'ADD' = add targets only */
    public final static String OPERATION_ADD = "ADD";

    /**
     * @return true if the operation is 'NEW' = new observation
     */
    public boolean isOperationNew() {
        return (this.operation != null) && OPERATION_NEW.equalsIgnoreCase(this.operation);
    }

    /**
     * @return true if the operation is not 'NEW' (ie 'ADD') = add targets only
     */
    public boolean isOperationAdd() {
        return !isOperationNew();
    }
    
//--simple--preserve

}
