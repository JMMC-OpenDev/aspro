
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
 *                 This type describes a link to a channel with its optical length
 *             
 * 
 * <p>Java class for ChannelLink complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ChannelLink"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="channel" type="{http://www.w3.org/2001/XMLSchema}IDREF"/&gt;
 *         &lt;element name="opticalLength" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="delayLine" type="{http://www.w3.org/2001/XMLSchema}IDREF" minOccurs="0"/&gt;
 *         &lt;element name="delayLineThrow" type="{http://www.jmmc.fr/aspro-oi/0.1}DelayLineRestrictionThrow" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ChannelLink", propOrder = {
    "channel",
    "opticalLength",
    "delayLine",
    "delayLineThrows"
})
public class ChannelLink
    extends OIBase
{

    @XmlElement(required = true, type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected Channel channel;
    protected double opticalLength;
    @XmlElement(type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected DelayLine delayLine;
    @XmlElement(name = "delayLineThrow")
    protected List<DelayLineRestrictionThrow> delayLineThrows;

    /**
     * Gets the value of the channel property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public Channel getChannel() {
        return channel;
    }

    /**
     * Sets the value of the channel property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setChannel(Channel value) {
        this.channel = value;
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

    /**
     * Gets the value of the delayLine property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public DelayLine getDelayLine() {
        return delayLine;
    }

    /**
     * Sets the value of the delayLine property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setDelayLine(DelayLine value) {
        this.delayLine = value;
    }

    /**
     * Gets the value of the delayLineThrows property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the delayLineThrows property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getDelayLineThrows().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link DelayLineRestrictionThrow }
     * 
     * 
     */
    public List<DelayLineRestrictionThrow> getDelayLineThrows() {
        if (delayLineThrows == null) {
            delayLineThrows = new ArrayList<DelayLineRestrictionThrow>();
        }
        return this.delayLineThrows;
    }

}
