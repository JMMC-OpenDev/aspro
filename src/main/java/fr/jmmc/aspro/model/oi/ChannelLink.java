
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
 *                 This type describes a link to a channel with its optical length
 *             
 * 
 * <p>Java class for ChannelLink complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ChannelLink">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="channel" type="{http://www.w3.org/2001/XMLSchema}IDREF"/>
 *         &lt;element name="opticalLength" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="delayLine" type="{http://www.w3.org/2001/XMLSchema}IDREF" minOccurs="0"/>
 *         &lt;element name="maximumThrowLow" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *         &lt;element name="maximumThrowHigh" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ChannelLink", propOrder = {
    "channel",
    "opticalLength",
    "delayLine",
    "maximumThrowLow",
    "maximumThrowHigh"
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
    protected Double maximumThrowLow;
    protected Double maximumThrowHigh;

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
     * Gets the value of the maximumThrowLow property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getMaximumThrowLow() {
        return maximumThrowLow;
    }

    /**
     * Sets the value of the maximumThrowLow property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setMaximumThrowLow(Double value) {
        this.maximumThrowLow = value;
    }

    /**
     * Gets the value of the maximumThrowHigh property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getMaximumThrowHigh() {
        return maximumThrowHigh;
    }

    /**
     * Sets the value of the maximumThrowHigh property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setMaximumThrowHigh(Double value) {
        this.maximumThrowHigh = value;
    }

}
