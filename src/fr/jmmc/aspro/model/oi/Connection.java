
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
 *         This type describes a connection between a station and a channel to give its optical length
 *       
 * 
 * <p>Java class for Connection complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Connection">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="station" type="{http://www.w3.org/2001/XMLSchema}IDREF"/>
 *         &lt;element name="channel" type="{http://www.w3.org/2001/XMLSchema}IDREF"/>
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
@XmlType(name = "Connection", propOrder = {
    "station",
    "channel",
    "opticalLength"
})
public class Connection
    extends OIBase
{

    @XmlElement(required = true, type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected Station station;
    @XmlElement(required = true, type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected Channel channel;
    protected double opticalLength;

    /**
     * Gets the value of the station property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public Station getStation() {
        return station;
    }

    /**
     * Sets the value of the station property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setStation(Station value) {
        this.station = value;
    }

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

}
