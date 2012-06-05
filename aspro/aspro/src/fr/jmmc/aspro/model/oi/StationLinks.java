
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
 *         This type describes the links between a station and the channels
 *       
 * 
 * <p>Java class for StationLinks complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="StationLinks">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="station" type="{http://www.w3.org/2001/XMLSchema}IDREF"/>
 *         &lt;element name="channelLink" type="{http://www.jmmc.fr/aspro-oi/0.1}ChannelLink" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "StationLinks", propOrder = {
    "station",
    "channelLinks"
})
public class StationLinks
    extends OIBase
{

    @XmlElement(required = true, type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected Station station;
    @XmlElement(name = "channelLink", required = true)
    protected List<ChannelLink> channelLinks;

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
     * Gets the value of the channelLinks property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the channelLinks property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getChannelLinks().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ChannelLink }
     * 
     * 
     */
    public List<ChannelLink> getChannelLinks() {
        if (channelLinks == null) {
            channelLinks = new ArrayList<ChannelLink>();
        }
        return this.channelLinks;
    }

}
