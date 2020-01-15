
package fr.jmmc.aspro.model.rawobs;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the raw observation
 *             
 * 
 * <p>Java class for RawObservation complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="RawObservation"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="obsId" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="type" type="{http://www.jmmc.fr/aspro-raw-obs/0.1}ObservationType"/&gt;
 *         &lt;element name="parentId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="programId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="interferometerName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="interferometerVersion" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="stations" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="pops" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="channels" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="instrumentName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="instrumentMode" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="instrumentSubMode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="targetName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="targetRa" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="targetDec" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="mjdStart" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="mjdEnd" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="projectedBaselines" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "RawObservation", propOrder = {
    "obsId",
    "type",
    "parentId",
    "programId",
    "interferometerName",
    "interferometerVersion",
    "stations",
    "pops",
    "channels",
    "instrumentName",
    "instrumentMode",
    "instrumentSubMode",
    "targetName",
    "targetRa",
    "targetDec",
    "mjdStart",
    "mjdEnd",
    "projectedBaselines"
})
public class RawObservation
    extends OIBase
{

    @XmlElement(required = true)
    protected String obsId;
    @XmlElement(required = true)
    @XmlSchemaType(name = "string")
    protected ObservationType type;
    protected String parentId;
    protected String programId;
    @XmlElement(required = true)
    protected String interferometerName;
    protected String interferometerVersion;
    @XmlElement(required = true)
    protected String stations;
    protected String pops;
    protected String channels;
    @XmlElement(required = true)
    protected String instrumentName;
    @XmlElement(required = true)
    protected String instrumentMode;
    protected String instrumentSubMode;
    @XmlElement(required = true)
    protected String targetName;
    protected double targetRa;
    protected double targetDec;
    protected double mjdStart;
    protected double mjdEnd;
    @XmlElement(required = true)
    protected String projectedBaselines;

    /**
     * Gets the value of the obsId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getObsId() {
        return obsId;
    }

    /**
     * Sets the value of the obsId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setObsId(String value) {
        this.obsId = value;
    }

    /**
     * Gets the value of the type property.
     * 
     * @return
     *     possible object is
     *     {@link ObservationType }
     *     
     */
    public ObservationType getType() {
        return type;
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value
     *     allowed object is
     *     {@link ObservationType }
     *     
     */
    public void setType(ObservationType value) {
        this.type = value;
    }

    /**
     * Gets the value of the parentId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getParentId() {
        return parentId;
    }

    /**
     * Sets the value of the parentId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setParentId(String value) {
        this.parentId = value;
    }

    /**
     * Gets the value of the programId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getProgramId() {
        return programId;
    }

    /**
     * Sets the value of the programId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setProgramId(String value) {
        this.programId = value;
    }

    /**
     * Gets the value of the interferometerName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInterferometerName() {
        return interferometerName;
    }

    /**
     * Sets the value of the interferometerName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInterferometerName(String value) {
        this.interferometerName = value;
    }

    /**
     * Gets the value of the interferometerVersion property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInterferometerVersion() {
        return interferometerVersion;
    }

    /**
     * Sets the value of the interferometerVersion property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInterferometerVersion(String value) {
        this.interferometerVersion = value;
    }

    /**
     * Gets the value of the stations property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getStations() {
        return stations;
    }

    /**
     * Sets the value of the stations property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setStations(String value) {
        this.stations = value;
    }

    /**
     * Gets the value of the pops property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPops() {
        return pops;
    }

    /**
     * Sets the value of the pops property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPops(String value) {
        this.pops = value;
    }

    /**
     * Gets the value of the channels property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getChannels() {
        return channels;
    }

    /**
     * Sets the value of the channels property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setChannels(String value) {
        this.channels = value;
    }

    /**
     * Gets the value of the instrumentName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInstrumentName() {
        return instrumentName;
    }

    /**
     * Sets the value of the instrumentName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInstrumentName(String value) {
        this.instrumentName = value;
    }

    /**
     * Gets the value of the instrumentMode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInstrumentMode() {
        return instrumentMode;
    }

    /**
     * Sets the value of the instrumentMode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInstrumentMode(String value) {
        this.instrumentMode = value;
    }

    /**
     * Gets the value of the instrumentSubMode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInstrumentSubMode() {
        return instrumentSubMode;
    }

    /**
     * Sets the value of the instrumentSubMode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInstrumentSubMode(String value) {
        this.instrumentSubMode = value;
    }

    /**
     * Gets the value of the targetName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTargetName() {
        return targetName;
    }

    /**
     * Sets the value of the targetName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTargetName(String value) {
        this.targetName = value;
    }

    /**
     * Gets the value of the targetRa property.
     * 
     */
    public double getTargetRa() {
        return targetRa;
    }

    /**
     * Sets the value of the targetRa property.
     * 
     */
    public void setTargetRa(double value) {
        this.targetRa = value;
    }

    /**
     * Gets the value of the targetDec property.
     * 
     */
    public double getTargetDec() {
        return targetDec;
    }

    /**
     * Sets the value of the targetDec property.
     * 
     */
    public void setTargetDec(double value) {
        this.targetDec = value;
    }

    /**
     * Gets the value of the mjdStart property.
     * 
     */
    public double getMjdStart() {
        return mjdStart;
    }

    /**
     * Sets the value of the mjdStart property.
     * 
     */
    public void setMjdStart(double value) {
        this.mjdStart = value;
    }

    /**
     * Gets the value of the mjdEnd property.
     * 
     */
    public double getMjdEnd() {
        return mjdEnd;
    }

    /**
     * Sets the value of the mjdEnd property.
     * 
     */
    public void setMjdEnd(double value) {
        this.mjdEnd = value;
    }

    /**
     * Gets the value of the projectedBaselines property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getProjectedBaselines() {
        return projectedBaselines;
    }

    /**
     * Sets the value of the projectedBaselines property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setProjectedBaselines(String value) {
        this.projectedBaselines = value;
    }
    
//--simple--preserve
    /**
     * Return the exposure time in seconds
     * @return exposure time in seconds
     */
    public int getExpTime() {
        return (int) Math.round((getMjdEnd() - getMjdStart()) * 86400.0);
    }

    @Override
    public final String toString() {
        return "RawObservation [" + getObsId() + "] {"
                + "type: " + getType()
                + ", parentId: " + getParentId()
                + ", programId: " + getProgramId()
                + ", interferometerName: " + getInterferometerName()
                + ", interferometerVersion: " + getInterferometerVersion()
                + ", stations: " + getStations()
                + ", pops: " + getPops()
                + ", channels: " + getChannels()
                + ", instrumentName: " + getInstrumentName()
                + ", instrumentMode: " + getInstrumentMode()
                + ", instrumentSubMode: " + getInstrumentSubMode()
                + ", targetName: " + getTargetName()
                + ", targetRA: " + getTargetRa()
                + ", targetDEC: " + getTargetDec()
                + ", mjdStart: " + getMjdStart()
                + ", mjdEnd: " + getMjdEnd()
                + ", projectedBaselines: " + getProjectedBaselines()
                + " | expTime: " + getExpTime()
                + "}";
    }

    /**
     * Return an HTML representation of the RawObservation used by tooltips in the given string buffer
     * @param sb string buffer to fill
     */
    public final void toHtml(final StringBuilder sb) {
        sb.append("<b>Observation Log<br>ID: ").append(getObsId());
        sb.append("</b><br>Type: ").append(getType());

        // skip parent id
        if (getProgramId() != null) {
            sb.append("<br>Program ID: ").append(getProgramId());
        }

        // Interferometer:
        sb.append("<hr><b>Interferometer: ").append(getInterferometerName());
        if (getInterferometerVersion() != null) {
            sb.append(" (").append(getInterferometerVersion()).append(")");
        }

        // Interferometer setup:
        sb.append("<br>Baseline: ").append(getStations()).append("</b>");
        if (getPops() != null) {
            sb.append("<br>PoPs: ").append(getPops());
        }
        if (getChannels() != null) {
            sb.append("<br>Channels: ").append(getInterferometerName());
        }

        // Instrument:
        sb.append("<b>Instrument: ").append(getInstrumentName());
        sb.append("<br>Mode: ").append(getInstrumentMode()).append("</b>");
        if (getChannels() != null) {
            sb.append("<br>sub Mode: ").append(getInstrumentSubMode());
        }

        // Target:
        sb.append("<hr><b>Target: ");
        if (getTargetName() != null) {
            sb.append(getTargetName());
        }
        sb.append("</b>");
        // note: generated targets for baseline limits do not have RA/DEC as string (useless):
        sb.append("<br><b>Coords</b>: ").append(getTargetRa()).append(' ').append(getTargetDec());

        // Obs time range:
        sb.append("<hr>MJD: ").append(getMjdStart());
        if (getExpTime() > 0.0) {
            sb.append("<br>Exp. time: ").append(getExpTime()).append(" s");
        }
    }

//--simple--preserve

}
