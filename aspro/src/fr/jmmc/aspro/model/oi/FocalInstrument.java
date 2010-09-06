
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes a focal instrument (AMBER, MIDI ...)
 *       
 * 
 * <p>Java class for FocalInstrument complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FocalInstrument">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}ID"/>
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="numberChannels" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="defaultSamplingTime" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="transmission" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="dit" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="ron" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="detectorSaturation" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="instrumentVisibility" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="instrumentVisibilityBias" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="instrumentPhaseBias" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="nbPixInterferometry" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="nbPixPhotometry" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="fracFluxInInterferometry" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="mode" type="{http://www.jmmc.fr/aspro-oi/0.1}FocalInstrumentMode" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FocalInstrument", propOrder = {
    "name",
    "description",
    "numberChannels",
    "defaultSamplingTime",
    "transmission",
    "dit",
    "ron",
    "detectorSaturation",
    "instrumentVisibility",
    "instrumentVisibilityBias",
    "instrumentPhaseBias",
    "nbPixInterferometry",
    "nbPixPhotometry",
    "fracFluxInInterferometry",
    "modes"
})
public class FocalInstrument
    extends OIBase
{

    @XmlElement(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String name;
    @XmlElement(required = true)
    protected String description;
    protected int numberChannels;
    protected int defaultSamplingTime;
    protected double transmission;
    protected double dit;
    protected double ron;
    protected double detectorSaturation;
    protected double instrumentVisibility;
    protected double instrumentVisibilityBias;
    protected double instrumentPhaseBias;
    protected int nbPixInterferometry;
    protected int nbPixPhotometry;
    protected double fracFluxInInterferometry;
    @XmlElement(name = "mode")
    protected List<FocalInstrumentMode> modes;

    /**
     * Gets the value of the name property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the value of the name property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setName(String value) {
        this.name = value;
    }

    /**
     * Gets the value of the description property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDescription() {
        return description;
    }

    /**
     * Sets the value of the description property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDescription(String value) {
        this.description = value;
    }

    /**
     * Gets the value of the numberChannels property.
     * 
     */
    public int getNumberChannels() {
        return numberChannels;
    }

    /**
     * Sets the value of the numberChannels property.
     * 
     */
    public void setNumberChannels(int value) {
        this.numberChannels = value;
    }

    /**
     * Gets the value of the defaultSamplingTime property.
     * 
     */
    public int getDefaultSamplingTime() {
        return defaultSamplingTime;
    }

    /**
     * Sets the value of the defaultSamplingTime property.
     * 
     */
    public void setDefaultSamplingTime(int value) {
        this.defaultSamplingTime = value;
    }

    /**
     * Gets the value of the transmission property.
     * 
     */
    public double getTransmission() {
        return transmission;
    }

    /**
     * Sets the value of the transmission property.
     * 
     */
    public void setTransmission(double value) {
        this.transmission = value;
    }

    /**
     * Gets the value of the dit property.
     * 
     */
    public double getDit() {
        return dit;
    }

    /**
     * Sets the value of the dit property.
     * 
     */
    public void setDit(double value) {
        this.dit = value;
    }

    /**
     * Gets the value of the ron property.
     * 
     */
    public double getRon() {
        return ron;
    }

    /**
     * Sets the value of the ron property.
     * 
     */
    public void setRon(double value) {
        this.ron = value;
    }

    /**
     * Gets the value of the detectorSaturation property.
     * 
     */
    public double getDetectorSaturation() {
        return detectorSaturation;
    }

    /**
     * Sets the value of the detectorSaturation property.
     * 
     */
    public void setDetectorSaturation(double value) {
        this.detectorSaturation = value;
    }

    /**
     * Gets the value of the instrumentVisibility property.
     * 
     */
    public double getInstrumentVisibility() {
        return instrumentVisibility;
    }

    /**
     * Sets the value of the instrumentVisibility property.
     * 
     */
    public void setInstrumentVisibility(double value) {
        this.instrumentVisibility = value;
    }

    /**
     * Gets the value of the instrumentVisibilityBias property.
     * 
     */
    public double getInstrumentVisibilityBias() {
        return instrumentVisibilityBias;
    }

    /**
     * Sets the value of the instrumentVisibilityBias property.
     * 
     */
    public void setInstrumentVisibilityBias(double value) {
        this.instrumentVisibilityBias = value;
    }

    /**
     * Gets the value of the instrumentPhaseBias property.
     * 
     */
    public double getInstrumentPhaseBias() {
        return instrumentPhaseBias;
    }

    /**
     * Sets the value of the instrumentPhaseBias property.
     * 
     */
    public void setInstrumentPhaseBias(double value) {
        this.instrumentPhaseBias = value;
    }

    /**
     * Gets the value of the nbPixInterferometry property.
     * 
     */
    public int getNbPixInterferometry() {
        return nbPixInterferometry;
    }

    /**
     * Sets the value of the nbPixInterferometry property.
     * 
     */
    public void setNbPixInterferometry(int value) {
        this.nbPixInterferometry = value;
    }

    /**
     * Gets the value of the nbPixPhotometry property.
     * 
     */
    public int getNbPixPhotometry() {
        return nbPixPhotometry;
    }

    /**
     * Sets the value of the nbPixPhotometry property.
     * 
     */
    public void setNbPixPhotometry(int value) {
        this.nbPixPhotometry = value;
    }

    /**
     * Gets the value of the fracFluxInInterferometry property.
     * 
     */
    public double getFracFluxInInterferometry() {
        return fracFluxInInterferometry;
    }

    /**
     * Sets the value of the fracFluxInInterferometry property.
     * 
     */
    public void setFracFluxInInterferometry(double value) {
        this.fracFluxInInterferometry = value;
    }

    /**
     * Gets the value of the modes property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the modes property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getModes().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link FocalInstrumentMode }
     * 
     * 
     */
    public List<FocalInstrumentMode> getModes() {
        if (modes == null) {
            modes = new ArrayList<FocalInstrumentMode>();
        }
        return this.modes;
    }
    
//--simple--preserve

    @Override
    public String toString() {
      return "FocalInstrument [" + ((this.name != null) ? this.name : "undefined") + "]";
    }

//--simple--preserve

}
