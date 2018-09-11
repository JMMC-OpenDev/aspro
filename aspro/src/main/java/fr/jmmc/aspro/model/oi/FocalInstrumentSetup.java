
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes a focal instrument setup ie an instrument operation setup (including noise parameters)
 *             
 * 
 * <p>Java class for FocalInstrumentSetup complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FocalInstrumentSetup"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}ID"/&gt;
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="defaultTotalIntegrationTime" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="defaultSamplingTime" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/&gt;
 *         &lt;element name="dit" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="frameRatio" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="ron" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="detectorSaturation" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="quantumEfficiency" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="fracFluxInInterferometry" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="fracFluxInPhotometry" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="nbPixInterferometry" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="nbPixPhotometry" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="transmission" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="instrumentVisibility" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="instrumentVisibilityBias" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="instrumentVis2CalibrationBias" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="instrumentPhaseBias" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="sequence" type="{http://www.jmmc.fr/aspro-oi/0.1}ObservationSequence" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="useStrehlCorrection" type="{http://www.w3.org/2001/XMLSchema}boolean" default="true" /&gt;
 *       &lt;attribute name="includeAtmosphereCorrection" type="{http://www.w3.org/2001/XMLSchema}boolean" default="true" /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FocalInstrumentSetup", propOrder = {
    "name",
    "description",
    "defaultTotalIntegrationTime",
    "defaultSamplingTime",
    "dit",
    "frameRatio",
    "ron",
    "detectorSaturation",
    "quantumEfficiency",
    "fracFluxInInterferometry",
    "fracFluxInPhotometry",
    "nbPixInterferometry",
    "nbPixPhotometry",
    "transmission",
    "instrumentVisibility",
    "instrumentVisibilityBias",
    "instrumentVis2CalibrationBias",
    "instrumentPhaseBias",
    "sequence"
})
public class FocalInstrumentSetup
    extends OIBase
{

    @XmlElement(required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String name;
    @XmlElement(required = true)
    protected String description;
    protected Integer defaultTotalIntegrationTime;
    protected Integer defaultSamplingTime;
    protected Double dit;
    protected Double frameRatio;
    protected Double ron;
    protected Double detectorSaturation;
    protected Double quantumEfficiency;
    protected Double fracFluxInInterferometry;
    protected Double fracFluxInPhotometry;
    protected Double nbPixInterferometry;
    protected Double nbPixPhotometry;
    protected Double transmission;
    protected Double instrumentVisibility;
    protected Double instrumentVisibilityBias;
    protected Double instrumentVis2CalibrationBias;
    protected Double instrumentPhaseBias;
    protected ObservationSequence sequence;
    @XmlAttribute(name = "useStrehlCorrection")
    protected Boolean useStrehlCorrection;
    @XmlAttribute(name = "includeAtmosphereCorrection")
    protected Boolean includeAtmosphereCorrection;

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
     * Gets the value of the defaultTotalIntegrationTime property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getDefaultTotalIntegrationTime() {
        return defaultTotalIntegrationTime;
    }

    /**
     * Sets the value of the defaultTotalIntegrationTime property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setDefaultTotalIntegrationTime(Integer value) {
        this.defaultTotalIntegrationTime = value;
    }

    /**
     * Gets the value of the defaultSamplingTime property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getDefaultSamplingTime() {
        return defaultSamplingTime;
    }

    /**
     * Sets the value of the defaultSamplingTime property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setDefaultSamplingTime(Integer value) {
        this.defaultSamplingTime = value;
    }

    /**
     * Gets the value of the dit property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getDit() {
        return dit;
    }

    /**
     * Sets the value of the dit property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setDit(Double value) {
        this.dit = value;
    }

    /**
     * Gets the value of the frameRatio property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFrameRatio() {
        return frameRatio;
    }

    /**
     * Sets the value of the frameRatio property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFrameRatio(Double value) {
        this.frameRatio = value;
    }

    /**
     * Gets the value of the ron property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getRon() {
        return ron;
    }

    /**
     * Sets the value of the ron property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setRon(Double value) {
        this.ron = value;
    }

    /**
     * Gets the value of the detectorSaturation property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getDetectorSaturation() {
        return detectorSaturation;
    }

    /**
     * Sets the value of the detectorSaturation property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setDetectorSaturation(Double value) {
        this.detectorSaturation = value;
    }

    /**
     * Gets the value of the quantumEfficiency property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getQuantumEfficiency() {
        return quantumEfficiency;
    }

    /**
     * Sets the value of the quantumEfficiency property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setQuantumEfficiency(Double value) {
        this.quantumEfficiency = value;
    }

    /**
     * Gets the value of the fracFluxInInterferometry property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFracFluxInInterferometry() {
        return fracFluxInInterferometry;
    }

    /**
     * Sets the value of the fracFluxInInterferometry property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFracFluxInInterferometry(Double value) {
        this.fracFluxInInterferometry = value;
    }

    /**
     * Gets the value of the fracFluxInPhotometry property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFracFluxInPhotometry() {
        return fracFluxInPhotometry;
    }

    /**
     * Sets the value of the fracFluxInPhotometry property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFracFluxInPhotometry(Double value) {
        this.fracFluxInPhotometry = value;
    }

    /**
     * Gets the value of the nbPixInterferometry property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getNbPixInterferometry() {
        return nbPixInterferometry;
    }

    /**
     * Sets the value of the nbPixInterferometry property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setNbPixInterferometry(Double value) {
        this.nbPixInterferometry = value;
    }

    /**
     * Gets the value of the nbPixPhotometry property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getNbPixPhotometry() {
        return nbPixPhotometry;
    }

    /**
     * Sets the value of the nbPixPhotometry property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setNbPixPhotometry(Double value) {
        this.nbPixPhotometry = value;
    }

    /**
     * Gets the value of the transmission property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getTransmission() {
        return transmission;
    }

    /**
     * Sets the value of the transmission property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setTransmission(Double value) {
        this.transmission = value;
    }

    /**
     * Gets the value of the instrumentVisibility property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getInstrumentVisibility() {
        return instrumentVisibility;
    }

    /**
     * Sets the value of the instrumentVisibility property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setInstrumentVisibility(Double value) {
        this.instrumentVisibility = value;
    }

    /**
     * Gets the value of the instrumentVisibilityBias property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getInstrumentVisibilityBias() {
        return instrumentVisibilityBias;
    }

    /**
     * Sets the value of the instrumentVisibilityBias property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setInstrumentVisibilityBias(Double value) {
        this.instrumentVisibilityBias = value;
    }

    /**
     * Gets the value of the instrumentVis2CalibrationBias property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getInstrumentVis2CalibrationBias() {
        return instrumentVis2CalibrationBias;
    }

    /**
     * Sets the value of the instrumentVis2CalibrationBias property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setInstrumentVis2CalibrationBias(Double value) {
        this.instrumentVis2CalibrationBias = value;
    }

    /**
     * Gets the value of the instrumentPhaseBias property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getInstrumentPhaseBias() {
        return instrumentPhaseBias;
    }

    /**
     * Sets the value of the instrumentPhaseBias property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setInstrumentPhaseBias(Double value) {
        this.instrumentPhaseBias = value;
    }

    /**
     * Gets the value of the sequence property.
     * 
     * @return
     *     possible object is
     *     {@link ObservationSequence }
     *     
     */
    public ObservationSequence getSequence() {
        return sequence;
    }

    /**
     * Sets the value of the sequence property.
     * 
     * @param value
     *     allowed object is
     *     {@link ObservationSequence }
     *     
     */
    public void setSequence(ObservationSequence value) {
        this.sequence = value;
    }

    /**
     * Gets the value of the useStrehlCorrection property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public boolean isUseStrehlCorrection() {
        if (useStrehlCorrection == null) {
            return true;
        } else {
            return useStrehlCorrection;
        }
    }

    /**
     * Sets the value of the useStrehlCorrection property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setUseStrehlCorrection(Boolean value) {
        this.useStrehlCorrection = value;
    }

    /**
     * Gets the value of the includeAtmosphereCorrection property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public boolean isIncludeAtmosphereCorrection() {
        if (includeAtmosphereCorrection == null) {
            return true;
        } else {
            return includeAtmosphereCorrection;
        }
    }

    /**
     * Sets the value of the includeAtmosphereCorrection property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setIncludeAtmosphereCorrection(Boolean value) {
        this.includeAtmosphereCorrection = value;
    }
    
//--simple--preserve
    @Override
    public String toString() {
        return "FocalInstrumentSetup[" + ((this.name != null) ? this.name : "undefined") + "]";
    }

    /**
     * Initialize and check this instance
     * @param logger logger to use
     * @throws IllegalStateException if the configuration is severly invalid !
     */
    public void init(final org.slf4j.Logger logger) throws IllegalStateException {
        if (this.name == null) {
            throw new IllegalStateException("Invalid name !");
        }

        // Set defaults for mandatory values:
        if (this.dit == null) {
            logger.warn("Invalid dit !");
            setDit(Double.NaN);
        }
        if (this.ron == null) {
            logger.warn("Invalid ron !");
            setRon(Double.NaN);
        }
        if (this.detectorSaturation == null) {
            logger.warn("Invalid detectorSaturation !");
            setDetectorSaturation(Double.NaN);
        }
        if (this.fracFluxInInterferometry == null) {
            logger.warn("Invalid fracFluxInInterferometry !");
            setFracFluxInInterferometry(Double.NaN);
        }
        if (this.fracFluxInPhotometry == null) {
            logger.warn("Invalid fracFluxInPhotometry !");
            setFracFluxInPhotometry(1.0 - this.fracFluxInInterferometry);
        }
        if (this.nbPixInterferometry == null) {
            logger.warn("Invalid nbPixInterferometry !");
            setNbPixInterferometry(Double.NaN);
        }
        if (this.nbPixPhotometry == null) {
            logger.warn("Invalid nbPixPhotometry !");
            setNbPixPhotometry(Double.NaN);
        }
        if (this.transmission == null) {
            logger.warn("Invalid transmission !");
            setTransmission(Double.NaN);
        }
        if (this.instrumentVisibility == null) {
            logger.warn("Invalid instrumentVisibility !");
            setInstrumentVisibility(Double.NaN);
        }
        if (this.instrumentVisibilityBias == null) {
            logger.warn("Invalid instrumentVisibilityBias !");
            setInstrumentVisibilityBias(Double.NaN);
        }
        if (this.instrumentPhaseBias == null) {
            logger.warn("Invalid instrumentPhaseBias !");
            setInstrumentPhaseBias(Double.NaN);
        }

        // Optional values:
        if (this.defaultTotalIntegrationTime == null) {
            setDefaultTotalIntegrationTime(Integer.valueOf(300)); // 300s
        }
        if (this.defaultSamplingTime == null) {
            setDefaultSamplingTime(Integer.valueOf(60)); // 60min
        }
        if (this.frameRatio == null) {
            setFrameRatio(1.0);
        }
        if (this.quantumEfficiency == null) {
            setQuantumEfficiency(1.0);
        }

        // Check sequence:
        if (this.sequence == null) {
            // Create the typical exposure:
            // both interferometric and photometric channels (without chopping nor dead time)
            final Exposure defExposure = new Exposure();
            defExposure.setType(ExposureType.SCIENCE);
            defExposure.setMode(ExposureMode.ALL);

            final ObservationSequence defSequence = new ObservationSequence();
            defSequence.getExposures().add(defExposure);

            setSequence(defSequence);
        }
        
        // Initialize the sequence:
        this.sequence.init(logger, name);
    }

    public void dump(final org.slf4j.Logger logger) {
        logger.info("  Setup[{}] {", getName());

        logger.info("    name: {}", getName());
        logger.info("    description: {}", getDescription());
        
        logger.info("    useStrehlCorrection: {}", isUseStrehlCorrection());

        logger.info("    defaultTotalIntegrationTime: {}", getDefaultTotalIntegrationTime());
        logger.info("    defaultSamplingTime: {}", getDefaultSamplingTime());

        logger.info("    dit: {}", getDit());
        logger.info("    frameRatio: {}", getFrameRatio());

        logger.info("    ron: {}", getRon());
        logger.info("    detectorSaturation: {}", getDetectorSaturation());
        logger.info("    quantumEfficiency: {}", getQuantumEfficiency());

        logger.info("    fracFluxInInterferometry: {}", getFracFluxInInterferometry());
        logger.info("    fracFluxInPhotometry: {}", getFracFluxInPhotometry());

        logger.info("    nbPixInterferometry: {}", getNbPixInterferometry());
        logger.info("    nbPixPhotometry: {}", getNbPixPhotometry());

        logger.info("    transmission: {}", getTransmission());
        logger.info("    instrumentVisibility: {}", getInstrumentVisibility());

        logger.info("    instrumentVisibilityBias: {}", getInstrumentVisibilityBias());
        logger.info("    instrumentVis2CalibrationBias: {}", getInstrumentVis2CalibrationBias());
        logger.info("    instrumentPhaseBias: {}", getInstrumentPhaseBias());

        if (getSequence() != null) {
            getSequence().dump(logger);
        }

        logger.info("  }");
    }
//--simple--preserve

}
