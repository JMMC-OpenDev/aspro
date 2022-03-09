
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
 *                 This type describes a focal instrument mode (AMBER : "Low_HK" ... "Medium_K_1_2300" ... "High_K_1_2481").
 *                 It has a wavelength range (waveLengthMin - waveLengthMax), a spectral resolution (resolution)
 *             
 * 
 * <p>Java class for FocalInstrumentMode complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FocalInstrumentMode"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="version" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="setupRef" type="{http://www.w3.org/2001/XMLSchema}IDREF" minOccurs="0"/&gt;
 *         &lt;element name="resolution" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="waveLengthMin" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="waveLengthMax" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="waveLengthRef" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="waveLengthBandRef" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="ft_waveLengthBandRef" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="dit" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="frameTime" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="ft_dit" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="ft_frameTime" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="table" type="{http://www.jmmc.fr/aspro-oi/0.1}SpectralSetup" minOccurs="0"/&gt;
 *         &lt;element name="parameter" type="{http://www.jmmc.fr/aspro-oi/0.1}Parameter" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FocalInstrumentMode", propOrder = {
    "name",
    "version",
    "setupRef",
    "resolution",
    "waveLengthMin",
    "waveLengthMax",
    "waveLengthRef",
    "waveLengthBandRef",
    "ftWaveLengthBandRef",
    "dit",
    "frameTime",
    "ftDit",
    "ftFrameTime",
    "table",
    "parameters"
})
public class FocalInstrumentMode
    extends OIBase
{

    @XmlElement(required = true)
    protected String name;
    protected String version;
    @XmlElement(type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected FocalInstrumentSetup setupRef;
    protected Double resolution;
    protected Double waveLengthMin;
    protected Double waveLengthMax;
    protected Double waveLengthRef;
    protected Double waveLengthBandRef;
    @XmlElement(name = "ft_waveLengthBandRef")
    protected Double ftWaveLengthBandRef;
    protected Double dit;
    protected Double frameTime;
    @XmlElement(name = "ft_dit")
    protected Double ftDit;
    @XmlElement(name = "ft_frameTime")
    protected Double ftFrameTime;
    protected SpectralSetup table;
    @XmlElement(name = "parameter")
    protected List<Parameter> parameters;

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
     * Gets the value of the version property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getVersion() {
        return version;
    }

    /**
     * Sets the value of the version property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setVersion(String value) {
        this.version = value;
    }

    /**
     * Gets the value of the setupRef property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public FocalInstrumentSetup getSetupRef() {
        return setupRef;
    }

    /**
     * Sets the value of the setupRef property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setSetupRef(FocalInstrumentSetup value) {
        this.setupRef = value;
    }

    /**
     * Gets the value of the resolution property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getResolution() {
        return resolution;
    }

    /**
     * Sets the value of the resolution property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setResolution(Double value) {
        this.resolution = value;
    }

    /**
     * Gets the value of the waveLengthMin property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getWaveLengthMin() {
        return waveLengthMin;
    }

    /**
     * Sets the value of the waveLengthMin property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setWaveLengthMin(Double value) {
        this.waveLengthMin = value;
    }

    /**
     * Gets the value of the waveLengthMax property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getWaveLengthMax() {
        return waveLengthMax;
    }

    /**
     * Sets the value of the waveLengthMax property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setWaveLengthMax(Double value) {
        this.waveLengthMax = value;
    }

    /**
     * Gets the value of the waveLengthRef property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getWaveLengthRef() {
        return waveLengthRef;
    }

    /**
     * Sets the value of the waveLengthRef property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setWaveLengthRef(Double value) {
        this.waveLengthRef = value;
    }

    /**
     * Gets the value of the waveLengthBandRef property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getWaveLengthBandRef() {
        return waveLengthBandRef;
    }

    /**
     * Sets the value of the waveLengthBandRef property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setWaveLengthBandRef(Double value) {
        this.waveLengthBandRef = value;
    }

    /**
     * Gets the value of the ftWaveLengthBandRef property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFtWaveLengthBandRef() {
        return ftWaveLengthBandRef;
    }

    /**
     * Sets the value of the ftWaveLengthBandRef property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFtWaveLengthBandRef(Double value) {
        this.ftWaveLengthBandRef = value;
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
     * Gets the value of the frameTime property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFrameTime() {
        return frameTime;
    }

    /**
     * Sets the value of the frameTime property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFrameTime(Double value) {
        this.frameTime = value;
    }

    /**
     * Gets the value of the ftDit property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFtDit() {
        return ftDit;
    }

    /**
     * Sets the value of the ftDit property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFtDit(Double value) {
        this.ftDit = value;
    }

    /**
     * Gets the value of the ftFrameTime property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getFtFrameTime() {
        return ftFrameTime;
    }

    /**
     * Sets the value of the ftFrameTime property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setFtFrameTime(Double value) {
        this.ftFrameTime = value;
    }

    /**
     * Gets the value of the table property.
     * 
     * @return
     *     possible object is
     *     {@link SpectralSetup }
     *     
     */
    public SpectralSetup getTable() {
        return table;
    }

    /**
     * Sets the value of the table property.
     * 
     * @param value
     *     allowed object is
     *     {@link SpectralSetup }
     *     
     */
    public void setTable(SpectralSetup value) {
        this.table = value;
    }

    /**
     * Gets the value of the parameters property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the parameters property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getParameters().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Parameter }
     * 
     * 
     */
    public List<Parameter> getParameters() {
        if (parameters == null) {
            parameters = new ArrayList<Parameter>();
        }
        return this.parameters;
    }
    
//--simple--preserve
    /**
     * Return the central wave length (read only)
     * @return central wave length
     */
    public final double getWaveLength() {
        if (this.waveLengthMin == null || this.waveLengthMax == null) {
            return Double.NaN;
        }
        return 0.5d * (this.waveLengthMax + this.waveLengthMin);
    }

    /**
     * Return the effective reference wave length (read only)
     * @return effective reference wave length
     */
    public final double getEffWaveLengthRef() {
        if (getWaveLengthRef() != null) {
            final double wlRef = getWaveLengthRef();
            if ((wlRef >= getWaveLengthMin()) && (wlRef <= getWaveLengthMax())) {
                return wlRef;
            }
        }
        return getWaveLength();
    }

    /**
     * Return the effective reference bandwidth (read only)
     * @return effective reference bandwidth
     */
    public final double getEffWaveLengthBandRef() {
        return (isWavelengthRangeRestriction()) ? getWaveLengthBandRef().doubleValue()
                : (getWaveLengthMax() - getWaveLengthMin());
    }

    /** spectral channels (derived from resolution)(read-only) */
    @javax.xml.bind.annotation.XmlTransient
    protected int spectralChannels = -1;

    /**
     * Return the number of spectral channels derived from resolution and bandwidth
     * @return number of spectral channels
     */
    public int getSpectralChannels() {
        if (this.spectralChannels == -1) {
            if (this.waveLengthMin == null || this.waveLengthMax == null) {
                this.spectralChannels = 1;
            } else {
                this.spectralChannels = getSpectralChannels(getEffWaveLengthRef(), getEffWaveLengthBandRef(), getResolution());
            }
        }
        return this.spectralChannels;
    }

    /**
     * Return true if the wavelength range is restricted (dit limit)
     * @return true if the wavelength range is restricted (dit limit)
     */
    public boolean isWavelengthRangeRestriction() {
        if (getWaveLengthBandRef() != null) {
            return ((getWaveLengthMax() - getWaveLengthMin()) > getWaveLengthBandRef());
        }
        return false;
    }

    /**
     * Return the parameter value of the given name from the list of parameters associated to this focal instrument mode
     * @param name parameter name
     * @return parameter value or null if the parameter was not found
     */
    public final String getParameterValue(final String name) {
        if (this.parameters != null) {
            final Parameter p = getParameter(name, this.parameters);
            if (p != null) {
                return p.getValue();
            }
        }
        return null;
    }

    /**
     * Return the parameter of the given name in the given list of parameters
     * @param name parameter name
     * @param parameters list of parameters
     * @return parameter or null if the parameter was not found
     */
    public static Parameter getParameter(final String name, final List<Parameter> parameters) {
        if (name != null) {
            for (Parameter p : parameters) {
                if (p.getName().equalsIgnoreCase(name)) {
                    return p;
                }
            }
        }
        return null;
    }

    @Override
    public final String toString() {
        return "FocalInstrumentMode [" + ((this.name != null) ? this.name : "undefined") + "]";
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

        // setup is tested at higher level !
        // dit and ditMin are optional.
        if (this.table != null) {
            this.table.init(logger);

            final double[] lambda = this.table.getColumn(SpectralSetupQuantity.LAMBDA).getValues();
            final double[] delta_lambda = this.table.getColumn(SpectralSetupQuantity.DELTA_LAMBDA).getValues();

            final int nbRows = this.table.getNbRows();

            // Define the number of spectral channels:
            this.spectralChannels = nbRows;

            // Define wavelength min/max and resolution:
            int ch = 0;
            setWaveLengthMin(lambda[ch] - 0.5 * delta_lambda[ch]);

            ch = nbRows - 1;
            setWaveLengthMax(lambda[ch] + 0.5 * delta_lambda[ch]);

            ch = nbRows / 2;
            setResolution(lambda[ch] / delta_lambda[ch]);
        }

        if (this.waveLengthMin == null) {
            logger.warn("Invalid waveLengthMin !");
            setWaveLengthMin(Double.NaN);
        }
        if (this.waveLengthMax == null) {
            logger.warn("Invalid waveLengthMax !");
            setWaveLengthMax(Double.NaN);
        }
        if (this.waveLengthMax < this.waveLengthMin) {
            logger.warn("Invalid waveLengthMax < waveLengthMin !");
        }
        if (this.resolution == null) {
            logger.warn("Invalid resolution !");
            setResolution(Double.NaN);
        }
    }

    public void dump(final org.slf4j.Logger logger) {
        logger.info("Mode[{}] {", getName());
        logger.info("  name: {}", getName());
        logger.info("  resolution: {}", getResolution());
        if (getWaveLengthMin() != null) {
            logger.info("  waveLengthMin: {}", fr.jmmc.jmcs.util.NumberUtils.trimTo5Digits(getWaveLengthMin()));
        }
        if (getWaveLengthMax() != null) {
            logger.info("  waveLengthMax: {}", fr.jmmc.jmcs.util.NumberUtils.trimTo5Digits(getWaveLengthMax()));
        }
        logger.info("  setup: {}", getSetupRef());

        logger.info("  dit: {}", getDit());

        // computed values:
        logger.info("  waveLength: {}", getWaveLength());
        logger.info("  nbSpectralChannels: {}", getSpectralChannels());

        if (this.table != null) {
            logger.info("  table: {}", table);
        }

        logger.info("}");
    }

    public double getEffectiveWavelengthRange(final double lambda, final boolean useWavelengthRangeRestriction,
                                              final double effband, final fr.jmmc.oitools.model.range.Range range) {
        double min = getWaveLengthMin();
        double max = getWaveLengthMax();

        double wlRef = lambda;

        if (useWavelengthRangeRestriction) {
            final double halfBand = 0.5 * effband;
            if (wlRef < min + halfBand) {
                wlRef = min + halfBand;
            }
            if (wlRef > max - halfBand) {
                wlRef = max - halfBand;
            }
            min = wlRef - halfBand;
            max = wlRef + halfBand;
        }
        range.setMin(min);
        range.setMax(max);

        return wlRef;
    }

    public static int getSpectralChannels(final double wavelength, final double waveBand, final double resolution) {
        return Math.max(1, (int) Math.round(resolution * waveBand / wavelength));
    }
//--simple--preserve

}
