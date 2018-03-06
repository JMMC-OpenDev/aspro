
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
 *         &lt;element name="resolution" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="waveLengthMin" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="waveLengthMax" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="waveLengthRef" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="setupRef" type="{http://www.w3.org/2001/XMLSchema}IDREF" minOccurs="0"/&gt;
 *         &lt;element name="dit" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="ditMin" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
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
    "resolution",
    "waveLengthMin",
    "waveLengthMax",
    "waveLengthRef",
    "setupRef",
    "dit",
    "ditMin",
    "table",
    "parameters"
})
public class FocalInstrumentMode
    extends OIBase
{

    @XmlElement(required = true)
    protected String name;
    protected Double resolution;
    protected Double waveLengthMin;
    protected Double waveLengthMax;
    protected Double waveLengthRef;
    @XmlElement(type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected FocalInstrumentSetup setupRef;
    protected Double dit;
    protected Double ditMin;
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
     * Gets the value of the ditMin property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getDitMin() {
        return ditMin;
    }

    /**
     * Sets the value of the ditMin property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setDitMin(Double value) {
        this.ditMin = value;
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
                this.spectralChannels = Math.max(1, (int) Math.round(getResolution() * (this.waveLengthMax - this.waveLengthMin) / getWaveLength()));
            }
        }
        return this.spectralChannels;
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
        logger.info("  ditMin: {}", getDitMin());

        // computed values:
        logger.info("  waveLength: {}", getWaveLength());
        logger.info("  nbSpectralChannels: {}", getSpectralChannels());

        if (this.table != null) {
            logger.info("  table: {}", table);
        }

        logger.info ("}");
    }
  
//--simple--preserve

}
