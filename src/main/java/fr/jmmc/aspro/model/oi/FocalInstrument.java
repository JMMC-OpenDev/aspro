
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes a focal instrument (AMBER, MIDI ...)
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
 *         &lt;element name="alias" type="{http://www.w3.org/2001/XMLSchema}NCName" minOccurs="0"/>
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="experimental" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="numberChannels" type="{http://www.w3.org/2001/XMLSchema}int"/>
 *         &lt;element name="fringeTracker" type="{http://www.w3.org/2001/XMLSchema}IDREF" minOccurs="0"/>
 *         &lt;element name="fringeTrackerRequired" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="setup" type="{http://www.jmmc.fr/aspro-oi/0.1}FocalInstrumentSetup" maxOccurs="unbounded"/>
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
    "alias",
    "description",
    "experimental",
    "numberChannels",
    "fringeTracker",
    "fringeTrackerRequired",
    "setups",
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
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlSchemaType(name = "NCName")
    protected String alias;
    @XmlElement(required = true)
    protected String description;
    protected Boolean experimental;
    protected int numberChannels;
    @XmlElement(type = Object.class)
    @XmlIDREF
    @XmlSchemaType(name = "IDREF")
    protected FringeTracker fringeTracker;
    protected Boolean fringeTrackerRequired;
    @XmlElement(name = "setup", required = true)
    protected List<FocalInstrumentSetup> setups;
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
     * Gets the value of the alias property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAlias() {
        return alias;
    }

    /**
     * Sets the value of the alias property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAlias(String value) {
        this.alias = value;
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
     * Gets the value of the experimental property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isExperimental() {
        return experimental;
    }

    /**
     * Sets the value of the experimental property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setExperimental(Boolean value) {
        this.experimental = value;
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
     * Gets the value of the fringeTracker property.
     * 
     * @return
     *     possible object is
     *     {@link Object }
     *     
     */
    public FringeTracker getFringeTracker() {
        return fringeTracker;
    }

    /**
     * Sets the value of the fringeTracker property.
     * 
     * @param value
     *     allowed object is
     *     {@link Object }
     *     
     */
    public void setFringeTracker(FringeTracker value) {
        this.fringeTracker = value;
    }

    /**
     * Gets the value of the fringeTrackerRequired property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isFringeTrackerRequired() {
        return fringeTrackerRequired;
    }

    /**
     * Sets the value of the fringeTrackerRequired property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setFringeTrackerRequired(Boolean value) {
        this.fringeTrackerRequired = value;
    }

    /**
     * Gets the value of the setups property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the setups property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getSetups().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link FocalInstrumentSetup }
     * 
     * 
     */
    public List<FocalInstrumentSetup> getSetups() {
        if (setups == null) {
            setups = new ArrayList<FocalInstrumentSetup>();
        }
        return this.setups;
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
    /** lower wave length of this instrument (micrometer) */
    @javax.xml.bind.annotation.XmlTransient
    protected double waveLengthMin;

    /**
     * Return the lower wave length of this instrument (micrometer)
     * @return lower wave length of this instrument (micrometer)
     */
    public final double getWaveLengthMin() {
        return waveLengthMin;
    }

    /**
     * Define the lower wave length of this instrument (micrometer)
     * @param waveLengthMin lower wave length of this instrument (micrometer)
     */
    private final void setWaveLengthMin(final double waveLengthMin) {
        this.waveLengthMin = waveLengthMin;
    }
    /** upper wave length of this instrument (micrometer) */
    @javax.xml.bind.annotation.XmlTransient
    protected double waveLengthMax;

    /**
     * Return the upper wave length of this instrument (micrometer)
     * @return upper wave length of this instrument (micrometer)
     */
    public final double getWaveLengthMax() {
        return waveLengthMax;
    }

    /**
     * Define the upper wave length of this instrument (micrometer)
     * @param waveLengthMax upper wave length of this instrument (micrometer)
     */
    private final void setWaveLengthMax(final double waveLengthMax) {
        this.waveLengthMax = waveLengthMax;
    }

    /**
     * Define the instrument wavelength range
     */
    public void defineWaveLengthRange() {
        double min = Double.POSITIVE_INFINITY;
        double max = Double.NEGATIVE_INFINITY;

        for (FocalInstrumentMode mode : getModes()) {
            if (mode.getWaveLengthMin() < min) {
                min = mode.getWaveLengthMin();
            }
            if (mode.getWaveLengthMax() > max) {
                max = mode.getWaveLengthMax();
            }
        }

        setWaveLengthMin(min);
        setWaveLengthMax(max);
    }

    /**
     * Return the alias if defined or the name
     * @return alias or name
     */
    public String getAliasOrName() {
        return (alias != null) ? alias : name;
    }

    @Override
    public final String toString() {
        return "FocalInstrument[" + ((this.name != null) ? this.name : "undefined") + "]";
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
        
        if (this.getSetups().isEmpty()) {
            throw new IllegalStateException("Missing setup !");
        }
        
        // TODO: check setupIds unicity (map) ?
        // Check setups:
        for (FocalInstrumentSetup setup : getSetups()) {
            setup.init(logger);
        }
        
        // Use first setup as default:
        final boolean singleSetup = getSetups().size() == 1;
        final FocalInstrumentSetup defSetup = getSetups().get(0);

        // Check modes:
        for (FocalInstrumentMode insMode : getModes()) {
            insMode.init(logger);

            if (insMode.getSetupRef()== null) {
                // Define the setup to the default setup:
                insMode.setSetupRef(defSetup);
                if (!singleSetup) {
                    logger.warn("Missing setupRef in the instrument mode [" + insMode.getName() + "] "
                            + "of the instrument [" + this.getName() + "] (several setups defined) !");
                }
            } else {
                // check if the setup exists and belongs to this instrument setups:
                if (!getSetups().contains(insMode.getSetupRef())) {
                    throw new IllegalStateException("Invalid setupRef[" + insMode.getSetupRef().getName()+ "] "
                            + "defined in the instrument mode [" + insMode.getName() + "] "
                            + "of the instrument [" + this.getName() + "] !");
                }
            }
        }
    }

    public void dump(final org.slf4j.Logger logger) {
        logger.info("Instrument[{}] {", getName());
        logger.info("  name: {}", getName());
        logger.info("  alias: {}", getAlias());
        logger.info("  description: {}", getDescription());
        
        logger.info("  experimental: {}", isExperimental());
        logger.info("  numberChannels: {}", getNumberChannels());
        
        logger.info("  fringeTracker: {}", getFringeTracker());
        logger.info("  fringeTrackerRequired: {}", isFringeTrackerRequired());

        // computed values:
        logger.info("  waveLengthMin: {}", fr.jmmc.jmcs.util.NumberUtils.trimTo5Digits(getWaveLengthMin()));
        logger.info("  waveLengthMax: {}", fr.jmmc.jmcs.util.NumberUtils.trimTo5Digits(getWaveLengthMax()));

        for (FocalInstrumentSetup setup : getSetups()) {
            setup.dump(logger);
        }
        for (FocalInstrumentMode insMode : getModes()) {
            insMode.dump(logger);
        }

        logger.info("}");
    }

    /**
     * Return the instrument setup of the given identifier in the given list of instrument setups
     * @param id target identifier
     * @param setups list of instrument setups
     * @return instrument setup or null if the instrument setup was not found
     */
    public static FocalInstrumentSetup getSetupById(final String id, final List<FocalInstrumentSetup> setups) {
        if (id != null) {
            for (FocalInstrumentSetup s : setups) {
                if (s.getName().equals(id)) {
                    return s;
                }
            }
        }
        return null;
    }
//--simple--preserve

}
