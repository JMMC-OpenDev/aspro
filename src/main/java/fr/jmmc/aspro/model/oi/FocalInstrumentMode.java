
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes a focal instrument mode (AMBER : "Low_HK" ... "Medium_K_1_2300" ... "High_K_1_2481").
 *         It has a wavelength range (waveLengthMin - waveLengthMax), a spectral resolution (resolution)
 *         and optionaly a number of channels (numberChannels)
 *       
 * 
 * <p>Java class for FocalInstrumentMode complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FocalInstrumentMode">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="resolution" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="numberChannels" type="{http://www.w3.org/2001/XMLSchema}int" minOccurs="0"/>
 *         &lt;element name="waveLengthMin" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="waveLengthMax" type="{http://www.w3.org/2001/XMLSchema}double"/>
 *         &lt;element name="parameter" type="{http://www.jmmc.fr/aspro-oi/0.1}Parameter" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FocalInstrumentMode", propOrder = {
    "name",
    "resolution",
    "numberChannels",
    "waveLengthMin",
    "waveLengthMax",
    "parameters"
})
public class FocalInstrumentMode
    extends OIBase
{

    @XmlElement(required = true)
    protected String name;
    protected double resolution;
    protected Integer numberChannels;
    protected double waveLengthMin;
    protected double waveLengthMax;
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
     */
    public double getResolution() {
        return resolution;
    }

    /**
     * Sets the value of the resolution property.
     * 
     */
    public void setResolution(double value) {
        this.resolution = value;
    }

    /**
     * Gets the value of the numberChannels property.
     * 
     * @return
     *     possible object is
     *     {@link Integer }
     *     
     */
    public Integer getNumberChannels() {
        return numberChannels;
    }

    /**
     * Sets the value of the numberChannels property.
     * 
     * @param value
     *     allowed object is
     *     {@link Integer }
     *     
     */
    public void setNumberChannels(Integer value) {
        this.numberChannels = value;
    }

    /**
     * Gets the value of the waveLengthMin property.
     * 
     */
    public double getWaveLengthMin() {
        return waveLengthMin;
    }

    /**
     * Sets the value of the waveLengthMin property.
     * 
     */
    public void setWaveLengthMin(double value) {
        this.waveLengthMin = value;
    }

    /**
     * Gets the value of the waveLengthMax property.
     * 
     */
    public double getWaveLengthMax() {
        return waveLengthMax;
    }

    /**
     * Sets the value of the waveLengthMax property.
     * 
     */
    public void setWaveLengthMax(double value) {
        this.waveLengthMax = value;
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
    return 0.5d * (this.waveLengthMax + this.waveLengthMin);
  }

  /**
   * Return the number of channels:
   * @deprecated TODO KILL ASAP
   * numberChannels if defined else computed from spectral resolution
   * @return number of channels
   */
  public final int getEffectiveNumberOfChannels() {
    if (getNumberChannels() != null) {
      return getNumberChannels().intValue();
    }
    return getSpectralChannels();
  }
  
  /** spectral channels (derived from resolution)(read-only) */
  @javax.xml.bind.annotation.XmlTransient
  private int spectralChannels = -1;

  /**
   * Return the number of spectral channels derived from resolution and bandwidth
   * @return number of spectral channels
   */
  public final int getSpectralChannels() {
    if (this.spectralChannels == -1) {
      this.spectralChannels = Math.max(1, (int) Math.round(getResolution() * (this.waveLengthMax - this.waveLengthMin) / getWaveLength()));
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
//--simple--preserve

}
