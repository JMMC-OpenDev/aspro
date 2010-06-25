
package fr.jmmc.aspro.model.oi;

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
    "waveLengthMax"
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
    
//--simple--preserve
  /**
   * Return the central wave length (read only)
   * @return central wave length
   */
  public final double getWaveLength() {
    return (this.waveLengthMax + this.waveLengthMin) / 2d;
  }

  /**
   * Return the number of channels :
   * numberChannels if defined else computed from spectral resolution
   * @return number of channels
   */
  public final int getEffectiveNumberOfChannels() {
    if (getNumberChannels() != null) {
      return getNumberChannels().intValue();
    }
    return computeNumberOfChannels();
  }

  /**
   * Compute the number of channels from the spectral resolution
   * @return number of channels
   */
  public final int computeNumberOfChannels() {
    return Math.max(1, (int) Math.round(getResolution() * (this.waveLengthMax - this.waveLengthMin) / getWaveLength()));
  }
//--simple--preserve

}
