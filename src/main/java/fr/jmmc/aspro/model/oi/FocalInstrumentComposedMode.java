
package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * 
 *                 This type describes a focal instrument mode (MATISSE LM + N)
 *                 as a composition of several individual instrument modes
 *             
 * 
 * <p>Java class for FocalInstrumentComposedMode complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FocalInstrumentComposedMode"&gt;
 *   &lt;complexContent&gt;
 *     &lt;extension base="{http://www.jmmc.fr/aspro-oi/0.1}FocalInstrumentMode"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="mode" type="{http://www.jmmc.fr/aspro-oi/0.1}FocalInstrumentMode" maxOccurs="unbounded"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/extension&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FocalInstrumentComposedMode", propOrder = {
    "modes"
})
public class FocalInstrumentComposedMode
    extends FocalInstrumentMode
{

    @XmlElement(name = "mode", required = true)
    protected List<FocalInstrumentMode> modes;

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
    /**
     * Gets the value of the waveLengthMin property.
     * 
     */
    @Override
    public Double getWaveLengthMin() {
        if (Double.isNaN(waveLengthMin)) {
            double min = Double.POSITIVE_INFINITY;
            // compute min:
            for (FocalInstrumentMode mode : getModes()) {
                min = Math.min(min, mode.getWaveLengthMin());
            }
            System.out.println("min: "+min);
            waveLengthMin = min;
        }
        return waveLengthMin;
    }

    /**
     * Gets the value of the waveLengthMax property.
     * 
     */
    @Override
    public Double getWaveLengthMax() {
        if (Double.isNaN(waveLengthMax)) {
            double max = Double.NEGATIVE_INFINITY;
            // compute max:
            for (FocalInstrumentMode mode : getModes()) {
                max = Math.max(max, mode.getWaveLengthMax());
            }
            System.out.println("max: "+max);
            waveLengthMax = max;
        }
        return waveLengthMax;
    }

    /**
     * Return the number of spectral channels derived from resolution and bandwidth
     * @return number of spectral channels
     */
    @Override
    public int getSpectralChannels() {
        if (this.spectralChannels == -1) {
            int n = 0;
            // compute sum(count(channels)):
            for (FocalInstrumentMode mode : getModes()) {
                n += mode.getSpectralChannels();
            }
            System.out.println("n: "+n);
            this.spectralChannels = Math.max(1, n);
        }
        return this.spectralChannels;
    }

//--simple--preserve

}
