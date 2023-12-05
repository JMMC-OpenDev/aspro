
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for SpectralSetupQuantity.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="SpectralSetupQuantity"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="lambda"/&gt;
 *     &lt;enumeration value="delta_lambda"/&gt;
 *     &lt;enumeration value="nb_photon_thermal"/&gt;
 *     &lt;enumeration value="transmission"/&gt;
 *     &lt;enumeration value="visibility"/&gt;
 *     &lt;enumeration value="nb_pix_Interf"/&gt;
 *     &lt;enumeration value="nb_pix_Photo"/&gt;
 *     &lt;enumeration value="A_bkg"/&gt;
 *     &lt;enumeration value="B_bkg"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "SpectralSetupQuantity")
@XmlEnum
public enum SpectralSetupQuantity {


    /**
     * 'lambda' = central wavelength
     * 
     */
    @XmlEnumValue("lambda")
    LAMBDA("lambda"),

    /**
     * 'delta_lambda' = spectral bandwidth
     * 
     */
    @XmlEnumValue("delta_lambda")
    DELTA_LAMBDA("delta_lambda"),

    /**
     * 'nb_photon_thermal' = number of thermal photons per seconds
     * 
     */
    @XmlEnumValue("nb_photon_thermal")
    NB_PHOTON_THERMAL("nb_photon_thermal"),

    /**
     * 'transmission' = overall transmission (instrument + interferometer)
     * 
     */
    @XmlEnumValue("transmission")
    TRANSMISSION("transmission"),

    /**
     * 'visibility' = instrumental visibility
     * 
     */
    @XmlEnumValue("visibility")
    VISIBILITY("visibility"),

    /**
     * 'nb_pix_Interf' = number of pixels to code all fringes together (interferometric channel)
     * 
     */
    @XmlEnumValue("nb_pix_Interf")
    NB_PIX_INTERF("nb_pix_Interf"),

    /**
     * 'nb_pix_Photo' = number of pixels to code each photometric channel
     * 
     */
    @XmlEnumValue("nb_pix_Photo")
    NB_PIX_PHOTO("nb_pix_Photo"),

    /**
     * 'A_bkg' = A coefficient in GRAVITY's background noise model: sig_RN = A_bkg / SQRT(DIT) + B_bkg
     * 
     */
    @XmlEnumValue("A_bkg")
    A_BKG("A_bkg"),

    /**
     * 'B_bkg' = B coefficient in GRAVITY's background noise model: sig_RN = A_bkg / SQRT(DIT) + B_bkg
     * 
     */
    @XmlEnumValue("B_bkg")
    B_BKG("B_bkg");
    private final String value;

    SpectralSetupQuantity(String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    public static SpectralSetupQuantity fromValue(String v) {
        for (SpectralSetupQuantity c: SpectralSetupQuantity.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

}
