
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
 * &lt;simpleType name="SpectralSetupQuantity">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *     &lt;enumeration value="lambda"/>
 *     &lt;enumeration value="delta_lambda"/>
 *     &lt;enumeration value="nb_photon_thermal"/>
 *     &lt;enumeration value="transmission"/>
 *     &lt;enumeration value="visibility"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
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
    VISIBILITY("visibility");
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
