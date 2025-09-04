
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for SpectralBand.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="SpectralBand"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="B"/&gt;
 *     &lt;enumeration value="G_bp"/&gt;
 *     &lt;enumeration value="V"/&gt;
 *     &lt;enumeration value="G"/&gt;
 *     &lt;enumeration value="R"/&gt;
 *     &lt;enumeration value="G_rp"/&gt;
 *     &lt;enumeration value="I"/&gt;
 *     &lt;enumeration value="J"/&gt;
 *     &lt;enumeration value="H"/&gt;
 *     &lt;enumeration value="K"/&gt;
 *     &lt;enumeration value="L"/&gt;
 *     &lt;enumeration value="M"/&gt;
 *     &lt;enumeration value="N"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "SpectralBand")
@XmlEnum
public enum SpectralBand {


    /**
     * B band
     * 
     */
    B("B"),

    /**
     * G_bp band (Gaia)
     * 
     */
    @XmlEnumValue("G_bp")
    G_BP("G_bp"),

    /**
     * V band
     * 
     */
    V("V"),

    /**
     * G band (Gaia)
     * 
     */
    G("G"),

    /**
     * R band
     * 
     */
    R("R"),

    /**
     * G_rp band (Gaia)
     * 
     */
    @XmlEnumValue("G_rp")
    G_RP("G_rp"),

    /**
     * I band
     * 
     */
    I("I"),

    /**
     * J band
     * 
     */
    J("J"),

    /**
     * H band
     * 
     */
    H("H"),

    /**
     * K band
     * 
     */
    K("K"),

    /**
     * L band
     * 
     */
    L("L"),

    /**
     * M band
     * 
     */
    M("M"),

    /**
     * N band
     * 
     */
    N("N");
    private final String value;

    SpectralBand(String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    public static SpectralBand fromValue(String v) {
        for (SpectralBand c: SpectralBand.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

}
