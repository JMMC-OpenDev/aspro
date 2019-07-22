
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlEnum;
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
 *     &lt;enumeration value="V"/&gt;
 *     &lt;enumeration value="G"/&gt;
 *     &lt;enumeration value="R"/&gt;
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
    B,

    /**
     * V band
     * 
     */
    V,

    /**
     * G band (Gaia)
     * 
     */
    G,

    /**
     * R band
     * 
     */
    R,

    /**
     * I band
     * 
     */
    I,

    /**
     * J band
     * 
     */
    J,

    /**
     * H band
     * 
     */
    H,

    /**
     * K band
     * 
     */
    K,

    /**
     * L band
     * 
     */
    L,

    /**
     * M band
     * 
     */
    M,

    /**
     * N band
     * 
     */
    N;

    public String value() {
        return name();
    }

    public static SpectralBand fromValue(String v) {
        return valueOf(v);
    }

}
