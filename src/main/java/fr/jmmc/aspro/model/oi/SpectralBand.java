
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for SpectralBand.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="SpectralBand">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *     &lt;enumeration value="V"/>
 *     &lt;enumeration value="R"/>
 *     &lt;enumeration value="I"/>
 *     &lt;enumeration value="J"/>
 *     &lt;enumeration value="H"/>
 *     &lt;enumeration value="K"/>
 *     &lt;enumeration value="N"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 * 
 */
@XmlType(name = "SpectralBand")
@XmlEnum
public enum SpectralBand {


    /**
     * V band
     * 
     */
    V,

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
