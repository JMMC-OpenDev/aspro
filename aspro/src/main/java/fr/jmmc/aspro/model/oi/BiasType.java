
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for BiasType.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="BiasType"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="VIS"/&gt;
 *     &lt;enumeration value="VISPHI"/&gt;
 *     &lt;enumeration value="T3PHI"/&gt;
 *     &lt;enumeration value="PHOT"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "BiasType")
@XmlEnum
public enum BiasType {


    /**
     * Visibility (absolute or differential) according to the instrument
     * 
     */
    VIS("VIS"),

    /**
     * Visibility Phase (absolute or differential) according to the instrument
     * 
     */
    VISPHI("VISPHI"),

    /**
     * Closure Phase
     * 
     */
    @XmlEnumValue("T3PHI")
    T_3_PHI("T3PHI"),

    /**
     * Photometry error
     * 
     */
    PHOT("PHOT");
    private final String value;

    BiasType(String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    public static BiasType fromValue(String v) {
        for (BiasType c: BiasType.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

}
