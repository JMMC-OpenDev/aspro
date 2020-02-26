
package fr.jmmc.aspro.model.rawobs;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ObservationType.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="ObservationType"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="calibrator"/&gt;
 *     &lt;enumeration value="science"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "ObservationType")
@XmlEnum
public enum ObservationType {


    /**
     * Calibrator target
     * 
     */
    @XmlEnumValue("calibrator")
    CALIBRATOR("calibrator"),

    /**
     * Science target
     * 
     */
    @XmlEnumValue("science")
    SCIENCE("science");
    private final String value;

    ObservationType(String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    public static ObservationType fromValue(String v) {
        for (ObservationType c: ObservationType.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

}
