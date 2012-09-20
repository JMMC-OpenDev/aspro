
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for AtmosphereQuality.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="AtmosphereQuality">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *     &lt;enumeration value="Average"/>
 *     &lt;enumeration value="Good"/>
 *     &lt;enumeration value="Excellent"/>
 *     &lt;enumeration value="Bad"/>
 *     &lt;enumeration value="Awful"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 * 
 */
@XmlType(name = "AtmosphereQuality")
@XmlEnum
public enum AtmosphereQuality {


    /**
     * 'Average' means seeing = 1.0 arc sec
     * 
     */
    @XmlEnumValue("Average")
    AVERAGE("Average"),

    /**
     * 'Good' means seeing = 0.6 arc sec
     * 
     */
    @XmlEnumValue("Good")
    GOOD("Good"),

    /**
     * 'Excellent' means seeing = 0.4 arc sec
     * 
     */
    @XmlEnumValue("Excellent")
    EXCELLENT("Excellent"),

    /**
     * 'Bad' means seeing = 1.4 arc sec
     * 
     */
    @XmlEnumValue("Bad")
    BAD("Bad"),

    /**
     * 'Awful' means seeing = 1.8 arc sec
     * 
     */
    @XmlEnumValue("Awful")
    AWFUL("Awful");
    private final String value;

    AtmosphereQuality(String v) {
        value = v;
    }

    public String value() {
        return value;
    }

    public static AtmosphereQuality fromValue(String v) {
        for (AtmosphereQuality c: AtmosphereQuality.values()) {
            if (c.value.equals(v)) {
                return c;
            }
        }
        throw new IllegalArgumentException(v);
    }

}
