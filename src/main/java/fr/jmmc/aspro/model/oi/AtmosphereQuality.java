
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
 * &lt;simpleType name="AtmosphereQuality"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="Average"/&gt;
 *     &lt;enumeration value="Good"/&gt;
 *     &lt;enumeration value="Excellent"/&gt;
 *     &lt;enumeration value="Worse"/&gt;
 *     &lt;enumeration value="Bad"/&gt;
 *     &lt;enumeration value="Awful"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "AtmosphereQuality")
@XmlEnum
public enum AtmosphereQuality {


    /**
     * 'Average' means seeing = 1.00 arcsec
     * 
     */
    @XmlEnumValue("Average")
    AVERAGE("Average"),

    /**
     * 'Good' means seeing = 0.70 arcsec
     * 
     */
    @XmlEnumValue("Good")
    GOOD("Good"),

    /**
     * 'Excellent' means seeing = 0.60 arcsec
     * 
     */
    @XmlEnumValue("Excellent")
    EXCELLENT("Excellent"),

    /**
     * 'Worse' means seeing = 1.15 arcsec
     * 
     */
    @XmlEnumValue("Worse")
    WORSE("Worse"),

    /**
     * 'Bad' means seeing = 1.40 arcsec
     * 
     */
    @XmlEnumValue("Bad")
    BAD("Bad"),

    /**
     * 'Awful' means seeing = 1.80 arcsec
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
