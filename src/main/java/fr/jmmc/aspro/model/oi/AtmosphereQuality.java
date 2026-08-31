
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
 *     &lt;enumeration value="Better"/&gt;
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
     * 'Average' means (T < 50%, corresponding to seeing < 1.00\u201c and \u03c40 > 3.2ms)
     * 
     */
    @XmlEnumValue("Average")
    AVERAGE("Average"),

    /**
     * 'Better' means (T < 30%, corresponding to seeing < 0.80\u201c and \u03c40 > 4.1ms)
     * 
     */
    @XmlEnumValue("Better")
    BETTER("Better"),

    /**
     * 'Good' means (T < 20%, corresponding to seeing < 0.70\u201c and \u03c40 > 4.4ms)
     * 
     */
    @XmlEnumValue("Good")
    GOOD("Good"),

    /**
     * 'Excellent' means (T < 10%, corresponding to seeing < 0.60\u201c and \u03c40 > 5.2ms)
     * 
     */
    @XmlEnumValue("Excellent")
    EXCELLENT("Excellent"),

    /**
     * 'Worse' means (T < 70%, corresponding to seeing < 1.15\u201c and \u03c40 > 2.2ms)
     * 
     */
    @XmlEnumValue("Worse")
    WORSE("Worse"),

    /**
     * 'Bad' means (T < 85%, corresponding to seeing < 1.40\u201c and \u03c40 > 1.6ms)
     * 
     */
    @XmlEnumValue("Bad")
    BAD("Bad"),

    /**
     * 'Awful' means (T > 85%, corresponding to seeing < 1.80\u201c and \u03c40 > 1.0ms)
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
