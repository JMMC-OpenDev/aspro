
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ExposureType.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="ExposureType"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="SCIENCE"/&gt;
 *     &lt;enumeration value="SKY"/&gt;
 *     &lt;enumeration value="DEAD_TIME"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "ExposureType")
@XmlEnum
public enum ExposureType {


    /**
     * 'SCIENCE' means observing the SCIENCE target
     * 
     */
    SCIENCE,

    /**
     * 'SKY' means observing the SKY only (chopping)
     * 
     */
    SKY,

    /**
     * 'DEAD_TIME' means no observing (overhead)
     * 
     */
    DEAD_TIME;

    public String value() {
        return name();
    }

    public static ExposureType fromValue(String v) {
        return valueOf(v);
    }

}
