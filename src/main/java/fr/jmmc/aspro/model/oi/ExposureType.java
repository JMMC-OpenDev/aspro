
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ExposureType.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="ExposureType">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *     &lt;enumeration value="SCIENCE"/>
 *     &lt;enumeration value="SKY"/>
 *     &lt;enumeration value="DEAD_TIME"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
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
