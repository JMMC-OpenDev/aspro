
package fr.jmmc.aspro.model.rawobs;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ObservationType.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="ObservationType"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="CALIBRATION"/&gt;
 *     &lt;enumeration value="SCIENCE"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "ObservationType")
@XmlEnum
public enum ObservationType {


    /**
     * Calibration
     * 
     */
    CALIBRATION,

    /**
     * Science
     * 
     */
    SCIENCE;

    public String value() {
        return name();
    }

    public static ObservationType fromValue(String v) {
        return valueOf(v);
    }

}
