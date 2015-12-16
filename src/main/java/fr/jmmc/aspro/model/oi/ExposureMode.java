
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ExposureMode.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="ExposureMode">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *     &lt;enumeration value="NONE"/>
 *     &lt;enumeration value="INTERFEROMETRY"/>
 *     &lt;enumeration value="PHOTOMETRY"/>
 *     &lt;enumeration value="ALL"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 * 
 */
@XmlType(name = "ExposureMode")
@XmlEnum
public enum ExposureMode {


    /**
     * 'NONE' means not observing
     * 
     */
    NONE,

    /**
     * 'INTERFEROMETRY' means observing only the interferometric channel
     * 
     */
    INTERFEROMETRY,

    /**
     * 'PHOTOMETRY' means observing only the photometric channel
     * 
     */
    PHOTOMETRY,

    /**
     * 'ALL' means observing both interferometric and photometric channels
     * 
     */
    ALL;

    public String value() {
        return name();
    }

    public static ExposureMode fromValue(String v) {
        return valueOf(v);
    }

}
