
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for BiasUnit.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="BiasUnit"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="ABS"/&gt;
 *     &lt;enumeration value="REL"/&gt;
 *     &lt;enumeration value="JY"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "BiasUnit")
@XmlEnum
public enum BiasUnit {


    /**
     * Absolute value
     * 
     */
    ABS,

    /**
     * Relative value
     * 
     */
    REL,

    /**
     * Jansky (flux)
     * 
     */
    JY;

    public String value() {
        return name();
    }

    public static BiasUnit fromValue(String v) {
        return valueOf(v);
    }

}
