
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for BestPopsMode.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="BestPopsMode"&gt;
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string"&gt;
 *     &lt;enumeration value="ALL"/&gt;
 *     &lt;enumeration value="SELECTED"/&gt;
 *   &lt;/restriction&gt;
 * &lt;/simpleType&gt;
 * </pre>
 * 
 */
@XmlType(name = "BestPopsMode")
@XmlEnum
public enum BestPopsMode {


    /**
     * ALL targets
     * 
     */
    ALL,

    /**
     * SELECTED targets
     * 
     */
    SELECTED;

    public String value() {
        return name();
    }

    public static BestPopsMode fromValue(String v) {
        return valueOf(v);
    }

}
