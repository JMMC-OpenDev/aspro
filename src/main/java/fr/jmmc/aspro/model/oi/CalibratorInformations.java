package fr.jmmc.aspro.model.oi;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;

/**
 * 
 *                 This type describes the optional calibrator information (uniform disk diameters).
 *             
 * 
 * <p>Java class for CalibratorInformations complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CalibratorInformations"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="parameter" type="{http://www.jmmc.fr/aspro-oi/0.1}BaseValue" maxOccurs="unbounded" minOccurs="0"/&gt;
 *         &lt;element name="field" type="{http://www.jmmc.fr/aspro-oi/0.1}BaseValue" maxOccurs="unbounded" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CalibratorInformations", propOrder = {
    "parameters",
    "fields"
})
public class CalibratorInformations
        extends OIBase {

    @XmlElement(name = "parameter")
    protected List<BaseValue> parameters;
    @XmlElement(name = "field")
    protected List<BaseValue> fields;

    /**
     * Gets the value of the parameters property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the parameters property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getParameters().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link BaseValue }
     * 
     * 
     */
    public List<BaseValue> getParameters() {
        if (parameters == null) {
            parameters = new ArrayList<BaseValue>();
        }
        return this.parameters;
    }

    /**
     * Gets the value of the fields property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the fields property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getFields().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link BaseValue }
     * 
     * 
     */
    public List<BaseValue> getFields() {
        if (fields == null) {
            fields = new ArrayList<BaseValue>();
        }
        return this.fields;
    }

//--simple--preserve
    @Override
    protected boolean areEquals(final OIBase o) {
        if (!super.areEquals(o)) {
            return false;
        }
        final CalibratorInformations other = (CalibratorInformations) o;
        return (areEquals(this.getParameters(), other.getParameters()) // may create lists
                && areEquals(this.getFields(), other.getFields())); // may create lists
    }

    /**
     * Return a string representation containing parameters and fields
     * @return string representation
     */
    @Override
    public final String toString() {
        return "CalibratorInformations[\nParameters : " + getParameters() + "\nFields: " + getFields() + "\n]";
    }

    /* parameter constants */
    /** parameter SearchCalGuiVersion (string) */
    public final static String PARAMETER_SCL_GUI_VERSION = "SearchCalGuiVersion";
    /** parameter SearchCalServerVersion (string) */
    public final static String PARAMETER_SCL_SERVER_VERSION = "SearchCalServerVersion";
    /** parameter band (string) */
    public final static String PARAMETER_SCL_BAND = "band";
    /** parameter baseline Max (m) (number) */
    public final static String PARAMETER_SCL_BASELINE_MAX = "baseMax";
    /** parameter wavelength (micrometer) (number) */
    public final static String PARAMETER_SCL_WAVE_LENGTH = "wlen";
    /** parameter bright mode (boolean) */
    public final static String PARAMETER_SCL_BRIGHT_MODE = "bright";
    /** main parameters for SearchCal information */
    public final static List<String> PARAMETERS_SCL = java.util.Arrays.asList(new String[]{
        PARAMETER_SCL_GUI_VERSION, PARAMETER_SCL_SERVER_VERSION,
        PARAMETER_SCL_BASELINE_MAX, PARAMETER_SCL_WAVE_LENGTH, PARAMETER_SCL_BRIGHT_MODE
    });

    /* field constants */
    /** field distance (degrees) (number) */
    public final static String FIELD_DISTANCE = "dist";
    /** field UD_B (mas) (number) */
    public final static String FIELD_UD_B = "UD_B";
    /** field UD_V (mas) (number) */
    public final static String FIELD_UD_V = "UD_V";
    /** field UD_R (mas) (number) */
    public final static String FIELD_UD_R = "UD_R";
    /** field UD_I (mas) (number) */
    public final static String FIELD_UD_I = "UD_I";
    /** field UD_J (mas) (number) */
    public final static String FIELD_UD_J = "UD_J";
    /** field UD_H (mas) (number) */
    public final static String FIELD_UD_H = "UD_H";
    /** field UD_K (mas) (number) */
    public final static String FIELD_UD_K = "UD_K";
    /** field UD_L (mas) (number) */
    public final static String FIELD_UD_L = "UD_L";
    /** field UD_M (mas) (number) */
    public final static String FIELD_UD_M = "UD_M";
    /** field UD_N (mas) (number) */
    public final static String FIELD_UD_N = "UD_N";

    /* alternate diameters */
    /** field LDD (mas) (number) = Limb-Darkened Diameter */
    public final static String FIELD_LDD = "LDD";

    /** main fields for SearchCal information */
    public final static List<String> FIELDS_SCL = java.util.Arrays.asList(new String[]{
        FIELD_DISTANCE,
        FIELD_LDD,
        FIELD_UD_B, FIELD_UD_V, FIELD_UD_R,
        FIELD_UD_I, FIELD_UD_J, FIELD_UD_H, FIELD_UD_K,
        FIELD_UD_L, FIELD_UD_M, FIELD_UD_N
    });

    /* Utility methods */
    /**
     * Return the parameter corresponding to the given name
     * @param name parameter name
     * @return parameter or null if not found
     */
    public final BaseValue getParameter(final String name) {
        return findValue(getParameters(), name);
    }

    /**
     * Return the field corresponding to the given name
     * @param name field name
     * @return field or null if not found
     */
    public final BaseValue getField(final String name) {
        return findValue(getFields(), name);
    }

    /**
     * Return the number value for the field corresponding to the given name
     * or null if this is not a NumberValue instance
     * @param name field name
     * @return number value or null
     */
    public final Double getFieldNumber(final String name) {
        final BaseValue value = getField(name);
        if (value != null) {
            return value.getNumber();
        }
        return null;
    }

    /**
     * Return the diameter in the given band
     * @param band spectral band
     * @return diameter in the given band or null if undefined
     */
    public final Double getUDDiameter(final SpectralBand band) {
        if (band != null) {
            switch (band) {
                case B:
                    return getFieldNumber(FIELD_UD_B);
                case V:
                    return getFieldNumber(FIELD_UD_V);
                case R:
                    return getFieldNumber(FIELD_UD_R);
                case I:
                    return getFieldNumber(FIELD_UD_I);
                case J:
                    return getFieldNumber(FIELD_UD_J);
                case H:
                    return getFieldNumber(FIELD_UD_H);
                case K:
                    return getFieldNumber(FIELD_UD_K);
                case L:
                    return getFieldNumber(FIELD_UD_L);
                case M:
                    return getFieldNumber(FIELD_UD_M);
                case N:
                    return getFieldNumber(FIELD_UD_N);
                default:
            }
        }
        return null;
    }

    /**
     * Return the available diameter LDD... (in order of priority)
     * @return diameter LDD... or null if unavailable
     */
    public final BaseValue getAlternateDiameter() {
        return getField(FIELD_LDD);
    }

    /**
     * Return the value corresponding to the given name
     * @param list list to traverse
     * @param name name of the value
     * @return value or null if not found
     */
    private final static BaseValue findValue(final List<BaseValue> list, final String name) {
        for (BaseValue v : list) {
            if (name.equals(v.getName())) {
                return v;
            }
        }
        return null;
    }
//--simple--preserve

}
