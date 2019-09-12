
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
    extends OIBase
{

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
        final CalibratorInformations other = (CalibratorInformations)o;
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

  /* field constants */

  /*
  <field xsi:type="a:StringValue" name="HD" value="23630" description="HD identifier, click to call Simbad on this object"/>
  <field xsi:type="a:StringValue" name="HIP" value="17702" description="HIP identifier, click to call Simbad on this object"/>
  <field xsi:type="a:StringValue" name="DM" value="2300541" description=""/>
  <field xsi:type="a:StringValue" name="TYC1" value="1800" description=""/>
  <field xsi:type="a:StringValue" name="TYC2" value="2202" description=""/>
  <field xsi:type="a:StringValue" name="TYC3" value="1" description=""/>
  <field xsi:type="a:StringValue" name="opt" value="T" description=""/>
  <field xsi:type="a:StringValue" name="2MASS" value="03472908+2406184" description="2MASS identifier, click to call VizieR on this object"/>
  <field xsi:type="a:StringValue" name="RAJ2000" value="03 47 29.076" unit="h:m:s" description="Right Ascencion - J2000"/>
  <field xsi:type="a:StringValue" name="DEJ2000" value="+24 06 18.48" unit="d:m:s" description="Declination - J2000"/>
  <field xsi:type="a:StringValue" name="DENIS" value="" description="DENIS identifier"/>
  <field xsi:type="a:StringValue" name="A2RAdeg" value="" unit="h:m:s" description=""/>
  <field xsi:type="a:StringValue" name="A2DEdeg" value="" unit="d:m:s" description=""/>
  <field xsi:type="a:NumberValue" name="pmRa" value="19.06" unit="mas/yr" description="Proper Motion in Right Ascension"/>
  <field xsi:type="a:NumberValue" name="pmDec" value="-44.75" unit="mas/yr" description="Proper Motion in Declination"/>
  <field xsi:type="a:NumberValue" name="plx" value="8.73" unit="mas" description="Trigonometric Parallaxe"/>
  <field xsi:type="a:NumberValue" name="e_Plx" value="0.99" description="Error on Parallaxe"/>
  <field xsi:type="a:StringValue" name="SpType" value="B7III" description="MK Spectral Type"/>
  <field xsi:type="a:StringValue" name="VarFlag1" value="" description="Variability from GCVS/NSV"/>
  <field xsi:type="a:StringValue" name="VarFlag2" value="" description="Variability in Tycho-1"/>
  <field xsi:type="a:StringValue" name="VarFlag3" value="" description="Variability type among C,D,M,P,R and U"/>
  <field xsi:type="a:StringValue" name="MultFlag" value="" description="Multiplicity type among C,G,O,V and X"/>
  <field xsi:type="a:StringValue" name="BinFlag" value="" description="Multiplicity type among SB or eclipsing B"/>
  <field xsi:type="a:StringValue" name="SBC9" value="" description="SBC9 identifier, click to call VizieR on this object"/>
  <field xsi:type="a:StringValue" name="WDS" value="03475+2406" description="WDS identifier, click to call VizieR on this object"/>
  <field xsi:type="a:NumberValue" name="sep1" value="117.1" unit="arcsec" description="Angular Separation of the binary on first observation"/>
  <field xsi:type="a:NumberValue" name="sep2" value="117.5" unit="arcsec" description="Angular Separation of the binary on last observation"/>
  <field xsi:type="a:StringValue" name="Iflag" value="" description=""/>
  <field xsi:type="a:StringValue" name="Jflag" value="" description=""/>
  <field xsi:type="a:StringValue" name="Kflag" value="" description=""/>
  <field xsi:type="a:StringValue" name="Qflag" value="DCD" description=""/>
  <field xsi:type="a:NumberValue" name="GLAT" value="-23.455" unit="deg" description="Galactic Latitude"/>
  <field xsi:type="a:NumberValue" name="GLON" value="166.668" unit="deg" description="Galactic Longitude"/>
  <field xsi:type="a:StringValue" name="RadVel" value="9.5" unit="km/s" description="Radial Velocity"/>
  <field xsi:type="a:StringValue" name="RotVel" value="215" unit="km/s" description="Rotation Velocity (v sini)"/>
  <field xsi:type="a:NumberValue" name="LD" value="" unit="mas" description="Limb-Darkened Diameter"/>
  <field xsi:type="a:NumberValue" name="e_LD" value="" unit="mas" description="Error on Limb-Darkened Diameter"/>
  <field xsi:type="a:NumberValue" name="UD" value="4.4" unit="mas" description="Uniform-Disc Diameter"/>
  <field xsi:type="a:NumberValue" name="e_UD" value="0.4" unit="mas" description="Error on Uniform-Disc Diameter"/>
  <field xsi:type="a:NumberValue" name="UDDK" value="" unit="mas" description="Uniform-Disc Diameter in K-band"/>
  <field xsi:type="a:NumberValue" name="e_UDDK" value="" unit="mas" description="Error on Uniform-Disc Diameter in K-band"/>
  <field xsi:type="a:NumberValue" name="Dia12" value="" unit="mas" description="Angular Diameter at 12 microns"/>
  <field xsi:type="a:NumberValue" name="e_dia12" value="" unit="mas" description="Error on Angular Diameter at 12 microns"/>
  <field xsi:type="a:StringValue" name="Meth" value="LO" description="Method of Measurement"/>
  <field xsi:type="a:NumberValue" name="lambda" value="" description="Wavelength"/>
  <field xsi:type="a:StringValue" name="lambda" value="B" description="Wavelength"/>
  <field xsi:type="a:StringValue" name="photflux" value="" description=""/>
  <field xsi:type="a:StringValue" name="units" value="M" description=""/>
  <field xsi:type="a:NumberValue" name="B" value="2.808" unit="mag" description="Johnson's Magnitude in B-band"/>
  <field xsi:type="a:NumberValue" name="Bphg" value="2.79" unit="mag" description="Photometric Magnitude in B-band"/>
  <field xsi:type="a:NumberValue" name="V" value="2.869" unit="mag" description="Johnson's Magnitude in V-band"/>
  <field xsi:type="a:NumberValue" name="Vphg" value="" unit="mag" description="Photometric Magnitude in V-band"/>
  <field xsi:type="a:NumberValue" name="R" value="2.84" unit="mag" description="Johnson's Magnitude in R-band"/>
  <field xsi:type="a:NumberValue" name="Rphg" value="2.84" unit="mag" description="Photometric Magnitude in R-band"/>
  <field xsi:type="a:NumberValue" name="I" value="2.88" unit="mag" description="Johnson's Magnitude in I-band"/>
  <field xsi:type="a:NumberValue" name="Iphg" value="" unit="mag" description="Photometric Magnitude in I-band"/>
  <field xsi:type="a:NumberValue" name="Icous" value="" unit="mag" description="Cousin's Magnitude in I-band"/>
  <field xsi:type="a:NumberValue" name="J" value="2.735" unit="mag" description="Johnson's Magnitude in J-band"/>
  <field xsi:type="a:NumberValue" name="H" value="2.735" unit="mag" description="Johnson's Magnitude in H-band"/>
  <field xsi:type="a:NumberValue" name="K" value="2.64" unit="mag" description="Johnson's Magnitude in K-band"/>
  <field xsi:type="a:NumberValue" name="L" value="2.9" unit="mag" description="Johnson's Magnitude in L-band"/>
  <field xsi:type="a:NumberValue" name="M" value="" unit="mag" description="Johnson's Magnitude in M-band"/>
  <field xsi:type="a:NumberValue" name="N" value="" unit="mag" description="Johnson's Magnitude in N-band"/>
  <field xsi:type="a:StringValue" name="color" value="" description=""/>
  <field xsi:type="a:StringValue" name="orig" value="" description="Source of the IR Flux among IRAS or MSX"/>
  <field xsi:type="a:NumberValue" name="F12" value="" unit="Jy" description="Mid-Infrared Flux at 12 microns"/>
  <field xsi:type="a:NumberValue" name="e_F12" value="" description="Relative Error on Mid-Infrared Flux at 12 microns"/>
  <field xsi:type="a:StringValue" name="Calib" value="" description=""/>
  <field xsi:type="a:NumberValue" name="Teff" value="" description="Effective Temperature"/>
  <field xsi:type="a:NumberValue" name="e_Teff" value="" description="Error on Effective Temperature"/>
  <field xsi:type="a:NumberValue" name="A_V" value="" description="Visible Interstellar Absorption"/>
  <field xsi:type="a:NumberValue" name="Chi2" value="" description="Chi2 of Spectro-Photmometric Data Model Fitting"/>
  <field xsi:type="a:NumberValue" name="SpTyp_Teff" value="" description="Spectral Type from adopted Modelling Effective Temperature"/>
  <field xsi:type="a:NumberValue" name="Jcous" value="" unit="mag" description="Cousin's Magnitude in J-band"/>
  <field xsi:type="a:NumberValue" name="Hcous" value="" unit="mag" description="Cousin's Magnitude in H-band"/>
  <field xsi:type="a:NumberValue" name="Kcous" value="" unit="mag" description="Cousin's Magnitude in K-band"/>
  <field xsi:type="a:NumberValue" name="diam_bv" value="0.699" unit="mas" description="B-V Diameter"/>
  <field xsi:type="a:NumberValue" name="e_diam_bv" value="0.056" unit="mas" description="Error on B-V Diameter"/>
  <field xsi:type="a:NumberValue" name="diam_vr" value="0.788" unit="mas" description="V-R Diameter"/>
  <field xsi:type="a:NumberValue" name="e_diam_vr" value="0.076" unit="mas" description="Error on V-R Diameter"/>
  <field xsi:type="a:NumberValue" name="diam_vk" value="0.967" unit="mas" description="V-K Diameter"/>
  <field xsi:type="a:NumberValue" name="e_diam_vk" value="0.067" unit="mas" description="Error on V-K Diameter"/>
  <field xsi:type="a:NumberValue" name="diam_ij" value="" unit="mas" description="I-J Diameter"/>
  <field xsi:type="a:NumberValue" name="e_diam_ij" value="" unit="mas" description="Error on I-J Diameter"/>
  <field xsi:type="a:NumberValue" name="diam_ik" value="" unit="mas" description="I-K Diameter"/>
  <field xsi:type="a:NumberValue" name="e_diam_ik" value="" unit="mas" description="Error on I-K Diameter"/>
  <field xsi:type="a:NumberValue" name="diam_jk" value="" unit="mas" description="J-K Diameter"/>
  <field xsi:type="a:NumberValue" name="e_diam_jk" value="" unit="mas" description="Error on J-K Diameter"/>
  <field xsi:type="a:NumberValue" name="diam_jh" value="" unit="mas" description="J-H Diameter"/>
  <field xsi:type="a:NumberValue" name="e_diam_jh" value="" unit="mas" description="Error on J-H Diameter"/>
  <field xsi:type="a:NumberValue" name="diam_hk" value="" unit="mas" description="H-K Diameter"/>
  <field xsi:type="a:NumberValue" name="e_diam_hk" value="" unit="mas" description="Error on H-K Diameter"/>
  <field xsi:type="a:NumberValue" name="diam_mean" value="" unit="mas" description="Mean Diameter from the IR Magnitude versus Color Indices Calibrations"/>
  <field xsi:type="a:NumberValue" name="e_diam_mean" value="" unit="mas" description="Estimated Error on Mean Diameter"/>
  <field xsi:type="a:StringValue" name="diamFlag" value="OK" description=""/>
  <field xsi:type="a:NumberValue" name="Teff_SpType" value="12000.0" description="Effective Temperature adopted from Spectral Type"/>
  <field xsi:type="a:NumberValue" name="logg_SpType" value="3.378" description="Gravity adopted from Spectral Type"/>
  <field xsi:type="a:NumberValue" name="UD_B" value="0.927" unit="mas" description="B-band Uniform-Disk Diamter"/>
  <field xsi:type="a:NumberValue" name="UD_I" value="0.946" unit="mas" description="I-band Uniform-Disk Diamter"/>
  <field xsi:type="a:NumberValue" name="UD_J" value="0.952" unit="mas" description="J-band Uniform-Disk Diamter"/>
  <field xsi:type="a:NumberValue" name="UD_H" value="0.955" unit="mas" description="H-band Uniform-Disk Diamter"/>
  <field xsi:type="a:NumberValue" name="UD_K" value="0.957" unit="mas" description="K-band Uniform-Disk Diamter"/>
  <field xsi:type="a:NumberValue" name="UD_L" value="0.961" unit="mas" description="L-band Uniform-Disk Diamter"/>
  <field xsi:type="a:NumberValue" name="UD_N" value="0.964" unit="mas" description="N-band Uniform-Disk Diamter"/>
  <field xsi:type="a:NumberValue" name="UD_R" value="0.94" unit="mas" description="R-band Uniform-Disk Diamter"/>
  <field xsi:type="a:NumberValue" name="UD_U" value="0.93" unit="mas" description="U-band Uniform-Disk Diamter"/>
  <field xsi:type="a:NumberValue" name="UD_V" value="0.933" unit="mas" description="V-band Uniform-Disk Diamter"/>
  <field xsi:type="a:NumberValue" name="Av" value="0.099" description="Visual Interstellar Absorption"/>
  <field xsi:type="a:NumberValue" name="Mo" value="" unit="mag" description=""/>
  <field xsi:type="a:NumberValue" name="Lo" value="2.894" unit="mag" description=""/>
  <field xsi:type="a:NumberValue" name="Ko" value="2.628" unit="mag" description=""/>
  <field xsi:type="a:NumberValue" name="Ho" value="2.718" unit="mag" description=""/>
  <field xsi:type="a:NumberValue" name="Jo" value="2.707" unit="mag" description=""/>
  <field xsi:type="a:NumberValue" name="Io" value="2.83" unit="mag" description=""/>
  <field xsi:type="a:NumberValue" name="Ro" value="2.766" unit="mag" description=""/>
  <field xsi:type="a:NumberValue" name="Vo" value="2.77" unit="mag" description=""/>
  <field xsi:type="a:NumberValue" name="Bo" value="2.677" unit="mag" description=""/>
  <field xsi:type="a:NumberValue" name="vis2" value="0.035" description="Squared Visibility"/>
  <field xsi:type="a:NumberValue" name="vis2Err" value="0.033" description="Error on Squared Visibility"/>
  <field xsi:type="a:NumberValue" name="vis2(8mu)" value="" description="Squared Visibility at 8 microns"/>
  <field xsi:type="a:NumberValue" name="vis2Err(8mu)" value="" description="Error on Squared Visibility at 8 microns"/>
  <field xsi:type="a:NumberValue" name="vis2(13mu)" value="" description="Squared Visibility at 13 microns"/>
  <field xsi:type="a:NumberValue" name="vis2Err(13mu)" value="" description="Error on Squared Visibility at 13 microns"/>
  <field xsi:type="a:StringValue" name="vis2Flag" value="" description=""/>
  <field xsi:type="a:NumberValue" name="dist" value="0.0" unit="deg" description="Calibrator to Science object Angular Distance"/>
  <field xsi:type="a:BooleanValue" name="deletedFlag" value="false" description="Used by SearchCal to flag deleted stars"/>
   */
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
    BaseValue diameter = getField(FIELD_LDD);
    // ...
    return diameter;
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
