
package fr.jmmc.aspro.model.rawobs;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes the raw observation
 *             
 * 
 * <p>Java class for RawObservation complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="RawObservation"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="obsId" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="type" type="{http://www.jmmc.fr/aspro-raw-obs/0.1}ObservationType"/&gt;
 *         &lt;element name="parentId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="programId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="interferometerName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="interferometerVersion" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="stations" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="pops" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="channels" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="instrumentName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="instrumentMode" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="instrumentSubMode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="targetName" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="targetRa" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="targetDec" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="mjdStart" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="mjdEnd" type="{http://www.w3.org/2001/XMLSchema}double"/&gt;
 *         &lt;element name="projectedBaselines" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="expTau0" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="expTemp" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="expSeeing" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "RawObservation", propOrder = {
    "obsId",
    "type",
    "parentId",
    "programId",
    "interferometerName",
    "interferometerVersion",
    "stations",
    "pops",
    "channels",
    "instrumentName",
    "instrumentMode",
    "instrumentSubMode",
    "targetName",
    "targetRa",
    "targetDec",
    "mjdStart",
    "mjdEnd",
    "projectedBaselines",
    "expTau0",
    "expTemp",
    "expSeeing"
})
public class RawObservation
    extends OIBase
{

    @XmlElement(required = true)
    protected String obsId;
    @XmlElement(required = true)
    @XmlSchemaType(name = "string")
    protected ObservationType type;
    protected String parentId;
    protected String programId;
    @XmlElement(required = true)
    protected String interferometerName;
    protected String interferometerVersion;
    @XmlElement(required = true)
    protected String stations;
    protected String pops;
    protected String channels;
    @XmlElement(required = true)
    protected String instrumentName;
    @XmlElement(required = true)
    protected String instrumentMode;
    protected String instrumentSubMode;
    @XmlElement(required = true)
    protected String targetName;
    protected double targetRa;
    protected double targetDec;
    protected double mjdStart;
    protected double mjdEnd;
    @XmlElement(required = true)
    protected String projectedBaselines;
    protected Double expTau0;
    protected Double expTemp;
    protected Double expSeeing;

    /**
     * Gets the value of the obsId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getObsId() {
        return obsId;
    }

    /**
     * Sets the value of the obsId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setObsId(String value) {
        this.obsId = value;
    }

    /**
     * Gets the value of the type property.
     * 
     * @return
     *     possible object is
     *     {@link ObservationType }
     *     
     */
    public ObservationType getType() {
        return type;
    }

    /**
     * Sets the value of the type property.
     * 
     * @param value
     *     allowed object is
     *     {@link ObservationType }
     *     
     */
    public void setType(ObservationType value) {
        this.type = value;
    }

    /**
     * Gets the value of the parentId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getParentId() {
        return parentId;
    }

    /**
     * Sets the value of the parentId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setParentId(String value) {
        this.parentId = value;
    }

    /**
     * Gets the value of the programId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getProgramId() {
        return programId;
    }

    /**
     * Sets the value of the programId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setProgramId(String value) {
        this.programId = value;
    }

    /**
     * Gets the value of the interferometerName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInterferometerName() {
        return interferometerName;
    }

    /**
     * Sets the value of the interferometerName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInterferometerName(String value) {
        this.interferometerName = value;
    }

    /**
     * Gets the value of the interferometerVersion property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInterferometerVersion() {
        return interferometerVersion;
    }

    /**
     * Sets the value of the interferometerVersion property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInterferometerVersion(String value) {
        this.interferometerVersion = value;
    }

    /**
     * Gets the value of the stations property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getStations() {
        return stations;
    }

    /**
     * Sets the value of the stations property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setStations(String value) {
        this.stations = value;
    }

    /**
     * Gets the value of the pops property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPops() {
        return pops;
    }

    /**
     * Sets the value of the pops property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPops(String value) {
        this.pops = value;
    }

    /**
     * Gets the value of the channels property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getChannels() {
        return channels;
    }

    /**
     * Sets the value of the channels property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setChannels(String value) {
        this.channels = value;
    }

    /**
     * Gets the value of the instrumentName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInstrumentName() {
        return instrumentName;
    }

    /**
     * Sets the value of the instrumentName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInstrumentName(String value) {
        this.instrumentName = value;
    }

    /**
     * Gets the value of the instrumentMode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInstrumentMode() {
        return instrumentMode;
    }

    /**
     * Sets the value of the instrumentMode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInstrumentMode(String value) {
        this.instrumentMode = value;
    }

    /**
     * Gets the value of the instrumentSubMode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInstrumentSubMode() {
        return instrumentSubMode;
    }

    /**
     * Sets the value of the instrumentSubMode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInstrumentSubMode(String value) {
        this.instrumentSubMode = value;
    }

    /**
     * Gets the value of the targetName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTargetName() {
        return targetName;
    }

    /**
     * Sets the value of the targetName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTargetName(String value) {
        this.targetName = value;
    }

    /**
     * Gets the value of the targetRa property.
     * 
     */
    public double getTargetRa() {
        return targetRa;
    }

    /**
     * Sets the value of the targetRa property.
     * 
     */
    public void setTargetRa(double value) {
        this.targetRa = value;
    }

    /**
     * Gets the value of the targetDec property.
     * 
     */
    public double getTargetDec() {
        return targetDec;
    }

    /**
     * Sets the value of the targetDec property.
     * 
     */
    public void setTargetDec(double value) {
        this.targetDec = value;
    }

    /**
     * Gets the value of the mjdStart property.
     * 
     */
    public double getMjdStart() {
        return mjdStart;
    }

    /**
     * Sets the value of the mjdStart property.
     * 
     */
    public void setMjdStart(double value) {
        this.mjdStart = value;
    }

    /**
     * Gets the value of the mjdEnd property.
     * 
     */
    public double getMjdEnd() {
        return mjdEnd;
    }

    /**
     * Sets the value of the mjdEnd property.
     * 
     */
    public void setMjdEnd(double value) {
        this.mjdEnd = value;
    }

    /**
     * Gets the value of the projectedBaselines property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getProjectedBaselines() {
        return projectedBaselines;
    }

    /**
     * Sets the value of the projectedBaselines property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setProjectedBaselines(String value) {
        this.projectedBaselines = value;
    }

    /**
     * Gets the value of the expTau0 property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getExpTau0() {
        return expTau0;
    }

    /**
     * Sets the value of the expTau0 property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setExpTau0(Double value) {
        this.expTau0 = value;
    }

    /**
     * Gets the value of the expTemp property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getExpTemp() {
        return expTemp;
    }

    /**
     * Sets the value of the expTemp property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setExpTemp(Double value) {
        this.expTemp = value;
    }

    /**
     * Gets the value of the expSeeing property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getExpSeeing() {
        return expSeeing;
    }

    /**
     * Sets the value of the expSeeing property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setExpSeeing(Double value) {
        this.expSeeing = value;
    }
    
//--simple--preserve
    public final static int VALID_ALL = 0;
    public final static int INVALID_META = 1;
    public final static int INVALID_TIMES = 2;
    public final static int INVALID_UV = 4;

    /** validation flag */
    @javax.xml.bind.annotation.XmlTransient
    private int valid = 0;

    public int getValid() {
        return valid;
    }

    public boolean isValid(final int flag) {
        return (valid & flag) == 0;
    }

    /** converted exposure time in seconds */
    @javax.xml.bind.annotation.XmlTransient
    private double expTime = 0.0;

    /**
     * Return the exposure time in seconds
     * @return exposure time in seconds
     */
    public double getExpTime() {
        return expTime;
    }

    /**
     * Return the exposure time in seconds
     * @return exposure time in seconds
     */
    public int getIntExpTime() {
        return (int) Math.round(expTime);
    }

    /** converted RA in HMS */
    @javax.xml.bind.annotation.XmlTransient
    private String ra = null;

    /**
     * Return the right ascension (RA) in HMS
     * @return right ascension (RA) in HMS
     */
    public final String getRa() {
        return this.ra;
    }

    /** converted DEC in DMS */
    @javax.xml.bind.annotation.XmlTransient
    private String dec = null;

    /**
     * Return the declination (DEC) in DMS
     * @return declination (DEC) in DMS
     */
    public final String getDec() {
        return this.dec;
    }

    /** resolved reference to the interferometer (read only) */
    @javax.xml.bind.annotation.XmlTransient
    private fr.jmmc.aspro.model.oi.InterferometerDescription interferometerRef = null;

    /**
     * Return the reference to the interferometer (read only)
     * @return interferometer or null
     */
    public final fr.jmmc.aspro.model.oi.InterferometerDescription getInterferometerRef() {
        return interferometerRef;
    }

    /** resolved reference to the instrument (read only) */
    @javax.xml.bind.annotation.XmlTransient
    private fr.jmmc.aspro.model.oi.FocalInstrument instrumentRef = null;

    /**
     * Return the reference to the instrument (read only)
     * @return instrument or null
     */
    public final fr.jmmc.aspro.model.oi.FocalInstrument getInstrumentRef() {
        return instrumentRef;
    }

    /** resolved reference to the instrument (read only) */
    @javax.xml.bind.annotation.XmlTransient
    private fr.jmmc.aspro.model.oi.FocalInstrumentMode instrumentModeRef = null;

    /**
     * Return the reference to the instrument (read only)
     * @return instrument or null
     */
    public final fr.jmmc.aspro.model.oi.FocalInstrumentMode getInstrumentModeRef() {
        return instrumentModeRef;
    }

    /** converted date start time */
    @javax.xml.bind.annotation.XmlTransient
    private String dateStart = null;

    /**
     * Return the date start time
     * @return date
     */
    public String getDateStart() {
        return dateStart;
    }

    /** converted start time */
    @javax.xml.bind.annotation.XmlTransient
    private String timeStart = null;

    /**
     * Return the start time
     * @return start time
     */
    public String getTimeStart() {
        return timeStart;
    }

    /** converted date End time */
    @javax.xml.bind.annotation.XmlTransient
    private String dateEnd = null;

    /**
     * Return the date End time
     * @return date
     */
    public String getDateEnd() {
        return dateEnd;
    }

    /** converted End time */
    @javax.xml.bind.annotation.XmlTransient
    private String timeEnd = null;

    /**
     * Return the End time
     * @return End time
     */
    public String getTimeEnd() {
        return timeEnd;
    }

    /** converted LST start time in hours */
    @javax.xml.bind.annotation.XmlTransient
    private double lstStart = Double.NaN;

    /**
     * Return the LST start time in hours
     * @return LST start time in hours
     */
    public double getLstStart() {
        return lstStart;
    }

    /** converted LST end time in hours */
    @javax.xml.bind.annotation.XmlTransient
    private double lstEnd = Double.NaN;

    /**
     * Return the LST end time in hours
     * @return LST end time in hours
     */
    public double getLstEnd() {
        return lstEnd;
    }
    
    /** group id (same observation setup) defined by RawObsManager.analyze() */
    @javax.xml.bind.annotation.XmlTransient
    private Integer gid = null;

    public Integer getGroupId() {
        return gid;
    }

    public void setGroupId(final Integer gid) {
        this.gid = gid;
    }
    
    /** List of UV points per baseline (parsed from JSON) */
    @javax.xml.bind.annotation.XmlTransient
    private java.util.List<fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData> uvPoints = null;

    /**
     * Return the List of UV points per baseline
     * @return List of UV points per baseline
     */
    public java.util.List<fr.jmmc.aspro.model.uvcoverage.UVRangeBaseLineData> getUVBaselines() {
        return uvPoints;
    }

    /**
     * Prepare this observation ie compute and resolve internal references
     * @param logger logger to use
     * @param sharedContext shared context (cache)
     * @param cm configuration manager
     */
    public final void prepare(final org.slf4j.Logger logger,
                              final java.util.Map sharedContext,
                              final fr.jmmc.aspro.model.ConfigurationManager cm) {

        // Convert exposure time:
        this.expTime = Math.max(0.0, (getMjdEnd() - getMjdStart()) * 86400.0);
        if (Double.isNaN(getExpTime())) {
            this.expTime = 0.0;
        }

        // Convert Target coords (deg) to HMS / DMS
        this.ra = fr.jmmc.jmal.ALX.toHMS(getTargetRa());
        this.dec = fr.jmmc.jmal.ALX.toDMS(getTargetDec());

        // Resolve references:
        // interferometer
        this.interferometerRef = cm.getInterferometerDescription(getInterferometerName());

        // instrument
        this.instrumentRef = cm.getInterferometerInstrumentByMode(getInstrumentName(), getInstrumentMode());

        // instrument mode
        this.instrumentModeRef = cm.getInstrumentMode(this.instrumentRef, getInstrumentMode());

        // Parse projected baselines
        this.uvPoints = fr.jmmc.aspro.model.util.ProjectedBaselineUtils.parse(getProjectedBaselines(), instrumentModeRef);

        // Validate:
        this.validate();

        // Prepare computations (observability, uv):
        if (isValid(INVALID_TIMES)) {
            // Prepare AstroSkyCalc on site:
            final String keyInterf = "INTERF_" + getInterferometerName();
            edu.dartmouth.AstroSkyCalc sc = (edu.dartmouth.AstroSkyCalc) sharedContext.get(keyInterf);
            if (sc == null) {
                sc = new edu.dartmouth.AstroSkyCalc();
                sharedContext.put(keyInterf, sc);

                // define site:
                sc.defineSite(this.interferometerRef.getName(),
                        this.interferometerRef.getPosSph(),
                        this.interferometerRef.getTimezone());
            }
            // Prepare ISO date/time formatter:
            java.text.DateFormat df = (java.text.DateFormat) sharedContext.get("DATE_FMT");
            if (df == null) {
                df = new java.text.SimpleDateFormat("yyyy/MM/dd");
                sharedContext.put("DATE_FMT", df);
            }
            java.text.DateFormat tf = (java.text.DateFormat) sharedContext.get("TIME_FMT");
            if (tf == null) {
                tf = new java.text.SimpleDateFormat("HH:mm:ss");
                sharedContext.put("TIME_FMT", tf);
            }

            final double jd_start = getMjdStart() + edu.dartmouth.AstroSkyCalc.MJD_REF;
            final double jd_end = jd_start + getExpTime() / 86400.0;
            logger.debug(" jd_start = {}", jd_start);
            logger.debug(" jd_end   = {}", jd_end);

            // Precompute Date Time UTC:
            final java.util.Date utc_start = sc.toDate(jd_start, fr.jmmc.aspro.model.TimeRef.UTC);
            final java.util.Date utc_end = sc.toDate(jd_end, fr.jmmc.aspro.model.TimeRef.UTC);
            logger.debug("Date UTC Start: {}", utc_start);
            logger.debug("Date UTC End  : {}", utc_end);

            this.dateStart = df.format(utc_start);
            this.dateEnd = df.format(utc_end);

            this.timeStart = tf.format(utc_start);
            this.timeEnd = tf.format(utc_end);
            logger.debug("Date/Time: {} {} to {} {}", dateStart, timeStart, dateEnd, timeEnd);

            // Precompute LST:
            this.lstStart = sc.toLst(jd_start);
            // ensure lstEnd > lstStart: exposure time is positive:
            this.lstEnd = lstStart + getExpTime() / 3600.0;

            logger.debug("lst_start = {}", lstStart);
            logger.debug("lst_end   = {}", lstEnd);
        }
        if (isValid(INVALID_UV)) {
            // prepare UV segments using the instrument mode
            // see UVRangeBaseLineData and UVCoverageService
        }

        logger.debug("prepared: {}", this);
    }

    private void validate() {
        int flag = 0;
        if (this.getType() == null) {
            logger.warn("Missing type for observation[{}]", getObsId());
            flag |= INVALID_META;
        }
        if (isEmpty(this.getStations())) {
            logger.warn("Missing stations for observation[{}]", getObsId());
            flag |= INVALID_META;
        }
        if (Double.isNaN(getMjdStart()) || (getMjdStart() == 0.0)
                || Double.isNaN(getMjdEnd()) || (getMjdEnd() == 0.0)
                || getMjdEnd() <= getMjdStart()) {
            logger.warn("Invalid MJD range [{} to {}] for observation[{}]", getMjdStart(), getMjdEnd(), getObsId());
            flag |= INVALID_META;
            flag |= INVALID_TIMES;
        }
        // check computed values:
        if (getExpTime() <= 1.0) {
            // less than 1 second !
            logger.warn("Invalid Exp. time [{} s] for observation[{}]", getExpTime(), getObsId());
            flag |= INVALID_META;
            flag |= INVALID_TIMES;
        }
        if (getUVBaselines() == null) {
            logger.warn("Invalid Projected baselines for observation[{}]: '{}'", getObsId(), getProjectedBaselines());
            flag |= INVALID_META;
            flag |= INVALID_UV;
        }
        // check references:
        if (this.getInterferometerRef() == null) {
            logger.warn("Missing interferometer reference '{}' for observation[{}]", getInterferometerName(), getObsId());
            flag |= INVALID_META;
            flag |= INVALID_TIMES;
            flag |= INVALID_UV;
        }
        if (this.getInstrumentRef() == null) {
            logger.warn("Missing instrument reference '{}' mode '{}' for observation[{}]", getInstrumentName(), getInstrumentMode(), getObsId());
            flag |= INVALID_META;
            flag |= INVALID_UV;
        } else {
            if (this.getInstrumentModeRef() == null) {
                logger.warn("Missing instrument mode reference '{}' for observation[{}]", getInstrumentMode(), getObsId());
                flag |= INVALID_META;
                flag |= INVALID_UV;
            }
        }
        this.valid = flag;
    }

    public boolean isCompatible(final RawObservation other) {
        if (this == other) {
            return true;
        }
        if (other == null) {
            return false;
        }
        if (!areEquals(this.targetName, other.targetName)) {
            return false;
        }
        if (Double.doubleToLongBits(this.targetRa) != Double.doubleToLongBits(other.targetRa)) {
            return false;
        }
        if (Double.doubleToLongBits(this.targetDec) != Double.doubleToLongBits(other.targetDec)) {
            return false;
        }
        if (!areEquals(this.programId, other.programId)) {
            return false;
        }
        if (!areEquals(this.interferometerName, other.interferometerName)) {
            return false;
        }
        if (!areEquals(this.stations, other.stations)) {
            return false;
        }
        if (!areEquals(this.pops, other.pops)) {
            return false;
        }
        if (!areEquals(this.instrumentName, other.instrumentName)) {
            return false;
        }
        if (!areEquals(this.instrumentMode, other.instrumentMode)) {
            return false;
        }
        if (!areEquals(this.instrumentSubMode, other.instrumentSubMode)) {
            return false;
        }
        if (this.type != other.type) {
            return false;
        }
        return true;
    }

    @Override
    public final String toString() {
        return "RawObservation [" + getObsId() + "] {"
                + "type: " + getType()
                + ", parentId: " + getParentId()
                + ", programId: " + getProgramId()
                + ", interferometerName: " + getInterferometerName()
                + ", interferometerVersion: " + getInterferometerVersion()
                + ", stations: " + getStations()
                + ", pops: " + getPops()
                + ", channels: " + getChannels()
                + ", instrumentName: " + getInstrumentName()
                + ", instrumentMode: " + getInstrumentMode()
                + ", instrumentSubMode: " + getInstrumentSubMode()
                + ", targetName: " + getTargetName()
                + ", targetRA: " + getTargetRa()
                + ", targetDEC: " + getTargetDec()
                + ", mjdStart: " + getMjdStart()
                + ", mjdEnd: " + getMjdEnd()
                + ", projectedBaselines: " + getProjectedBaselines()
                + ", tau0: " + getExpTau0()
                + ", temp: " + getExpTemp()
                + ", seeing: " + getExpSeeing()
                + " | expTime: " + getExpTime()
                + ", RA: " + getRa()
                + ", DEC: " + getDec()
                + ", date: " + getDateStart()
                + ", time: " + getTimeStart()
                + ", LST start: " + getLstStart()
                + ", LST end: " + getLstEnd()
                + "}";
    }

    /**
     * Return an HTML representation of the RawObservation used by tooltips in the given string buffer
     * @param sb string buffer to fill
     */
    public final void toHtml(final StringBuffer sb) {
        toHtml(sb, 0);
    }

    /**
     * Return an HTML representation of the RawObservation used by tooltips in the given string buffer
     * @param sb string buffer to fill
     * @param obsCount observation count (optional)
     */
    public final void toHtml(final StringBuffer sb, final int obsCount) {
        sb.append("<b>Observation Log");
        if (obsCount > 0) {
            sb.append(" (").append(obsCount).append("):");
        }
        sb.append("<br>Type:</b> ").append(getType());

        // skip parent id
        if (getProgramId() != null) {
            sb.append("<br><b>Program ID:</b> ").append(getProgramId());
        }

        // Interferometer:
        sb.append("<hr><b>Interferometer: ").append(getInterferometerName());
        if (getInterferometerVersion() != null) {
            sb.append(" (").append(getInterferometerVersion()).append(")");
        }

        // Interferometer setup:
        if (getStations() != null) {
            sb.append("<br>Baseline: ").append(getStations());
        }
        sb.append("</b>");
        if (getPops() != null) {
            sb.append("<br><b>PoPs:</b> ").append(getPops());
        }
        if (getChannels() != null) {
            sb.append("<br><b>Channels:</b> ").append(getInterferometerName());
        }

        // Instrument:
        sb.append("<br><b>Instrument: ").append(getInstrumentName());
        sb.append("<br>Mode: ").append(getInstrumentMode()).append("</b>");
        if (getChannels() != null) {
            sb.append("<br><b>Sub Mode:</b> ").append(getInstrumentSubMode());
        }

        // Target:
        sb.append("<hr><b>Target: ");
        if (getTargetName() != null) {
            sb.append(getTargetName());
        }
        sb.append("<br>Coords:</b> ").append(getRa()).append(' ').append(getDec());
    }

    /**
     * Return an HTML representation of the RawObservation Times used by tooltips in the given string buffer
     * @param sb string buffer to fill
     * @param showDate bits (1 = start, 2 = end) 
     * @param showId true to indicate the obs id
     */
    public final void timeToHtml(final StringBuffer sb, final int showDate) {
        sb.append("<hr><b>ID:</b> ").append(getObsId());
        if (getExpTime() > 0.0) {
            sb.append("<br><b>Exp. time:</b> ").append(getIntExpTime()).append(" s");
        }
        if ((showDate & 1) != 0) {
            if (getDateStart() != null) {
                sb.append("<br><b>Start time:</b> ").append(getDateStart()).append(' ').append(getTimeStart());
            } else {
                sb.append("<br><b>MJD Start:</b> ").append(getMjdStart());
            }
            if (getLstStart() > 0.0) {
                sb.append("<br><b>LST:</b> ").append(fr.jmmc.jmcs.util.NumberUtils.trimTo3Digits(getLstStart())).append(" h");
            }
        }
        if ((showDate & 2) != 0) {
            if (getDateEnd() != null) {
                sb.append("<br><b>End time:</b> ").append(getDateEnd()).append(' ').append(getTimeEnd());
            } else {
                sb.append("<br><b>MJD End:</b> ").append(getMjdEnd());
            }
            if (getLstEnd() > 0.0) {
                sb.append("<br><b>LST:</b> ").append(fr.jmmc.jmcs.util.NumberUtils.trimTo3Digits(getLstEnd())).append(" h");
            }
        }
    }
        
    /**
     * Return an HTML representation of the RawObservation Times used by tooltips in the given string buffer
     * @param sb string buffer to fill
     */
    public final void weatherToHtml(final StringBuffer sb) {
        // show weather conditions:
        if (getExpTau0() != null) {
            sb.append("<br><b>Tau0:</b> ").append(Math.round(1000.0 * getExpTau0())).append(" ms");
        }
        if (getExpTemp()!= null) {
            sb.append("<br><b>Temp:</b> ").append(fr.jmmc.jmcs.util.NumberUtils.trimTo1Digits(getExpTemp())).append(" C");
        }
        if (getExpSeeing()!= null) {
            sb.append("<br><b>Seeing:</b> ").append(fr.jmmc.jmcs.util.NumberUtils.trimTo1Digits(getExpSeeing())).append(" as");
        }
    }
        
//--simple--preserve

}
