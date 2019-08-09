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
 *                 This type describes an observation sequence (SCIENCE | SKY | DEAD_TIME)
 *                 on interferometric and/or photometric channels
 *             
 * 
 * <p>Java class for ObservationSequence complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ObservationSequence"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="exposure" type="{http://www.jmmc.fr/aspro-oi/0.1}Exposure" maxOccurs="unbounded"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ObservationSequence", propOrder = {
    "exposures"
})
public class ObservationSequence
        extends OIBase {

    @XmlElement(name = "exposure", required = true)
    protected List<Exposure> exposures;

    /**
     * Gets the value of the exposures property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the exposures property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getExposures().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link Exposure }
     * 
     * 
     */
    public List<Exposure> getExposures() {
        if (exposures == null) {
            exposures = new ArrayList<Exposure>();
        }
        return this.exposures;
    }

//--simple--preserve
    /** ratio interferometry vs total time in time units (read-only) */
    @javax.xml.bind.annotation.XmlTransient
    protected double ratioInterfero = 0.0;
    /** ratio between total photometry vs Science photometry per beam (read-only) */
    @javax.xml.bind.annotation.XmlTransient
    protected double ratioPhotoPerBeam = 0.0;
    /** ratio between photometry vs interferometry in time unit per beam (read-only) */
    @javax.xml.bind.annotation.XmlTransient
    protected double ratioPhotoVsInterfero = 0.0;
    /** ratio between dead vs interferometry in time units (read-only) */
    @javax.xml.bind.annotation.XmlTransient
    protected double ratioDeadTime = 0.0;

    public double getRatioInterferometry() {
        return ratioInterfero;
    }

    public double getRatioPhotoPerBeam() {
        return ratioPhotoPerBeam;
    }

    public double getRatioPhotoVsInterfero() {
        return ratioPhotoVsInterfero;
    }

    public double getRatioDeadTime() {
        return ratioDeadTime;
    }

    @Override
    public String toString() {
        return "ObservationSequence{" + getExposures() + "}";
    }

    /**
     * Initialize and check this instance
     * @param logger logger to use
     * @param setupName setup name
     * @throws IllegalStateException if the configuration is severly invalid !
     */
    public void init(final org.slf4j.Logger logger, final String setupName) throws IllegalStateException {
        // Check sequence:
        for (Exposure exp : getExposures()) {
            switch (exp.getType()) {
                case SKY:
                    exp.setMode(ExposureMode.PHOTOMETRY);
                    break;
                case DEAD_TIME:
                    exp.setMode(ExposureMode.NONE);
                    break;
                default:
            }
        }

        final double totalInterfero = getTotalUnits(ExposureMode.INTERFEROMETRY);
        final double totalPhoto = getTotalUnits(ExposureMode.PHOTOMETRY);
        final double total = getTotalExposures();

        this.ratioInterfero = totalInterfero / total;

        final double totalDead = getTotalUnits(ExposureMode.NONE);

        this.ratioDeadTime = totalDead / totalInterfero;

        final double totalInterferoPerBeam = getTotalPerBeam(ExposureMode.INTERFEROMETRY);
        final double totalPhotoPerBeam = getTotalPerBeam(ExposureMode.PHOTOMETRY);
        final double totalPhotoSciPerBeam = getTotalPerBeam(ExposureMode.PHOTOMETRY, ExposureType.SCIENCE);

        this.ratioPhotoPerBeam = totalPhotoPerBeam / totalPhotoSciPerBeam;

        this.ratioPhotoVsInterfero = totalPhotoPerBeam / totalInterferoPerBeam;
        /*
        logger.info("--- Setup: " + setupName);
        logger.info("totalInterfero:       " + totalInterfero);
        logger.info("totalPhoto:           " + totalPhoto);
        logger.info("total:                " + total);
        logger.info("ratioInterfero:       " + ratioInterfero);
        logger.info("totalDead:            " + totalDead);
        logger.info("ratioDeadTime:        " + ratioDeadTime);
        logger.info("totalInterferoPerBeam:" + totalInterferoPerBeam);
        logger.info("totalPhotoPerBeam:    " + totalPhotoPerBeam);
        logger.info("totalPhotoSciPerBeam: " + totalPhotoSciPerBeam);
        logger.info("ratioPhotoPerBeam:    " + ratioPhotoPerBeam);
        logger.info("ratioPhotoVsInterfero:" + ratioPhotoVsInterfero);
         */
    }

    private double getTotalExposures() {
        double total = 0.0;
        for (Exposure exp : getExposures()) {
            if (exp.getType() != ExposureType.DEAD_TIME) {
                // matching exposure:
                total += exp.getUnit();
            }
        }
        return total;
    }

    private double getTotalUnits(final ExposureMode mode) {
        double total = 0.0;
        for (Exposure exp : getExposures()) {
            if (exp.getMode() == mode || (mode != ExposureMode.NONE && exp.getMode() == ExposureMode.ALL)) {
                // matching exposure:
                total += exp.getUnit();
            }
        }
        return total;
    }

    private double getTotalPerBeam(final ExposureMode mode) {
        return getTotalPerBeam(mode, null);
    }

    private double getTotalPerBeam(final ExposureMode mode, final ExposureType type) {
        double total = 0.0;
        for (Exposure exp : getExposures()) {
            if ((exp.getMode() == mode || (mode != ExposureMode.NONE && exp.getMode() == ExposureMode.ALL))
                    && (type == null || exp.getType() == type)) {
                // matching exposure:

                // check beams:
                if (exp.getBeams() != null && exp.getBeams().intValue() == 1) {
                    total += 1;
                } else {
                    total += exp.getUnit();
                }
            }
        }
        return total;
    }

    public void dump(final org.slf4j.Logger logger) {
        logger.info("    Sequence {");

        for (Exposure exp : getExposures()) {
            logger.info("      {}", exp);
        }
        logger.info("    }");
    }
//--simple--preserve

}
