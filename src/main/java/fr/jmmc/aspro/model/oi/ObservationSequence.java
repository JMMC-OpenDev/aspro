
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
 * &lt;complexType name="ObservationSequence">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="exposure" type="{http://www.jmmc.fr/aspro-oi/0.1}Exposure" maxOccurs="unbounded"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ObservationSequence", propOrder = {
    "exposures"
})
public class ObservationSequence
    extends OIBase
{

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
    /** ratio total time vs interferometry in time units (read-only) */
    @javax.xml.bind.annotation.XmlTransient
    protected double ratioTime = 0.0;
    /** ratio between photometry vs interferometry per beam (read-only) */
    @javax.xml.bind.annotation.XmlTransient
    protected double ratioPhotoPerBeam = 0.0;
    /** ratio between dead vs interferometry in time units (read-only) */
    @javax.xml.bind.annotation.XmlTransient
    protected double ratioDeadTime = 0.0;

    public double getRatioTime() {
        return ratioTime;
    }

    public double getRatioPhotoPerBeam() {
        return ratioPhotoPerBeam;
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
     * @throws IllegalStateException if the configuration is severly invalid !
     */
    public void init(final org.slf4j.Logger logger) throws IllegalStateException {
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
        /*
        Sequences:
            <sequence>
              <exposure type="SCIENCE" mode="ALL" beams="4"/>
              <exposure type="SKY" mode="PHOTOMETRY" beams="4"/>
              <exposure type="DEAD_TIME" unit="2"/> <!-- 2 exposures -->
            </sequence>
        
            <!--
                1. <sequence type=SCIENCE mode=INTERFEROMETRY unit=1> (1 seul frame ou 10?)
                2. <sequence type=SCIENCE mode=PHOTO unit=4 beams=1> (4T)
                3. <sequence type=SKY mode=PHOTO unit=4 beams=1> (chopping sur chaque mesure photom\ufffd\ufffdtrique)
                4. <sequence type=DEAD_TIME unit=9> (total overheads)
            -->
         */

        final double totalInterfero = getTotalUnits(ExposureMode.INTERFEROMETRY);
        final double totalPhoto = getTotalUnits(ExposureMode.PHOTOMETRY);

        this.ratioTime = getTotalExposures();

        final double totalDead = getTotalUnits(ExposureMode.NONE);

        this.ratioDeadTime = totalDead / totalInterfero;

        final double totalPhotoPerBeam = getTotalPerBeam(ExposureMode.PHOTOMETRY);

        this.ratioPhotoPerBeam = totalPhotoPerBeam / totalInterfero;
/*
        logger.info("totalInterfero: " + totalInterfero);
        logger.info("totalPhoto: " + totalPhoto);
        logger.info("ratioTime: " + ratioTime);
        logger.info("totalDead: " + totalDead);
        logger.info("ratioDeadTime: " + ratioDeadTime);
        logger.info("totalPhotoPerBeam: " + totalPhotoPerBeam);
        logger.info("ratioPhotoPerBeam: " + ratioPhotoPerBeam);
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
        double total = 0.0;
        for (Exposure exp : getExposures()) {
            if (exp.getMode() == mode || (mode != ExposureMode.NONE && exp.getMode() == ExposureMode.ALL)) {
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
