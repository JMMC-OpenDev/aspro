/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.ob;

import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.SpectralBand;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.service.ObservabilityService;
import fr.jmmc.jmcs.util.FileUtils;
import fr.jmmc.jmcs.util.ResourceUtils;
import java.io.File;
import java.io.IOException;

/**
 * This class generates an observing block for the VLTI GRAVITY instrument
 * @author bourgesl
 */
public final class ExportOBGravity extends ExportOBVLTI {

    /** template name */
    private final static String TEMPLATE_FILE = "fr/jmmc/aspro/ob/GRAVITY_template.obx";
    /** category value for SCIENCE OB: used in TEMPLATE.NAME 'GRAVITY_single_obs_<CATEGORY>' */
    public final static String GRAVITY_VAL_CATEGORY_SCIENCE = "exp";
    /** category value for CALIBRATOR OB: used in TEMPLATE.NAME 'GRAVITY_single_obs_<CATEGORY>' */
    public final static String GRAVITY_VAL_CATEGORY_CALIBRATOR = "calibrator";

    /* keywords */
    /** keyword - SEQ.FT.ROBJ.MAG */
    public final static String KEY_KMAG = "<KMAG>";
    /** keyword - INS-SPEC-RES */
    public final static String KEY_INS_SPEC_RES = "<INS-SPEC-RES>";
    /** keyword - INS-POLA-MODE */
    public final static String KEY_INS_POLA_MODE = "<INS-POLA-MODE>";
    /** keyword - DIAMETER */
    public final static String KEY_DIAMETER = "<DIAMETER>";

    /**
     * Forbidden constructor
     */
    private ExportOBGravity() {
        super();
    }

    /**
     * Generate the OB file for the given target
     * @param file file to save
     * @param observation observation settings
     * @param os observability service with computed data
     * @param target target to process
     *
     * @throws IllegalStateException if the template file is not found or can not be read
     * @throws IOException if an I/O exception occurred while writing the observing block
     */
    public static void generate(final File file, final ObservationSetting observation,
                                final ObservabilityService os, final Target target)
            throws IllegalStateException, IOException {

        logger.debug("generate file: {}", file);

        // get OB template :
        final String template = ResourceUtils.readResource(TEMPLATE_FILE);

        // process common VLTI part :
        String document = processCommon(template, file.getName(), observation, os, target,
                GRAVITY_VAL_CATEGORY_SCIENCE, GRAVITY_VAL_CATEGORY_CALIBRATOR);

        // Magnitudes for H, K :
        document = document.replaceFirst(KEY_HMAG, df3.format(getMagnitude(target.getFLUXH())));
        document = document.replaceFirst(KEY_KMAG, df3.format(getMagnitude(target.getFLUXK())));
        
        // Coude Guided Star = Science (= mag V) :
        document = document.replaceFirst(KEY_COUDE_GS_MAG, df3.format(getMagnitude(target.getFLUXV())));

        // Mode (INS.DISP.NAME) among "FREE" or "GRISM":
        final String instrumentMode = observation.getInstrumentConfiguration().getInstrumentMode();

        // Parse Aspro2's instrument mode: 
        // [LOW-COMBINED, LOW-SPLIT, MEDIUM-COMBINED, MEDIUM-SPLIT, HIGH-COMBINED, HIGH-SPLIT]
        String res;
        final String pola;
        final int pos = instrumentMode.indexOf('-');
        if (pos == -1) {
            logger.warn("Invalid instrument mode for GRAVITY: {}", instrumentMode);
            res = "LOW";
            pola = "IN";
        } else {
            res = instrumentMode.substring(0, pos);
            if ("MEDIUM".equals(res)) {
                res = "MED";
            }
            /*
                pour les deux polariseurs (SPEC.POL et FT.POL):
                SPLIT -> IN
                COMBINED -> OUT            
            */
            pola = ("COMBINED".equals(instrumentMode.substring(pos + 1))) ? "OUT" : "IN";
        }

        document = document.replaceAll(KEY_INS_SPEC_RES, res);
        document = document.replaceAll(KEY_INS_POLA_MODE, pola);

        // Calibrator Angular diameter (mas)
        final TargetUserInformations targetUserInfos = observation.getTargetUserInfos();

        Double diameter = null;
        if (targetUserInfos != null && targetUserInfos.isCalibrator(target)) {
            diameter = target.getDiameter(SpectralBand.K);
        }
        if (diameter == null) {
            diameter = 0.0;
        }
        document = document.replaceFirst(KEY_DIAMETER, df3.format(diameter));
        
        // Finally, write the file :
        FileUtils.writeFile(file, document);
    }
}
