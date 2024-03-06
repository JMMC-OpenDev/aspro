/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetGroup;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.jmal.ALX;
import fr.jmmc.jmal.CoordUtils;
import fr.jmmc.jmal.star.Star;
import static fr.jmmc.jmal.star.StarResolver.SIMBAD_MAIN_URL;
import fr.jmmc.jmcs.service.BrowserLauncher;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.StringUtils;
import fr.jmmc.jmcs.util.UrlUtils;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author bourgesl
 */
public final class TargetUtils {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(TargetUtils.class.getName());
    /** Simbad URL (query by identifier) */
    public static final String SIMBAD_QUERY_ID = SIMBAD_MAIN_URL + "sim-id?Ident=";

    /** distance in degrees to consider same targets = 5 arcsecs */
    public final static double SAME_TARGET_DISTANCE = 5d * fr.jmmc.jmal.ALX.ARCSEC_IN_DEGREES;

    /** minimum number of identifiers to analyze */
    public final static double MIN_ID = 3;

    /**
     * Forbidden constructor
     */
    private TargetUtils() {
        // no-op
    }

    public static String getSimbadURL(final String id) {
        if (!StringUtils.isEmpty(id)) {
            return SIMBAD_QUERY_ID + UrlUtils.encode(id);
        }
        return null;
    }

    public static void openSimbad(final String id) {
        final String url = getSimbadURL(id);
        if (url != null) {
            logger.debug("Simbad url = {}", url);

            BrowserLauncher.openURL(url);
        }
    }

    /**
     * Fix RA: parse given value as HMS and re-format to HMS (normalization)
     * @param ra right ascension as HMS
     * @return right ascension as HMS
     */
    public static String fixRA(final String ra) {
        return ALX.toHMS(ALX.parseHMS(ra));
    }

    /**
     * Fix DEC: parse given value as DMS and re-format to DMS (normalization)
     * @param dec declination as DMS
     * @return declination as DMS
     */
    public static String fixDEC(final String dec) {
        return ALX.toDMS(ALX.parseDEC(dec));
    }

    public static double computeDistanceInDegrees(final Target refTarget, final Target otherTarget) {
        return CoordUtils.computeDistanceInDegrees(refTarget.getRADeg(), refTarget.getDECDeg(),
                otherTarget.getRADeg(), otherTarget.getDECDeg());
    }

    /**
     * Check the distance between the given source target and the given list of targets (5 arcesecs)
     * @param srcTarget source target
     * @param targets list of targets
     * @return Target if found or null
     * @throws IllegalArgumentException if the target is too close to another target present in the given list of targets
     */
    public static TargetMatch matchTargetCoordinates(final Target srcTarget, final List<Target> targets) throws IllegalArgumentException {
        double distance, min = Double.MAX_VALUE;
        Target match = null;

        for (Target target : targets) {
            distance = computeDistanceInDegrees(srcTarget, target);

            if (distance <= SAME_TARGET_DISTANCE) {
                // check simbad identifiers to avoid false-positives:
                int count = matchTargetIds(srcTarget, target);
                if (count != -1) {
                    // only 1 common identifier may be not enough (depends on the catalog accuracy)
                    if (count >= MIN_ID) {
                        logger.info("match[{} arcsec] for [{}]: [{}]", NumberUtils.trimTo3Digits(distance * ALX.DEG_IN_ARCSEC),
                                srcTarget.getId(), target.getId());
                        return new TargetMatch(target);
                    } else if (count == 0) {
                        // no common identifier at all => skip to ignore that target
                        logger.info("no match[{} arcsec] for [{}]: [{}]", NumberUtils.trimTo3Digits(distance * ALX.DEG_IN_ARCSEC),
                                srcTarget.getId(), target.getId());
                        continue;
                    }
                }

                // keep closest target:
                if (distance < min) {
                    min = distance;
                    match = target;
                }
            }
        }
        if (match != null) {
            // return the closest match:
            return new TargetMatch(match, min);
        }
        return null;
    }

    private static int matchTargetIds(final Target src, final Target other) {
        int count = -1;

        final String sIds = src.getIDS();
        final String oIds = other.getIDS();

        if (sIds != null && oIds != null) {
            final String[] sIdArray = sIds.split(",");

            // require at least several identifiers to compare:
            if (sIdArray.length >= MIN_ID) {
                // exactly the same identifiers (simbad):
                if (sIds.equals(oIds)) {
                    count = sIdArray.length;
                } else {
                    final String[] oIdArray = oIds.split(",");
                    if (oIdArray.length >= MIN_ID) {
                        count = 0;
                        for (String s : sIdArray) {
                            for (String o : oIdArray) {
                                if (s.equals(o)) {
                                    count++;
                                }
                            }
                        }
                    }
                }
                if (count >= 0) {
                    logger.info("match[{}] from ids [{}] x [{}]", count, sIds, oIds);
                }
            }
        }
        return count;
    }

    /**
     * Convert the given Star instance to a Target instance
     * @param star star instance
     * @return new Target instance
     */
    public static Target convert(final Star star) {
        if (star == null || StringUtils.isEmpty(star.getName())) {
            return null;
        }
        /* 
         Star data:
         Strings = {DEC=+43 49 23.910, RA=05 01 58.1341, OTYPELIST=**,Al*,SB*,*,Em*,V*,IR,UV, SPECTRALTYPES=A8Iab:}
         Doubles = {PROPERMOTION_RA=0.18, PARALLAX=1.6, DEC_d=43.8233083, FLUX_J=1.88, PROPERMOTION_DEC=-2.31, FLUX_K=1.533, PARALLAX_err=1.16, FLUX_V=3.039, FLUX_H=1.702, RA_d=75.4922254}
         */
        final Target newTarget = new Target();
        // format the target name:
        newTarget.updateNameAndIdentifier(star.getName());

        // coordinates (HMS / DMS) (mandatory):
        newTarget.setCoords(
                star.getPropertyAsString(Star.Property.RA).replace(' ', ALX.SEPARATOR),
                star.getPropertyAsString(Star.Property.DEC).replace(' ', ALX.SEPARATOR),
                AsproConstants.EPOCH_J2000);

        // Proper motion (mas/yr) (optional) :
        newTarget.setPMRA(star.getPropertyAsDouble(Star.Property.PROPERMOTION_RA));
        newTarget.setPMDEC(star.getPropertyAsDouble(Star.Property.PROPERMOTION_DEC));

        // Parallax (mas) (optional) :
        newTarget.setPARALLAX(star.getPropertyAsDouble(Star.Property.PARALLAX));
        newTarget.setPARAERR(star.getPropertyAsDouble(Star.Property.PARALLAX_err));

        // Magnitudes (optional) :
        newTarget.setFLUXB(star.getPropertyAsDouble(Star.Property.FLUX_B));
        newTarget.setFLUXV(star.getPropertyAsDouble(Star.Property.FLUX_V));
        newTarget.setFLUXR(star.getPropertyAsDouble(Star.Property.FLUX_R));
        newTarget.setFLUXG(star.getPropertyAsDouble(Star.Property.FLUX_G));
        newTarget.setFLUXI(star.getPropertyAsDouble(Star.Property.FLUX_I));
        newTarget.setFLUXJ(star.getPropertyAsDouble(Star.Property.FLUX_J));
        newTarget.setFLUXH(star.getPropertyAsDouble(Star.Property.FLUX_H));
        newTarget.setFLUXK(star.getPropertyAsDouble(Star.Property.FLUX_K));
        // LMN magnitudes are missing in Simbad !

        // Spectral types :
        newTarget.setSPECTYP(star.getPropertyAsString(Star.Property.SPECTRALTYPES));

        // Object types :
        newTarget.setOBJTYP(star.getPropertyAsString(Star.Property.OTYPELIST));

        // Radial velocity (km/s) (optional) :
        newTarget.setSYSVEL(star.getPropertyAsDouble(Star.Property.RV));
        newTarget.setVELTYP(star.getPropertyAsString(Star.Property.RV_DEF));

        // Identifiers :
        newTarget.setIDS(star.getPropertyAsString(Star.Property.IDS));

        // fix NaN / null values:
        newTarget.checkValues();

        return newTarget;
    }

    public static void createDefaultTargetGroups(final TargetUserInformations targetUserInfos) {
        if (targetUserInfos.getGroupById(TargetGroup.GROUP_AO) == null) {
            targetUserInfos.addGroup(new TargetGroup(
                    TargetGroup.GROUP_AO,
                    "AO Star",
                    TargetGroup.CATEGORY_OB,
                    "Group indicating stars used by the Adaptive Optics system",
                    "#F781BF"
            ));
        }
        if (targetUserInfos.getGroupById(TargetGroup.GROUP_FT) == null) {
            targetUserInfos.addGroup(new TargetGroup(
                    TargetGroup.GROUP_FT,
                    "FT Star",
                    TargetGroup.CATEGORY_OB,
                    "Group gathering stars used by the Fringe Tracking system",
                    "#75C147"
            ));
        }
        if (targetUserInfos.getGroupById(TargetGroup.GROUP_GUIDE) == null) {
            targetUserInfos.addGroup(new TargetGroup(
                    TargetGroup.GROUP_GUIDE,
                    "Guide Star",
                    TargetGroup.CATEGORY_OB,
                    "Group indicating stars used by the telescope guiding",
                    "#5BAFD6"
            ));
        }
        if (false && targetUserInfos.getGroupById(TargetGroup.GROUP_SELECTED) == null) {
            targetUserInfos.addGroup(new TargetGroup(
                    TargetGroup.GROUP_SELECTED,
                    "Selected Targets",
                    TargetGroup.CATEGORY_SEL,
                    "Dynamic group indicating targets selected in ASPRO2",
                    "#8781FF"
            ));
        }

        // TODO: add more predefined groups:
        // backup
        // priority High, Normal, Low
        // status In Progress / Done
    }
}
