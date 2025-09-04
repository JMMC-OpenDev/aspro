/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

/**
 *
 * @author bourgesl
 */
public enum TargetRole {
    AO, FT, SCI,
    SINGLE_FIELD    /* special flag (GRAVITY single field ie SCI = FT) */,
    GPAO_LGS        /* special flag (GPAO Laser guide star) */,
    GPAO_VIS        /* special flag (GPAO VIS WFS not IR WFS) */;
}
