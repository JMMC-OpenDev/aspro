/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservationEventType.java,v 1.4 2011-02-22 18:11:30 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.3  2011/01/31 15:24:34  bourgesl
 * added file header
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.model.event;

/**
   * This enumeration defines all types of observation events
   */
public enum ObservationEventType {

    /** the observation was loaded */
    LOADED,
    /** target list (target, models and calibrators) changed */
    TARGET_CHANGED,
    /** target selection changed */
    TARGET_SELECTION_CHANGED,
    /** ask listeners to save thier swing state into the main observation */
    DO_UPDATE,
    /** one or more general attribute(s) changed */
    REFRESH,
    /** one or more attribute(s) used for UV related changed */
    REFRESH_UV,
    /** the observability was computed */
    OBSERVABILITY_DONE,
    /** the warnings are ready */
    WARNINGS_READY,
    /** the OIFits was computed */
    OIFITS_DONE

    /* TODO Missing UVCoverageDone event */
  }
