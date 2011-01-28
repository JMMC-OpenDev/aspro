package fr.jmmc.aspro.model.event;

/**
   * This enumeration defines all types of observation events
   */
public enum ObservationEventType {

    /** the observation was loaded */
    LOADED,
    /** target list (target, models and calibrators) changed */
    TARGET_CHANGED,
    /** ask listeners to save swing states into the observation */
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
