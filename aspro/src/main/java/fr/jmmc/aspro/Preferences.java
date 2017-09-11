/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro;

import fr.jmmc.aspro.model.TimeRef;
import fr.jmmc.aspro.model.observability.SunTimeInterval.SunType;
import fr.jmmc.aspro.service.UserModelService.MathMode;
import fr.jmmc.aspro.service.pops.BestPopsEstimatorFactory.Algorithm;
import fr.jmmc.aspro.service.pops.Criteria;
import fr.jmmc.jmcs.data.preference.PreferencesException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Handles preferences for Aspro.
 *
 * Note : There is a special preference 'splash.screen.show' used to disable the splash screen (dev mode) if its value is 'false'.
 */
public final class Preferences extends fr.jmmc.oiexplorer.core.Preferences {

    /** Singleton instance */
    private static Preferences _singleton = null;
    /** Logger */
    private static final Logger logger = LoggerFactory.getLogger(Preferences.class.getName());
    /* Preferences */
    /** Preference : enable/disable GUI restrictions */
    public final static String GUI_RESTRICTIONS = "gui.restrictions";
    /** Preference : edit positions in XY (true) or rho/theta (false) in the model editor */
    public final static String MODELEDITOR_PREFERXY = "modeleditor.preferxy";
    /** Preference : Image size to use for the object model image in the UV Coverage plot */
    public final static String MODEL_IMAGE_SIZE = "model.image.size";
    /** Preference : Enable noise modeling for the object model image in the UV Coverage plot */
    public final static String MODEL_IMAGE_NOISE = "model.image.noise";
    /** Preference : Enable fast user model (optimize FFT and direct FT i.e. skip useless data) */
    public final static String MODEL_USER_FAST = "model.user.fast";
    /** Preference : time reference (LST/UTC) */
    public final static String TIME_REFERENCE = "time.reference";
    /** Preference : minimum elevation */
    public final static String MIN_ELEVATION = "min.elevation";
    /** Preference : center observability plot arround night */
    public final static String CENTER_NIGHT = "center.night";
    /** Preference : twilight night (Astro_Twilight/Nautic_Twilight/Civil_Twilight): see SunTimeInterval */
    public final static String TWILIGHT_NIGHT = "twilight.night";
    /** Preference : show only night */
    public final static String ONLY_NIGHT = "only.night";
    /** Preference : best Pops algorithm */
    public final static String BEST_POPS_ALGORITHM = "bestPops.algorithm";
    /** Preference : best Pops criteria on sigma */
    public final static String BEST_POPS_CRITERIA_SIGMA = "bestPops.criteria.sigma";
    /** Preference : best Pops criteria on average weight */
    public final static String BEST_POPS_CRITERIA_AVERAGE_WEIGHT = "bestPops.criteria.averageWeight";
    /** Preference : Add noise to OIFits data */
    public final static String OIFITS_ADD_NOISE = "oifits.noise";
    /** Preference : MathMode used by OIFits computation (QUICK, FAST, DEFAULT) */
    public final static String OIFITS_MATH_MODE = "oifits.math.mode";
    /** Preference : Number of complex visibility samples used to compute the complex visibility of each spectral channel (1, 5, 10 ...) */
    public final static String OIFITS_SUPER_SAMPLING = "oifits.supersampling";

    /**
     * Private constructor that must be empty.
     *
     * @param notify flag to enable/disable observer notifications
     */
    private Preferences(final boolean notify) {
        super(notify);
    }

    /**
     * Return the singleton instance of Preferences.
     *
     * @return the singleton preference instance
     */
    public synchronized static Preferences getInstance() {
        // Build new reference if singleton does not already exist
        // or return previous reference
        if (_singleton == null) {
            logger.debug("Preferences.getInstance()");

            // disable notifications:
            _singleton = new Preferences(false);

            // register color palette observer:
            _singleton.addColorPaletteObserver();

            // enable future notifications:
            _singleton.setNotify(true);
        }

        return _singleton;
    }

    /**
     * Define the default properties used to reset default preferences.
     *
     * @throws PreferencesException if any preference value has a unsupported class type
     */
    @Override
    protected void setDefaultPreferences() throws PreferencesException {
        super.setDefaultPreferences();

        logger.debug("Preferences.setDefaultPreferences()");

        // Gui restrictions:
        setDefaultPreference(GUI_RESTRICTIONS, Boolean.TRUE);

        // Model editor:
        setDefaultPreference(MODELEDITOR_PREFERXY, Boolean.FALSE);

        // UV Coverage - image size and LUT:
        setDefaultPreference(MODEL_IMAGE_SIZE, AsproConstants.DEFAULT_IMAGE_SIZE);
        setDefaultPreference(MODEL_IMAGE_NOISE, Boolean.FALSE);

        // User model:
        setDefaultPreference(MODEL_USER_FAST, Boolean.TRUE);

        // Time reference:
        setDefaultPreference(TIME_REFERENCE, TimeRef.LST.getDisplayName());

        // minimum elevation:
        setDefaultPreference(MIN_ELEVATION, Double.valueOf(AsproConstants.DEFAULT_MIN_ELEVATION));

        // center observability plot arround night:
        setDefaultPreference(CENTER_NIGHT, Boolean.TRUE);

        // twilight night (Astro_Twilight):
        setDefaultPreference(TWILIGHT_NIGHT, SunType.Night.toString());

        // show only night on observability plot:
        setDefaultPreference(ONLY_NIGHT, Boolean.TRUE);

        // best Pops algorithm:
        setDefaultPreference(BEST_POPS_ALGORITHM, Algorithm.HALimits.toString());
        setDefaultPreference(BEST_POPS_CRITERIA_SIGMA, Criteria.MEDIUM.toString());
        setDefaultPreference(BEST_POPS_CRITERIA_AVERAGE_WEIGHT, Criteria.LARGE.toString());

        // OIFits computation:
        setDefaultPreference(OIFITS_ADD_NOISE, Boolean.TRUE);
        setDefaultPreference(OIFITS_SUPER_SAMPLING, AsproConstants.DEFAULT_SUPER_SAMPLING);
        setDefaultPreference(OIFITS_MATH_MODE, MathMode.FAST.toString());
    }

    /**
     * Return the preference filename.
     *
     * @return preference filename.
     */
    @Override
    protected String getPreferenceFilename() {
        return "fr.jmmc.aspro.properties";
    }

    /**
     *  Return preference version number.
     *
     * @return preference version number.
     */
    @Override
    protected int getPreferencesVersionNumber() {
        return 1;
    }

    /**
     * Return the twilight night Preference : use preferences or Astro_Twilight if it is undefined
     * @return twilight night (Astro_Twilight/Nautic_Twilight/Civil_Twilight)
     */
    public SunType getTwilightAsNightLimit() {
        final String value = getPreference(TWILIGHT_NIGHT);

        try {
            return SunType.valueOf(value);
        } catch (IllegalArgumentException iae) {
            logger.debug("ignored invalid value: {}", value);
        }
        return SunType.Night;
    }

    /**
     * Return true if the fast mode for user model is enabled
     * @return true if the fast mode for user model is enabled 
     */
    public boolean isFastUserModel() {
        return getPreferenceAsBoolean(Preferences.MODEL_USER_FAST);
    }

    /**
     * Return the best Pops algorithm Preference : use preferences or Algorithm.HALimits if it is undefined
     * @return best Pops algorithm
     */
    public Algorithm getBestPopsAlgorithm() {
        final String value = getPreference(BEST_POPS_ALGORITHM);

        try {
            return Algorithm.valueOf(value);
        } catch (IllegalArgumentException iae) {
            logger.debug("ignored invalid value: {}", value);
        }
        return Algorithm.HALimits;
    }

    /**
     * Return the best Pops criteria on sigma Preference : use preferences or Criteria.MEDIUM if it is undefined
     * @return Criteria
     */
    public Criteria getBestPopsCriteriaSigma() {
        return getCriteria(BEST_POPS_CRITERIA_SIGMA, Criteria.MEDIUM);
    }

    /**
     * Return the best Pops criteria on average weight : use preferences or Criteria.LARGE if it is undefined
     * @return Criteria
     */
    public Criteria getBestPopsCriteriaAverageWeight() {
        return getCriteria(BEST_POPS_CRITERIA_AVERAGE_WEIGHT, Criteria.LARGE);
    }

    /**
     * Return the Criteria Preference for the given preference name: use preferences or def value if it is undefined
     * @param preferenceName preference key
     * @param def default value
     * @return Criteria instance
     */
    private Criteria getCriteria(final Object preferenceName, final Criteria def) {
        final String value = getPreference(preferenceName);

        try {
            return Criteria.valueOf(value);
        } catch (IllegalArgumentException iae) {
            logger.debug("ignored invalid value: {}", value);
        }
        return def;
    }

    /**
     * Return the MathMode used by OIFits computation (QUICK, FAST, DEFAULT)
     * @return MathMode used by OIFits computation (QUICK, FAST, DEFAULT)
     */
    public MathMode getOIFitsMathMode() {
        final String value = getPreference(OIFITS_MATH_MODE);

        try {
            return MathMode.valueOf(value);
        } catch (IllegalArgumentException iae) {
            logger.debug("ignored invalid value: {}", value);
        }
        return MathMode.FAST;
    }
}
