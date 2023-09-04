/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.oi.FocalInstrument;

/**
 * Simple Java bean to store apodization parameters
 * @author bourgesl
 */
public final class ApodizationParameters {

    /** telescope diameter in meters */
    public final double diameter;
    /** minimum wavelength in meters */
    public final double lambdaMin;
    /** maximum wavelength in meters */
    public final double lambdaMax;
    /** (optional) reference wavelength for the spatial filter (fixed radius like pinhole) */
    public final double lambdaRef;
    /** (optional) scaling factor proportional to (lambda / diameter) */
    public final double scalingFactor;
    /* outputs */
    /** wavelength used by apodization (airy radius) */
    public double lambda;

    /**
     * Constructor with undefined parameters
     */
    public ApodizationParameters() {
        this(Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN);
    }

    /**
     * Constructor with basic parameters
     * @param diameter telescope diameter in meters
     * @param lambdaMin minimum wavelength in meters
     */
    public ApodizationParameters(final double diameter, final double lambdaMin) {
        this(diameter, lambdaMin, Double.NaN, Double.NaN, Double.NaN);
    }

    /**
     * Constructor with basic parameters
     * @param diameter telescope diameter in meters
     * @param lambdaMin minimum wavelength in meters
     * @param lambdaMax maximum wavelength in meters
     */
    public ApodizationParameters(final double diameter, final double lambdaMin, final double lambdaMax) {
        this(diameter, lambdaMin, lambdaMax, Double.NaN, Double.NaN);
    }

    /**
     * Constructor with advanced parameters
     * @param diameter telescope diameter in meters
     * @param lambdaMin minimum wavelength in meters
     * @param lambdaMax maximum wavelength in meters
     * @param instrument instrument in use to get instrument's spatial filter parameters
     */
    public ApodizationParameters(final double diameter, final double lambdaMin, final double lambdaMax, final FocalInstrument instrument) {
        this(diameter, lambdaMin, lambdaMax, getWaveLengthRef(instrument), getScalingFactor(instrument));
    }

    private ApodizationParameters(final double diameter, final double lambdaMin, final double lambdaMax,
                                  final double lambdaRef, final double scalingFactor) {
        this.diameter = diameter;
        this.lambdaMin = lambdaMin;
        this.lambdaMax = lambdaMax;
        this.lambdaRef = lambdaRef;
        this.scalingFactor = scalingFactor;
        this.lambda = lambdaMin;
    }

    private static double getWaveLengthRef(final FocalInstrument instrument) {
        if ((instrument != null) && (instrument.getSpatialFilter() != null)) {
            return instrument.getSpatialFilter().getWaveLengthRefOrNaN() * AsproConstants.MICRO_METER;
        }
        return Double.NaN;
    }

    private static double getScalingFactor(final FocalInstrument instrument) {
        if ((instrument != null) && (instrument.getSpatialFilter() != null)) {
            return instrument.getSpatialFilter().getScalingFactorOrNaN();
        }
        return Double.NaN;
    }

    @Override
    public String toString() {
        return "ApodizationParameters{"
                + "diameter=" + diameter + ", lambdaMin=" + lambdaMin + ", lambdaMax=" + lambdaMax
                + ", lambdaRef=" + lambdaRef + ", scalingFactor=" + scalingFactor
                + ", lambda=" + lambda + '}';
    }

}
