/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.model.OIBase;
import fr.jmmc.oitools.model.range.Range;
import fr.jmmc.oitools.image.FitsImage;
import fr.jmmc.oitools.image.FitsImageHDU;
import java.awt.Rectangle;

/**
 * This class gathers prepared user model data as FitsImage (FFT) and 1D arrays (exact FT) (normalized flux and X/Y spatial coordinates) 
 * @author bourgesl
 */
public final class UserModelData extends OIBase {

    /* members */
    /** FFT ready fits image */
    private FitsImage fitsImage = null;
    /** threshold flux (temporary) */
    private float thresholdFlux = 0f;
    /** image ROI (temporary) */
    private Rectangle roi = null;
    /** wavelength range of the fits image (fits cube) */
    private Range wavelengthRange = null;
    /** 
     * Flattened data points (1D) stored with col and row: [data xfreq yfreq] 
     * spatial coordinates along the column and row axis (rad)
     */
    private float[] data1D = null;
    /** totalFlux flux (before normalization) */
    private double totalFlux = Double.NaN;
    /** airy radius (apodization) */
    private double airyRadius = Double.NaN;

    /**
     * Public constructor
     */
    public UserModelData() {
        super();
    }

    /**
     * Return the FFT ready fits image
     * @return FFT ready fits image
     */
    public FitsImage getFitsImage() {
        return fitsImage;
    }

    /**
     * Define the FFT ready fits image
     * @param fitsImage FFT ready fits image
     */
    public void setFitsImage(final FitsImage fitsImage) {
        this.fitsImage = fitsImage;
    }

    /**
     * Return the totalFlux flux (before normalization)
     * @return totalFlux flux (before normalization)
     */
    public double getTotalFlux() {
        return totalFlux;
    }

    /**
     * Define the totalFlux flux (before normalization)
     * @param total totalFlux flux (before normalization)
     */
    public void setTotalFlux(final double total) {
        this.totalFlux = total;
    }

    public float getThresholdFlux() {
        return thresholdFlux;
    }

    public void setThresholdFlux(float thresholdFlux) {
        this.thresholdFlux = thresholdFlux;
    }

    public Rectangle getRoi() {
        return roi;
    }

    public void setRoi(Rectangle roi) {
        this.roi = roi;
    }

    /* image meta data */
    /**
     * Return the FITS image HDU
     * @return FITS image HDU
     */
    public FitsImageHDU getFitsImageHDU() {
        return fitsImage.getFitsImageHDU();
    }

    /**
     * Return the wavelength increment along the wavelength axis (meter per pixel)
     * @return wavelength increment along the wavelength axis (meter per pixel)
     */
    public double getWaveLengthIncrement() {
        return fitsImage.hasIncWL() ? getWaveLengthRange().getLength() : Double.NaN;
    }

    /**
     * Return the image wavelength in meters or Double.NaN if undefined
     * @return image wavelength in meters or Double.NaN if undefined
     */
    public double getWaveLength() {
        return fitsImage.getWaveLength();
    }

    /**
     * Return the image wavelength range in meters [wavelength - 1/2 increment_wavelength; wavelength + 1/2 increment_wavelength] or Double.NaN if wavelength is undefined
     * @return the image wavelength range in meters
     */
    public Range getWaveLengthRange() {
        if (this.wavelengthRange == null) {
            this.wavelengthRange = new Range(fitsImage.getWaveLengthMin(), fitsImage.getWaveLengthMax());
        }
        return this.wavelengthRange;
    }

    /**
     * Define model data
     * @param data1D flattened data points (1D)
     * @return this instance
     */
    public UserModelData set(final float[] data1D) {
        this.data1D = data1D;
        return this;
    }

    /**
     * Return the number of data1D elements (3 x data points)
     * 
     * @return number of data1D elements
     */
    public int getNData() {
        return (this.data1D != null) ? this.data1D.length : 0;
    }

    /**
     * Return the flattened data points (1D) [data xfreq yfreq] 
     * @return flattened data points (1D) [data xfreq yfreq] 
     */
    public float[] getData1D() {
        return data1D;
    }

    /**
     * Return the airy radius (apodization)
     * @return airy radius (apodization)
     */
    public double getAiryRadius() {
        return airyRadius;
    }

    /**
     * Define the airy radius (apodization)
     * @param airyRadius airy radius (apodization)
     */
    public void setAiryRadius(final double airyRadius) {
        this.airyRadius = airyRadius;
    }

    /**
     * @return string representation from FitsImage information
     */
    @Override
    public String toString() {
        return "UserModelData{ " + getFitsImage() + " nData: " + getNData()
                + " airyRadius: " + getAiryRadius() + " totalFlux: " + getTotalFlux()
                + " wl: " + getWaveLength() + " range: " + getWaveLengthRange() + '}';
    }

    public int getMemorySize() {
        int len = 0;
        if (fitsImage != null) {
            len += fitsImage.getNbRows() * fitsImage.getNbCols();
        }
        if (data1D != null) {
            len += data1D.length;
        }
        return len * 4;
    }
}
