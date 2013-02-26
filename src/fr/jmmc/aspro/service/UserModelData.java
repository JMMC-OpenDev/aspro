/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.model.OIBase;
import fr.jmmc.aspro.model.Range;
import fr.jmmc.oitools.image.FitsImage;
import fr.jmmc.oitools.image.FitsImageHDU;

/**
 * This class gathers prepared user model data as FitsImage (FFT) and 1D arrays (exact FT) (normalized flux and X/Y spatial coordinates) 
 * @author bourgesl
 */
public final class UserModelData extends OIBase {

    /* members */
    /** FFT ready fits image */
    private FitsImage fitsImage = null;
    /** wavelength range of the fits image (fits cube) */
    private Range wavelengthRange = null;
    /** 
     * Flattened data points (1D) stored with col and row: [data col row] 
     * spatial coordinates along the column and row axis (rad)
     */
    private float[] data1D = null;

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
        // TODO: remove
        return fitsImage.getIncWL();
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
     * Define the FFT ready fits image
     * @param fitsImage FFT ready fits image
     */
    public void setFitsImage(final FitsImage fitsImage) {
        this.fitsImage = fitsImage;
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
     * Return the number of data points
     * 
     * TODO: x 3 or / 3: decide soon
     * 
     * @return number of data points
     */
    public int getNData() {
        return this.data1D.length;
    }

    /**
     * Return the flattened data points (1D) [data col row]
     * @return flattened data points (1D) [data col row]
     */
    public float[] getData1D() {
        return data1D;
    }

    /**
     * @return string representation from FitsImage information
     */
    @Override
    public String toString() {
        return "UserModelData:" + getFitsImage() + " - nData=" + getNData();
    }
}
