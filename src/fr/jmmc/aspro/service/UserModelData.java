/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.model.OIBase;
import fr.jmmc.oitools.image.FitsImage;

/**
 * This class gathers prepared user model data as FitsImage (FFT) and 1D arrays (exact FT) (normalized flux and X/Y spatial coordinates) 
 * @author bourgesl
 */
public final class UserModelData extends OIBase {

    /* members */
    /** FFT ready fits image */
    private FitsImage fitsImage = null;
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

    /**
     * Return the image wavelength in meters
     * @return image wavelength in meters or Double.NaN if undefined
     */
    public Double getWaveLength() {
        return fitsImage.getWaveLength();
    }

    /**
     * Return the wavelength increment along the wavelength axis (meter per pixel)
     * @return wavelength increment along the wavelength axis (meter per pixel)
     */
    public double getIncWL() {
        return fitsImage.getIncWL();
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
