/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.Channel;
import fr.jmmc.aspro.model.oi.DelayLine;
import fr.jmmc.aspro.model.oi.Station;

/**
 * This class defines the beam definition coming from the station through a delay line and arriving to a channel in the interferometer
 * @author bourgesl
 */
public final class Beam {

    /** station */
    private final Station station;
    /** channel */
    private Channel channel;
    /** channel */
    private DelayLine delayLine;
    /** fixed optical length (m) from the interferometer switchyard */
    private double opticalPathLength = 0d;
    /** optional delay line / VCM low constraint expressed in throw (m) */
    private Double maximumThrowLow = null;
    /** optional delay line / VCM high constraint expressed in throw (m) */
    private Double maximumThrowHigh = null;

    /**
     * Constructor for a given station
     * @param station station
     */
    public Beam(final Station station) {
        this.station = station;
    }

    /**
     * Return the station
     * @return station
     */
    public Station getStation() {
        return station;
    }

    /**
     * Return the channel
     * @return channel
     */
    public Channel getChannel() {
        return channel;
    }

    /**
     * Set the channel
     * @param channel channel to use
     */
    public void setChannel(final Channel channel) {
        this.channel = channel;
    }

    /**
     * Return the delay line
     * @return delay line
     */
    public DelayLine getDelayLine() {
        return delayLine;
    }

    /**
     * Set the delay line
     * @param delayLine delay line
     */
    public void setDelayLine(final DelayLine delayLine) {
        this.delayLine = delayLine;
    }

    /**
     * Return the fixed optical path length (m)
     * @return fixed optical path length
     */
    public double getOpticalPathLength() {
        return opticalPathLength;
    }

    /**
     * Add the given value to the fixed optical path length (m)
     * @param opticalPathLength length (m) to add to the fixed optical path length
     */
    public void addToOpticalPathLength(final double opticalPathLength) {
        this.opticalPathLength += opticalPathLength;
    }

    /**
     * Return the optional delay line / VCM low constraint expressed in throw (m)
     * @return optional delay line / VCM low constraint expressed in throw (m)
     */
    public Double getMaximumThrowLow() {
        return maximumThrowLow;
    }

    /**
     * Define the optional delay line / VCM low constraint expressed in throw (m)
     * @param maximumThrowLow optional delay line / VCM low constraint expressed in throw (m)
     */
    public void setMaximumThrowLow(final Double maximumThrowLow) {
        this.maximumThrowLow = maximumThrowLow;
    }

    /**
     * Return the optional delay line / VCM high constraint expressed in throw (m)
     * @return optional delay line / VCM high constraint expressed in throw (m)
     */
    public Double getMaximumThrowHigh() {
        return maximumThrowHigh;
    }

    /**
     * Define the optional delay line / VCM high constraint expressed in throw (m)
     * @param maximumThrowHigh optional delay line / VCM high constraint expressed in throw (m)
     */
    public void setMaximumThrowHigh(final Double maximumThrowHigh) {
        this.maximumThrowHigh = maximumThrowHigh;
    }

    /**
     * Return a string representation of the Beam object
     * @return string representation of the Beam object
     */
    @Override
    public String toString() {
        return "Beam : " + this.station + " - " + this.channel + " - " + this.delayLine + " = " + this.opticalPathLength
                + " m [ maxThrow Low = " + this.maximumThrowLow + " High = " + this.maximumThrowHigh + " m ]";
    }
}
