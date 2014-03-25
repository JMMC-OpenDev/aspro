/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.Channel;
import fr.jmmc.aspro.model.oi.DelayLine;
import fr.jmmc.aspro.model.oi.DelayLineRestrictionThrow;
import fr.jmmc.aspro.model.oi.Station;
import java.util.List;

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
    /** optional delay line throws due to delay line restrictions */
    private List<DelayLineRestrictionThrow> delayLineThrows = null;

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
     * Return the optional delay line throws due to delay line restrictions
     * @return optional delay line throws due to delay line restrictions
     */
    public List<DelayLineRestrictionThrow> getDelayLineThrows() {
        return delayLineThrows;
    }

    /**
     * Define the optional delay line throws due to delay line restrictions
     * @param delayLineThrows optional delay line throws due to delay line restrictions
     */
    public void setDelayLineThrows(final List<DelayLineRestrictionThrow> delayLineThrows) {
        this.delayLineThrows = delayLineThrows;
    }

    /**
     * Return a string representation of the Beam object
     * @return string representation of the Beam object
     */
    @Override
    public String toString() {
        return "Beam : " + this.station + " - " + this.channel + " - " + this.delayLine + " = " + this.opticalPathLength
                + " m - delayLineThrows = " + this.delayLineThrows;
    }
}
