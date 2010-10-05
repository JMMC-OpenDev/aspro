/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: Beam.java,v 1.1 2009-11-20 16:55:47 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 *
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.Channel;
import fr.jmmc.aspro.model.oi.DelayLine;
import fr.jmmc.aspro.model.oi.Station;

/**
 * This class defines the beam definition coming from the station through a delay line and arriving to a channel in the interferometer
 * @author bourgesl
 */
public class Beam {

  /** station */
  private final Station station;
  /** channel */
  private Channel channel;
  /** channel */
  private DelayLine delayLine;
  /** fixed optical length from the interferometer switchyard */
  private double opticalLength = 0d;

  /**
   * Constructor for a given station
   * @param station station
   */
  public Beam(final Station station) {
    this.station = station;
  }

  public Station getStation() {
    return station;
  }

  public Channel getChannel() {
    return channel;
  }

  public void setChannel(Channel channel) {
    this.channel = channel;
  }

  public DelayLine getDelayLine() {
    return delayLine;
  }

  public void setDelayLine(DelayLine delayLine) {
    this.delayLine = delayLine;
  }

  public double getOpticalLength() {
    return opticalLength;
  }

  public void addOpticalLength(double opticalLength) {
    this.opticalLength += opticalLength;
  }

  @Override
  public String toString() {
    return "Beam : " + this.station + " - " + this.channel + " - " + this.delayLine + " = " + this.opticalLength + " m";
  }
}
