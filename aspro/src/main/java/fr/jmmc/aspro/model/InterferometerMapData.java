/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

/**
 * This class contains the results of the InterferometerMap service
 * @author bourgesl
 */
public final class InterferometerMapData {

    // maximum value for either X or Y value :
    private double maxXY;
    // stations :
    private String[] stationName;
    private double[] diameter;
    private double[] stationX;
    private double[] stationY;
    /** configuration */
    private String stationNames;
    private String sortedStationNames;
    // base lines :
    /* baseline name = "'S1'-'S2'" */
    private String[] baselineName;
    /* baseline name = "'S1'-'S2' length m" */
    private String[] baselineLabel;
    private double[] baselineStationX1;
    private double[] baselineStationY1;
    private double[] baselineStationX2;
    private double[] baselineStationY2;

    /**
     * Public constructor
     */
    public InterferometerMapData() {
        super();
    }

    public double getMaxXY() {
        return maxXY;
    }

    public void setMaxXY(final double maxXY) {
        this.maxXY = maxXY;
    }

    public String[] getStationName() {
        return stationName;
    }

    public void setStationName(final String[] name) {
        this.stationName = name;
    }

    public int getStationIndex(final String name) {
        for (int i = 0, len = this.stationName.length; i < len; i++) {
            if (name.equals(this.stationName[i])) {
                return i;
            }
        }
        return -1;
    }

    public double[] getDiameter() {
        return diameter;
    }

    public void setDiameter(final double[] diameter) {
        this.diameter = diameter;
    }

    public double[] getStationX() {
        return stationX;
    }

    public void setStationX(final double[] x) {
        this.stationX = x;
    }

    public double[] getStationY() {
        return stationY;
    }

    public void setStationY(final double[] y) {
        this.stationY = y;
    }

    public String getStationNames() {
        return stationNames;
    }

    public void setStationNames(final String stationNames) {
        this.stationNames = stationNames;
    }

    public String getSortedStationConf() {
        return sortedStationNames;
    }

    public void setSortedStationNames(final String stationNames) {
        this.sortedStationNames = stationNames;
    }

    public String[] getBaselineName() {
        return baselineName;
    }

    public void setBaselineName(final String[] name) {
        this.baselineName = name;
    }

    public String[] getBaselineLabel() {
        return baselineLabel;
    }

    public void setBaselineLabel(String[] baselineLabel) {
        this.baselineLabel = baselineLabel;
    }

    public double[] getBaselineStationX1() {
        return baselineStationX1;
    }

    public void setBaselineStationX1(final double[] x) {
        this.baselineStationX1 = x;
    }

    public double[] getBaselineStationX2() {
        return baselineStationX2;
    }

    public void setBaselineStationX2(final double[] x) {
        this.baselineStationX2 = x;
    }

    public double[] getBaselineStationY1() {
        return baselineStationY1;
    }

    public void setBaselineStationY1(final double[] y) {
        this.baselineStationY1 = y;
    }

    public double[] getBaselineStationY2() {
        return baselineStationY2;
    }

    public void setBaselineStationY2(final double[] y) {
        this.baselineStationY2 = y;
    }

}
