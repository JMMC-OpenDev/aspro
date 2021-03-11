/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.oi.ObservationCollection;
import fr.jmmc.aspro.model.uvcoverage.UVCoverageData;
import fr.jmmc.aspro.service.OIFitsCreatorService;
import fr.jmmc.jmal.model.UVMapData;
import java.util.List;

/**
 * This class extends the observation collection to have observability, UV Coverage and UV Map map data
 * @author bourgesl
 */
public final class ObservationCollectionUVData extends ObservationCollectionObsData {

    /** UV Coverage data */
    private final List<UVCoverageData> uvDataList;
    /** uv map data */
    private UVMapData uvMapData = null;
    /** warning container (merged) */
    private WarningContainer warningContainer;
    /** flag indicating that OIFits computation is done */
    private boolean OIFitsDone = false;

    /**
     * Public constructor : copy information from the given observation collection (by reference)
     * @param obsCollection observation collection to copy
     * @param obsDataList observability data
     * @param uvDataList UV Coverage data
     */
    public ObservationCollectionUVData(final ObservationCollection obsCollection,
                                       final List<ObservabilityData> obsDataList, final List<UVCoverageData> uvDataList) {
        super(obsCollection, obsDataList);
        this.uvDataList = uvDataList;
    }

    /**
     * Return the UV Coverage data
     * @return UV Coverage data
     */
    public List<UVCoverageData> getUVDataList() {
        return this.uvDataList;
    }

    /**
     * Return the first UV Coverage data
     * @return first UV Coverage data
     */
    public UVCoverageData getFirstUVData() {
        return this.getUVDataList().get(0);
    }

    /**
     * Return the target name corresponding to this data collection
     * @return target name
     */
    public String getTargetName() {
        return getFirstUVData().getTargetName();
    }

    /**
     * Return the UV Map data
     * @return UV Map data
     */
    public UVMapData getUVMapData() {
        return this.uvMapData;
    }

    /**
     * Define the UV Map data
     * @param uvMapData UV Map data
     */
    public void setUvMapData(final UVMapData uvMapData) {
        this.uvMapData = uvMapData;
    }

    /**
     * Return the warning container (merged)
     * @return warning container (merged)
     */
    public WarningContainer getWarningContainer() {
        return warningContainer;
    }

    /**
     * Define the warning container (merged)
     * @param warningContainer warning container (merged)
     */
    public void setWarningContainer(final WarningContainer warningContainer) {
        this.warningContainer = warningContainer;
    }

    /**
     * Return the flag indicating that OIFits computation is done
     * @return flag indicating that OIFits computation is done
     */
    public boolean isOIFitsDone() {
        return OIFitsDone;
    }

    /**
     * Define the flag indicating that OIFits computation is done
     * @param OIFitsDone flag indicating that OIFits computation is done
     */
    public void setOIFitsDone(final boolean OIFitsDone) {
        this.OIFitsDone = OIFitsDone;
    }

    /**
     * Check if this ObservationCollectionUVData has the same input parameters to reuse its computed OIFits data :
     * @param uvDataCollection another ObservationCollectionUVData previously computed
     * @return true only if input parameters are equals
     */
    public boolean isOIFitsValid(final ObservationCollectionUVData uvDataCollection) {

        // note: OIFitsCreatorService parameter dependencies:
        // observation {target, instrumentMode {lambdaMin, lambdaMax, nSpectralChannels}}
        // obsData {beams, baseLines, starData, sc (DateCalc)}
        // parameter: supersamplingOIFits, doDataNoise, useInstrumentBias
        // results: computeObservableUV {HA, targetUVObservability} {obsData + observation{haMin/haMax, instrumentMode {lambdaMin, lambdaMax}}}
        // and warning container
        // check if computation needed:
        // - check same target
        // - check observation (target and UV version includes main version)
        // - check obsData (main version)
        // - check supersamplingOIFits
        // - check doNoise: noiseService.isDoNoise()
        // - check useInstrumentBias: noiseService.isUseCalibrationBias()
        if (!getTargetName().equals(uvDataCollection.getTargetName())) {
            return false;
        }
        if (!getVersion().isSameTargetVersion(uvDataCollection.getVersion())) {
            return false;
        }
        if (!getVersion().isSameUVVersion(uvDataCollection.getVersion())) {
            return false;
        }
        if (!getFirstObsData().getVersion().isSameMainVersion(uvDataCollection.getFirstObsData().getVersion())) {
            return false;
        }

        // check if OIFitsCreatorServices have same doNoise flag and identical OIFits preferences:
        final OIFitsCreatorService current = getFirstUVData().getOiFitsCreator();
        final OIFitsCreatorService other = uvDataCollection.getFirstUVData().getOiFitsCreator();

        if (current != null && other != null) {
            if (current.getSupersampling() != other.getSupersampling()) {
                return false;
            }
            if (current.getMathMode() != other.getMathMode()) {
                return false;
            }
            if (current.isDoNoise() != other.isDoNoise()) {
                return false;
            }
            if (current.isUseCalibrationBias() != other.isUseCalibrationBias()) {
                return false;
            }
            if (current.getSnrThreshold() != other.getSnrThreshold()) {
                return false;
            }
        }
        return true;
    }
}
