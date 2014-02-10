/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.oi.ObservationCollection;
import java.util.List;

/**
 * This class extends the observation collection to have interferometer map data
 * @author bourgesl
 */
public final class ObservationCollectionMapData extends ObservationCollection {

  /** interferometer map data */
  private final List<InterferometerMapData> mapDataList;

  /**
   * Public constructor : copy information from the given observation collection (by reference)
   * @param obsCollection observation collection to copy
   * @param mapDataList interferometer map data
   */
  public ObservationCollectionMapData(final ObservationCollection obsCollection,
                                      final List<InterferometerMapData> mapDataList) {
    super(obsCollection);
    this.mapDataList = mapDataList;
  }

  /**
   * Return the interferometer map data
   * @return interferometer map data
   */
  public List<InterferometerMapData> getMapDataList() {
    return this.mapDataList;
  }

  /**
   * Return the first interferometer map data
   * @return first interferometer map data
   */
  public InterferometerMapData getFirstMapData() {
    return this.getMapDataList().get(0);
  }
}
