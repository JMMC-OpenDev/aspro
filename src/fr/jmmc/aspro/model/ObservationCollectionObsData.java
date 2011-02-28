/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ObservationCollectionObsData.java,v 1.1 2011-02-28 17:12:51 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 */
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.observability.ObservabilityData;
import fr.jmmc.aspro.model.oi.ObservationCollection;
import java.util.ArrayList;
import java.util.List;

/**
 * This class extends the observation collection to have observability data
 * @author bourgesl
 */
public class ObservationCollectionObsData extends ObservationCollection {

  /** observability data */
  private final List<ObservabilityData> obsDataList;
  /** configuration names using the format 'CONF + PoPs' */
  private final List<String> confNames;

  /**
   * Public constructor : copy information from the given observation collection (by reference)
   * @param obsCollection observation collection to copy
   * @param obsDataList observability data
   */
  public ObservationCollectionObsData(final ObservationCollection obsCollection,
                                      final List<ObservabilityData> obsDataList) {
    super(obsCollection);
    this.obsDataList = obsDataList;

    if (isSingle()) {
      this.confNames = null;
    } else {
      this.confNames = new ArrayList<String>(size());
      prepareConfigurationNames();
    }
  }

  /**
   * Prepare the configuration names using the format 'CONF + PoPs'
   */
  private final void prepareConfigurationNames() {
    final StringBuilder sb = new StringBuilder();

    // Iterate over UV Coverage data (multi conf) :
    for (ObservabilityData obsData : getObsDataList()) {
      sb.append(obsData.getStationNames());
      if (!obsData.isUserPops() && obsData.getBestPops() != null) {
        obsData.getBestPops().toString(sb);
      }

      this.confNames.add(sb.toString());
      sb.setLength(0);
    }
  }

  /**
   * Return the observability data
   * @return observability data
   */
  public final List<ObservabilityData> getObsDataList() {
    return this.obsDataList;
  }

  /**
   * Return the first observability data
   * @return first observability data
   */
  public final ObservabilityData getFirstObsData() {
    return this.getObsDataList().get(0);
  }

  /**
   * Return the flag to find baseline limits
   * @return flag to find baseline limits
   */
  public final boolean isDoBaseLineLimits() {
    return this.getFirstObsData().isDoBaseLineLimits();
  }

  /**
   * Return the flag to produce detailed output with all BL / horizon / rise intervals per target
   * @return flag to produce detailed output with all BL / horizon / rise intervals per target
   */
  public final boolean isDoDetailedOutput() {
    return this.getFirstObsData().isDoDetailedOutput();
  }

  /**
   * Return the configuration names using the format 'CONF + PoPs'
   * @return the configuration names using the format 'CONF + PoPs'
   */
  public final List<String> getConfigurationNames() {
    return this.confNames;
  }
}
