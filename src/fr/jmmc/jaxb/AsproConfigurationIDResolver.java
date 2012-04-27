/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.jaxb;

import com.sun.xml.bind.IDResolver;
import fr.jmmc.aspro.model.Configuration;
import fr.jmmc.aspro.model.oi.Channel;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.aspro.model.oi.Station;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.SAXException;

/**
 * This custom IDResolver (JAXB) is able to resolve object identifier present in Aspro2 configuration (see ConfigurationManager)
 * i.e. references present in any InterferometerDescription object graph
 * 
 * @author bourgesl
 */
public final class AsproConfigurationIDResolver extends IDResolver {

  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(AsproConfigurationIDResolver.class.getName());

  /* members */
  /** flag indicating that the configuration must be initialized using the first reference (InterferometerDescription) */
  private boolean doConfInit = true;
  /** configuration used to resolve references */
  private final Configuration configuration;
  /** configuration reference cache */
  private final Map<String, Object> configurationReferences = new HashMap<String, Object>(128);
  /** document reference cache */
  private final Map<String, Object> documentReferences = new HashMap<String, Object>(32);

  /**
   * Aspro ID resolver using from the given configuration
   * @param configuration configuration to use
   */
  public AsproConfigurationIDResolver(final Configuration configuration) {
    this.configuration = configuration;
  }

  /**
   * Bind given object id and reference coming from the xml document
   * @param id object identifier
   * @param reference object reference
   * @throws SAXException never thrown
   */
  @Override
  public void bind(final String id, final Object reference) throws SAXException {
    logger.debug("bind('{}'): {}", id, reference);

    documentReferences.put(id, reference);
  }

  /**
   * Resolve the given object identifier having optionally its class type
   * @param id object identifier
   * @param type optional class type
   * @return resolved object reference or null if not found in document or configuration references
   * @throws SAXException never thrown
   */
  @Override
  public Callable<?> resolve(final String id, final Class type) throws SAXException {
    return new Callable() {

      @Override
      public Object call() throws SAXException {
        logger.debug("resolve('{}'): {}", id, type);

        // note: ignore type as erasure can happen (Object.class)

        Object reference = documentReferences.get(id);

        if (reference != null) {
          logger.debug("resolve('{}'): doc  ref = {}", id, reference);
          return reference;
        }

        if (doConfInit) {
          if (type == InterferometerDescription.class) {
            prepare(id);
          } else {
            // altenatively could use complete configuration cache (all InterferometerDescription) but it can lead to possible ID conflicts !
            throw new SAXException("resolve: configuration reference cache can not be initialized using identifier '" + id
                    + "' of class type [" + type + "]");
          }
        }

        reference = configurationReferences.get(id);

        if (reference == null) {
          logger.warn("resolve('{}'): unresolved reference", id);
        } else {
          logger.debug("resolve('{}'): conf ref = {}", id, reference);
        }

        return reference;
      }
    };
  }

  /**
   * Prepare the configuration reference cache from the given configuration
   * 
   * @param interferometerDescriptionID identifier of an InterferometerDescription
   */
  private void prepare(final String interferometerDescriptionID) {
    logger.debug("prepare: interferometerDescriptionID = '{}'", interferometerDescriptionID);

    final InterferometerDescription id = configuration.getInterferometerDescriptions().get(interferometerDescriptionID);

    // TODO: put all references (look at xsd:idref(s) / xsd:id occurences in InterferometerDescription) ?

    // interferometer ref:
    configurationReferences.put(id.getName(), id);

    // stations ref:
    for (Station station : id.getStations()) {
      configurationReferences.put(station.getName(), station);
    }

    // channel ref:
    for (Channel channel : id.getChannels()) {
      configurationReferences.put(channel.getName(), channel);
    }

    // pops ref:
    for (Pop pop : id.getPops()) {
      configurationReferences.put(pop.getName(), pop);
    }

    // instrument ref:
    for (FocalInstrument instrument : id.getFocalInstruments()) {
      configurationReferences.put(instrument.getName(), instrument);
    }

    // configuration cache is now initialized:
    doConfInit = false;

    logger.debug("prepare: configurationReferences =\n{}", configurationReferences);
  }
}
