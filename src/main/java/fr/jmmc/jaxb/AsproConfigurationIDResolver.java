/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.jaxb;

import com.sun.xml.bind.IDResolver;
import fr.jmmc.aspro.model.Configuration;
import fr.jmmc.aspro.model.oi.Channel;
import fr.jmmc.aspro.model.oi.DelayLine;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.aspro.model.oi.Station;
import fr.jmmc.aspro.model.oi.SwitchYard;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.SAXException;

/**
 * This custom IDResolver (JAXB) is able to resolve object identifier present in Aspro2 configuration (see ConfigurationManager)
 * i.e. references present in any InterferometerConfiguration instance
 * 
 * @author bourgesl
 */
public final class AsproConfigurationIDResolver extends IDResolver {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(AsproConfigurationIDResolver.class.getName());

    /* members */
    /** flag indicating that the configuration must be initialized using the first reference (InterferometerDescription) */
    private boolean doConfInit = true;
    /** flag for debug logs */
    private final boolean doLogDebug = logger.isDebugEnabled();
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
        if (doLogDebug) {
            logger.debug("bind   ('{}'): document ref = {} ({})",
                    id, reference, reference.getClass().getSimpleName());
        }
        addDocumentReference(id, reference);
    }

    private void addDocumentReference(final String id, final Object reference) {
        Object prev = getDocumentReference(id);
        if (prev != null) {
            logger.debug("addDocumentReference('{}') conflict with previous = {}", id, prev);
        }
        // Anyway overwrite the reference:
        documentReferences.put(id, reference);
    }

    private Object getDocumentReference(final String id) {
        return documentReferences.get(id);
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
        return new Callable<Object>() {

            @Override
            public Object call() throws SAXException {
                if (doLogDebug) {
                    logger.debug("resolve('{}'): {}", id, type);
                }

                // note: ignore type as erasure can happen (Object.class)
                // Resolve ID in document:
                Object reference = getDocumentReference(id);

                if (reference != null) {
                    if (doLogDebug) {
                        logger.debug("resolve('{}'): document ref = {} ({})",
                                id, reference, reference.getClass().getSimpleName());
                    }
                } else {
                    if (doConfInit) {
                        doConfInit = false;

                        if (type == InterferometerDescription.class) {
                            // when loading an extendedConfiguration element,
                            // the first reference is the InterferometerDescription reference.
                            // prepare the configuration references with needed InterferometerDescription internal references:
                            prepare(id);
                        } else {
                            // altenatively could use complete configuration cache (all InterferometerDescription) but it can lead to possible ID conflicts !
                            throw new SAXException("resolve: configuration reference cache can not be initialized using identifier '" + id
                                    + "' of class type [" + type + "]");
                        }
                    }
                    // Resolve ID in configuration:
                    reference = getConfigurationReference(id);

                    if (reference != null) {
                        if (doLogDebug) {
                            logger.debug("resolve('{}'): configuration ref = {} ({})",
                                    id, reference, reference.getClass().getSimpleName());
                        }
                    } else {
                        logger.warn("resolve('{}'): unresolved reference", id);

                        if (true) {
                            throw new SAXException("resolve: unresolved identifier '" + id + "' of class type [" + type + "]");
                        }
                        // note: return null to ignore invalid references
                    }
                }
                return reference;
            }
        };
    }

    /**
     * Prepare the configuration reference cache for the given InterferometerDescription id
     * 
     * @param interferometerDescriptionID identifier of an InterferometerDescription
     * @throws org.xml.sax.SAXException if 
     */
    private void prepare(final String interferometerDescriptionID) throws SAXException {
        if (doLogDebug) {
            logger.debug("prepare: interferometerDescriptionID = '{}'", interferometerDescriptionID);
        }

        final InterferometerDescription id = configuration.getInterferometerDescription(interferometerDescriptionID);

        if (id == null) {
            throw new SAXException("prepare: unresolved InterferometerDescription identifier '" + id + "' !");
        }

        /*
        Loading an ObservationSetting.extendedConfiguration requires the following IDREFS to be resolvable:
            <xsd:complexType name="InterferometerConfiguration">

                <xsd:element name="interferometer" type="xsd:IDREF"> <jaxb:baseType name="InterferometerDescription" />        

                <xsd:element name="switchyard" type="xsd:IDREF" minOccurs="0"> <jaxb:baseType name="SwitchYard" />                  X

                <xsd:element name="instrument" type="FocalInstrumentConfiguration" minOccurs="0" maxOccurs="unbounded">


            <xsd:complexType name="FocalInstrumentConfiguration">

                <xsd:element name="focalInstrument" type="xsd:IDREF"> <jaxb:baseType name="FocalInstrument" />

                <xsd:element name="configuration" type="FocalInstrumentConfigurationItem" minOccurs="0" maxOccurs="unbounded">


            <xsd:complexType name="FocalInstrumentConfigurationItem">

                    <xsd:element name="stations" type="xsd:IDREFS"> <jaxb:baseType name="Station" />                                X

                    <xsd:element name="channels" type="xsd:IDREFS" minOccurs="0"> <jaxb:baseType name="Channel" />                  X

                    <xsd:element name="delayLines" type="xsd:IDREFS" minOccurs="0"> <jaxb:baseType name="DelayLine" />              X

                    <xsd:element name="pops" type="xsd:IDREFS" minOccurs="0"> <jaxb:baseType name="Pop" />                          X
        
        For example:
            <extendedConfiguration>
              <!-- custom name to create a new custom configuration -->
              <version>VEGA PIVOT</version>
              <!-- interferometer required -->
              <interferometer>CHARA</interferometer>

              <instrument>
                <!-- instrument required -->
                <focalInstrument>VEGA_3T</focalInstrument> 
                <configuration>
                  <stations>W2 W1 E2</stations>
                  <channels>V1 V3 V6</channels>
                  <pops>PoP1 PoP2 PoP3</pops>
                </configuration>
              </instrument>

            </extendedConfiguration>
         */
        // Put all possibly used references ie xsd:idref(s) occurences in InterferometerConfiguration:
        // InterferometerConfiguration needed refs:
        // InterferometerDescription reference for the given id:
        addConfigurationReference(id.getName(), id);

        // SwitchYard references:
        for (SwitchYard sw : id.getSwitchyards()) {
            addConfigurationReference(sw.getName(), sw);
        }

        // FocalInstrumentConfiguration needed refs:
        // instrument references:
        for (FocalInstrument instrument : id.getFocalInstruments()) {
            // use name as main identifier:
            addConfigurationReference(instrument.getName(), instrument);
            // use altNames for compatibility:
            for (String altName : instrument.getAltNames()) {
                addConfigurationReference(altName, instrument);
            }
        }

        // FocalInstrumentConfigurationItem needed refs:
        // Station references:
        for (Station station : id.getStations()) {
            addConfigurationReference(station.getName(), station);
        }

        // Channel references:
        for (Channel channel : id.getChannels()) {
            addConfigurationReference(channel.getName(), channel);
        }

        // DelayLine references:
        for (DelayLine dl : id.getDelayLines()) {
            addConfigurationReference(dl.getName(), dl);
        }

        // Pop references:
        for (Pop pop : id.getPops()) {
            addConfigurationReference(pop.getName(), pop);
        }
        if (doLogDebug) {
            logger.debug("prepare: configurationReferences =\n{}", configurationReferences);
        }
    }

    private void addConfigurationReference(final String id, final Object reference) {
        Object prev = getConfigurationReference(id);
        if (prev != null) {
            logger.warn("addConfigurationReference('{}') conflict: new = {} previous = {}",
                    id, prev, reference);
        }
        // Anyway overwrite the reference:
        configurationReferences.put(id, reference);
    }

    private Object getConfigurationReference(final String id) {
        return configurationReferences.get(id);
    }
}
