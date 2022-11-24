
package fr.jmmc.aspro.model.ob;

import javax.xml.bind.annotation.XmlRegistry;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the fr.jmmc.aspro.model.ob package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {


    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: fr.jmmc.aspro.model.ob
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link ObservingBlockDefinition }
     * 
     */
    public ObservingBlockDefinition createObservingBlockDefinition() {
        return new ObservingBlockDefinition();
    }

    /**
     * Create an instance of {@link InterferometerConfiguration }
     * 
     */
    public InterferometerConfiguration createInterferometerConfiguration() {
        return new InterferometerConfiguration();
    }

    /**
     * Create an instance of {@link InstrumentConfiguration }
     * 
     */
    public InstrumentConfiguration createInstrumentConfiguration() {
        return new InstrumentConfiguration();
    }

    /**
     * Create an instance of {@link ObservationConfiguration }
     * 
     */
    public ObservationConfiguration createObservationConfiguration() {
        return new ObservationConfiguration();
    }

    /**
     * Create an instance of {@link ObservationSchedule }
     * 
     */
    public ObservationSchedule createObservationSchedule() {
        return new ObservationSchedule();
    }

    /**
     * Create an instance of {@link ObservationConstraints }
     * 
     */
    public ObservationConstraints createObservationConstraints() {
        return new ObservationConstraints();
    }

    /**
     * Create an instance of {@link OBItem }
     * 
     */
    public OBItem createOBItem() {
        return new OBItem();
    }

    /**
     * Create an instance of {@link Target }
     * 
     */
    public Target createTarget() {
        return new Target();
    }

    /**
     * Create an instance of {@link ExtraInformations }
     * 
     */
    public ExtraInformations createExtraInformations() {
        return new ExtraInformations();
    }

    /**
     * Create an instance of {@link BaseValue }
     * 
     */
    public BaseValue createBaseValue() {
        return new BaseValue();
    }

    /**
     * Create an instance of {@link BooleanValue }
     * 
     */
    public BooleanValue createBooleanValue() {
        return new BooleanValue();
    }

    /**
     * Create an instance of {@link NumberValue }
     * 
     */
    public NumberValue createNumberValue() {
        return new NumberValue();
    }

    /**
     * Create an instance of {@link StringValue }
     * 
     */
    public StringValue createStringValue() {
        return new StringValue();
    }

}
