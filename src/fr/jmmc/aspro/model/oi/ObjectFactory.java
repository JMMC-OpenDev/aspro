
package fr.jmmc.aspro.model.oi;

import java.util.List;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlIDREF;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the fr.jmmc.aspro.model.oi package. 
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

    private final static QName _TargetInformationCalibrators_QNAME = new QName("", "calibrators");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: fr.jmmc.aspro.model.oi
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link InterferometerDescription }
     * 
     */
    public InterferometerDescription createInterferometerDescription() {
        return new InterferometerDescription();
    }

    /**
     * Create an instance of {@link Telescope }
     * 
     */
    public Telescope createTelescope() {
        return new Telescope();
    }

    /**
     * Create an instance of {@link DelayLine }
     * 
     */
    public DelayLine createDelayLine() {
        return new DelayLine();
    }

    /**
     * Create an instance of {@link Configurations }
     * 
     */
    public Configurations createConfigurations() {
        return new Configurations();
    }

    /**
     * Create an instance of {@link TargetInformation }
     * 
     */
    public TargetInformation createTargetInformation() {
        return new TargetInformation();
    }

    /**
     * Create an instance of {@link TargetUserInformations }
     * 
     */
    public TargetUserInformations createTargetUserInformations() {
        return new TargetUserInformations();
    }

    /**
     * Create an instance of {@link AdaptiveOptics }
     * 
     */
    public AdaptiveOptics createAdaptiveOptics() {
        return new AdaptiveOptics();
    }

    /**
     * Create an instance of {@link StationLinks }
     * 
     */
    public StationLinks createStationLinks() {
        return new StationLinks();
    }

    /**
     * Create an instance of {@link AzEl }
     * 
     */
    public AzEl createAzEl() {
        return new AzEl();
    }

    /**
     * Create an instance of {@link FocalInstrumentConfigurationItem }
     * 
     */
    public FocalInstrumentConfigurationItem createFocalInstrumentConfigurationItem() {
        return new FocalInstrumentConfigurationItem();
    }

    /**
     * Create an instance of {@link TargetConfiguration }
     * 
     */
    public TargetConfiguration createTargetConfiguration() {
        return new TargetConfiguration();
    }

    /**
     * Create an instance of {@link Channel }
     * 
     */
    public Channel createChannel() {
        return new Channel();
    }

    /**
     * Create an instance of {@link SwitchYard }
     * 
     */
    public SwitchYard createSwitchYard() {
        return new SwitchYard();
    }

    /**
     * Create an instance of {@link HorizonProfile }
     * 
     */
    public HorizonProfile createHorizonProfile() {
        return new HorizonProfile();
    }

    /**
     * Create an instance of {@link FocalInstrument }
     * 
     */
    public FocalInstrument createFocalInstrument() {
        return new FocalInstrument();
    }

    /**
     * Create an instance of {@link LonLatAlt }
     * 
     */
    public LonLatAlt createLonLatAlt() {
        return new LonLatAlt();
    }

    /**
     * Create an instance of {@link ChannelLink }
     * 
     */
    public ChannelLink createChannelLink() {
        return new ChannelLink();
    }

    /**
     * Create an instance of {@link InterferometerConfigurationChoice }
     * 
     */
    public InterferometerConfigurationChoice createInterferometerConfigurationChoice() {
        return new InterferometerConfigurationChoice();
    }

    /**
     * Create an instance of {@link Station }
     * 
     */
    public Station createStation() {
        return new Station();
    }

    /**
     * Create an instance of {@link PopLink }
     * 
     */
    public PopLink createPopLink() {
        return new PopLink();
    }

    /**
     * Create an instance of {@link FocalInstrumentMode }
     * 
     */
    public FocalInstrumentMode createFocalInstrumentMode() {
        return new FocalInstrumentMode();
    }

    /**
     * Create an instance of {@link Position3D }
     * 
     */
    public Position3D createPosition3D() {
        return new Position3D();
    }

    /**
     * Create an instance of {@link FringeTracker }
     * 
     */
    public FringeTracker createFringeTracker() {
        return new FringeTracker();
    }

    /**
     * Create an instance of {@link FocalInstrumentConfiguration }
     * 
     */
    public FocalInstrumentConfiguration createFocalInstrumentConfiguration() {
        return new FocalInstrumentConfiguration();
    }

    /**
     * Create an instance of {@link InterferometerSetting }
     * 
     */
    public InterferometerSetting createInterferometerSetting() {
        return new InterferometerSetting();
    }

    /**
     * Create an instance of {@link WhenSetting }
     * 
     */
    public WhenSetting createWhenSetting() {
        return new WhenSetting();
    }

    /**
     * Create an instance of {@link Target }
     * 
     */
    public Target createTarget() {
        return new Target();
    }

    /**
     * Create an instance of {@link Pop }
     * 
     */
    public Pop createPop() {
        return new Pop();
    }

    /**
     * Create an instance of {@link InterferometerConfiguration }
     * 
     */
    public InterferometerConfiguration createInterferometerConfiguration() {
        return new InterferometerConfiguration();
    }

    /**
     * Create an instance of {@link FocalInstrumentConfigurationChoice }
     * 
     */
    public FocalInstrumentConfigurationChoice createFocalInstrumentConfigurationChoice() {
        return new FocalInstrumentConfigurationChoice();
    }

    /**
     * Create an instance of {@link ObservationSetting }
     * 
     */
    public ObservationSetting createObservationSetting() {
        return new ObservationSetting();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link List }{@code <}{@link Object }{@code >}{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "calibrators", scope = TargetInformation.class)
    @XmlIDREF
    public JAXBElement<List<Object>> createTargetInformationCalibrators(List<Object> value) {
        return new JAXBElement<List<Object>>(_TargetInformationCalibrators_QNAME, ((Class) List.class), TargetInformation.class, ((List<Object> ) value));
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link List }{@code <}{@link Object }{@code >}{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "", name = "calibrators", scope = TargetUserInformations.class)
    @XmlIDREF
    public JAXBElement<List<Object>> createTargetUserInformationsCalibrators(List<Object> value) {
        return new JAXBElement<List<Object>>(_TargetInformationCalibrators_QNAME, ((Class) List.class), TargetUserInformations.class, ((List<Object> ) value));
    }

}
