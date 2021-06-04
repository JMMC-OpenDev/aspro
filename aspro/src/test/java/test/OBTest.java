/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package test;

import fr.jmmc.aspro.model.OBManager;
import fr.jmmc.aspro.model.ob.InstrumentConfiguration;
import fr.jmmc.aspro.model.ob.InterferometerConfiguration;
import fr.jmmc.aspro.model.ob.OBItem;
import fr.jmmc.aspro.model.ob.ObservationConfiguration;
import fr.jmmc.aspro.model.ob.ObservationConstraints;
import fr.jmmc.aspro.model.ob.ObservationSchedule;
import fr.jmmc.aspro.model.ob.ObservationType;
import fr.jmmc.aspro.model.ob.ObservingBlockDefinition;
import fr.jmmc.aspro.model.ob.Target;
import java.io.StringWriter;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Basic OB Test
 * @author bourgesl
 */
public class OBTest {

    /** Class logger */
    protected static final Logger logger = LoggerFactory.getLogger(OBTest.class.getName());

    @Test
    public void testOB() {
        final ObservingBlockDefinition obd = new ObservingBlockDefinition();

        final InterferometerConfiguration intConf = new InterferometerConfiguration();
        intConf.setName("VLTI");
        intConf.setStations("UT1 UT2 UT3 UT4");
        obd.setInterferometerConfiguration(intConf);

        final InstrumentConfiguration insConf = new InstrumentConfiguration();
        insConf.setName("GRAVITY");
        insConf.setInstrumentMode("MEDIUM-SPLIT");
        obd.setInstrumentConfiguration(insConf);
        
        // SCI
        final ObservationConfiguration obSCI = new ObservationConfiguration();
        obSCI.setId("SCI_UY_Aur");
        obSCI.setType(ObservationType.SCIENCE);
        
        final Target targetSCI = new Target();
        targetSCI.setName("UY Aur");
        targetSCI.setRA("10:10 ...");
        targetSCI.setDEC("-57:10 ...");
        obSCI.setSCTarget(targetSCI);
        
        final ObservationConstraints obSCICons = new ObservationConstraints();
        obSCICons.getLSTintervals().add("14:33/18:22");
        obSCI.setObservationConstraints(obSCICons);
        
        obd.getObservationConfigurations().add(obSCI);

        // CAL
        final ObservationConfiguration obCAL = new ObservationConfiguration();
        obCAL.setId("CAL_UY_Aur");
        obCAL.setType(ObservationType.CALIBRATION);
        
        final Target targetCAL = new Target();
        targetCAL.setName("CALIB UY Aur");
        targetCAL.setRA("12:10 ...");
        targetCAL.setDEC("-58:10 ...");
        obCAL.setSCTarget(targetCAL);
        
        final ObservationConstraints obCALCons = new ObservationConstraints();
        obCALCons.getLSTintervals().add("14:20/19:03");
        obCAL.setObservationConstraints(obCALCons);
        
        obd.getObservationConfigurations().add(obCAL);

        final ObservationSchedule obsSch = new ObservationSchedule();
        final OBItem cal = new OBItem(obCAL);
        final OBItem sci = new OBItem(obSCI);
        obsSch.getOBS().add(cal);
        obsSch.getOBS().add(sci);
        obsSch.getOBS().add(cal);
        obd.setObservationSchedule(obsSch);

        final StringWriter sw = new StringWriter(4096);
        OBManager.getInstance().saveObject(sw, obd);

        logger.info("OB:\n{}", sw.toString());
    }
}
