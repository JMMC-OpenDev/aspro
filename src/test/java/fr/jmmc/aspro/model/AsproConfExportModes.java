/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import static fest.common.FestSwingCustomJUnitTestCase.getProjectFolderPath;
import fr.jmmc.aspro.model.oi.FocalInstrument;
import fr.jmmc.aspro.model.oi.FocalInstrumentMode;
import fr.jmmc.aspro.model.oi.InterferometerDescription;
import fr.jmmc.aspro.model.oi.LonLatAlt;
import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.oitools.util.FileUtils;
import java.io.File;
import java.io.IOException;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 *
 * @author bourgesl
 */
public class AsproConfExportModes {

    /** absolute path to test folder to load test resources */
    protected final static String TEST_FOLDER = getProjectFolderPath() + "src/test/resources/";

    /**
     * Export Aspro2 configuration into xml format (obsportal)
     * @param args unused
     */
    public static void main(final String[] args) throws IOException {

        // invoke Bootstrapper method to initialize logback now:
        Bootstrapper.getState();

        export("VLTI");
        export("CHARA");
    }

    /**
     * Export the given interferometer into xml format
     * @param interferometerName
     */
    public static void export(String interferometerName) throws IOException {

        final ConfigurationManager cm = ConfigurationManager.getInstance();

        final InterferometerDescription id = cm.getInterferometerDescription(interferometerName);

        if (id == null) {
            System.err.println("Missing InterferometerDescription = " + interferometerName);
            return;
        }

        final Set<String> insNames = new LinkedHashSet<String>();

        final StringBuilder sb = new StringBuilder(16 * 1024);
        sb.append("<configuration>\n");

        sb.append("    <interferometer>\n");
        sb.append("        <name>").append(id.getName()).append("</name>\n");

        final LonLatAlt position = id.getPosSph();
        sb.append("        <lat>").append(Math.toDegrees(position.getLatitude())).append("</lat>\n");
        sb.append("        <lon>").append(Math.toDegrees(position.getLongitude())).append("</lon>\n");
        sb.append("        <alt>").append(position.getAltitude()).append("</alt>\n");

        for (FocalInstrument ins : id.getFocalInstruments()) {
            String insName = ins.getName();

            int pos = insName.lastIndexOf('_');
            if (pos != -1) {
                final String suffix = insName.substring(pos + 1, insName.length());

                if ((suffix.length() == 2) && Character.isDigit(suffix.charAt(0)) && (suffix.charAt(1) == 'T')) {
                    // ins name ends with "_?T" = instrument variant:
                    insName = insName.substring(0, pos);
                }
            }

            if (insNames.contains(insName)) {
                System.out.println("Skipping instrument : " + ins.getName() + " (already processed " + insName + ") !");
                continue;
            } else {
                insNames.add(insName);
            }

            System.out.println("Processing instrument : " + insName + " ...");

            sb.append("        <instrument>\n");
            sb.append("            <name>").append(insName).append("</name>\n");

            for (FocalInstrumentMode mode : ins.getModes()) {
                sb.append("            <mode>\n");
                sb.append("                <name>").append(mode.getName()).append("</name>\n");
                sb.append("                <wave_min>").append(NumberUtils.trimTo5Digits(mode.getWaveLengthMin())).append("</wave_min>\n");
                sb.append("                <wave_max>").append(NumberUtils.trimTo5Digits(mode.getWaveLengthMax())).append("</wave_max>\n");
                sb.append("                <spec_res>").append((int) Math.ceil(mode.getResolution())).append("</spec_res>\n");
                sb.append("            </mode>\n");
            }
            sb.append("        </instrument>\n");
        }
        sb.append("    </interferometer>\n");

        sb.append("</configuration>\n");

        System.out.println("XML:\n" + sb.toString());

        FileUtils.writeFile(new File(TEST_FOLDER + "aspro-conf-" + interferometerName + ".xml"), sb.toString());
    }
}
