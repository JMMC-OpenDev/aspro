/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: ConfigurationManager.java,v 1.1 2009-09-21 15:38:51 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 *
 ******************************************************************************/
package fr.jmmc.aspro.config;

import fr.jmmc.jaxb.JAXBFactory;
import java.util.logging.Level;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

/**
 * This class manages configuration files for the Interferometer configurations
 * @author bourgesl
 */
public class ConfigurationManager {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.config.ConfigurationManager";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /** package name for JAXB generated code */
  private final static String OI_JAXB_PATH = "fr.jmmc.aspro.model.oi";

  /** singleton pattern */
  private static ConfigurationManager instance = new ConfigurationManager();

  // members :
  private JAXBFactory jf;

  /**
   * Return the ConfigurationManager singleton
   * @return ConfigurationManager singleton
   */
  private static ConfigurationManager getInstance() {
    return instance;
  }

  /**
   * Private constructor
   */
  private ConfigurationManager() {
    this.jf = JAXBFactory.getInstance(OI_JAXB_PATH);
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("ConfigurationManager : " + this.jf);
    }
  }

  public Object load(final String uri) {
    final Unmarshaller u = this.jf.createUnMarshaller();

    Object result = null;
    try {
      
      result = u.unmarshal(this.getClass().getResource(uri));
    } catch (JAXBException je) {
    }
    return result;
  }



  /**
   * Test code
   */
  public static void main(final String[] args) {
    final Object loaded = ConfigurationManager.getInstance().load("../model/VLTI.xml");
    logger.log(Level.SEVERE, "ConfigurationManager : " + loaded);



  }
}
