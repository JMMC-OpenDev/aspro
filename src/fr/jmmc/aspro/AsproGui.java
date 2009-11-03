/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AsproGui.java,v 1.3 2009-11-03 16:57:56 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.2  2009/10/02 15:20:18  bourgesl
 * updated model + stupid tree model
 *
 * Revision 1.1  2009/09/21 15:38:50  bourgesl
 * initial jmcs gui + jaxb loader
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro;

import fr.jmmc.aspro.gui.MainFrame;
import fr.jmmc.mcs.gui.App;
import java.awt.EventQueue;
import java.util.Locale;
import java.util.TimeZone;

/**
 * This class represents the Aspro GUI application
 * @author bourgesl
 */
public class AsproGui extends App {

  /**
   * Public constructor with command line arguments
   * @param args command line arguments
   */
  public AsproGui(final String[] args) {
/*    super(args); */
    // no splash screen during the developments :
    super(args, false, false);
  }

  /** Initialize application objects */
  @Override
  protected void init(String[] args) {
    // Set the default locale to en-US locale (for Numerical Fields "." ",")
    Locale.setDefault(new Locale("en", "US"));
    // Set the default timezone to GMT to handle properly the date in UTC :
    TimeZone.setDefault(TimeZone.getTimeZone("GMT"));
  }

  /** Execute application body */
  @Override
  protected void execute() {
    // EDT :
    EventQueue.invokeLater(
      new Runnable() {
        public void run() {
          // Initializes the swing components :
          final MainFrame main = new MainFrame();

          main.setVisible(true);
        }
      }
    );
  }

  /**
   * Main entry point
   */
  public static void main(final String[] args) {
    // Start application without any argument
    new AsproGui(new String[] {});
  }
}
