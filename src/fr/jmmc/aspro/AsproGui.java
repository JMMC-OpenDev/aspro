/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: AsproGui.java,v 1.4 2009-12-04 15:37:32 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.3  2009/11/03 16:57:56  bourgesl
 * added observability plot with LST/UTC support containing only day/night/twilight zones
 *
 * Revision 1.2  2009/10/02 15:20:18  bourgesl
 * updated model + stupid tree model
 *
 * Revision 1.1  2009/09/21 15:38:50  bourgesl
 * initial jmcs gui + jaxb loader
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro;

import fr.jmmc.aspro.gui.SettingPanel;
import fr.jmmc.aspro.gui.action.SaveObservationAction;
import fr.jmmc.aspro.gui.util.ComponentResizeAdapter;
import fr.jmmc.aspro.model.ConfigurationManager;
import fr.jmmc.mcs.gui.App;
import fr.jmmc.mcs.gui.StatusBar;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Locale;
import java.util.TimeZone;
import javax.swing.JFrame;
import javax.swing.JScrollPane;

/**
 * This class represents the Aspro GUI application
 * @author bourgesl
 */
public class AsproGui extends App {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.AsproGui";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* members */
  /* Swing Components */
  /** observation setting panel */
  protected SettingPanel settingPanel;
  /** Status Bar */
  protected StatusBar statusBar;

  /**
   * Public constructor with command line arguments
   * @param args command line arguments
   */
  public AsproGui(final String[] args) {
    // no splash screen during the developments :
    super(args, false, !AsproConstants.DEBUG_MODE);
  }

  /** Initialize application objects */
  @Override
  protected void init(final String[] args) {
    logger.fine("init : enter");

    // Set the default locale to en-US locale (for Numerical Fields "." ",")
    Locale.setDefault(new Locale("en", "US"));
    // Set the default timezone to GMT to handle properly the date in UTC :
    TimeZone.setDefault(TimeZone.getTimeZone("GMT"));

    // Preload configurations :
    ConfigurationManager.getInstance();

    // Initializes the swing components with their actions :
    prepareFrame((JFrame) getFrame());

    logger.fine("init : exit");
  }

  /** Execute application body */
  @Override
  protected void execute() {
    getFrame().setVisible(true);
  }

  private void prepareFrame(final JFrame frame) {
    frame.setTitle(App.getSharedApplicationDataModel().getProgramName() + " v" + App.getSharedApplicationDataModel().getProgramVersion());

    final Dimension dim = new Dimension(800, 600);
    frame.setMinimumSize(dim);
    frame.addComponentListener(new ComponentResizeAdapter(dim));

    // handle closing by mouse :
    frame.addWindowListener(new CloseFrameAdapter());

    // previous adapter manages the windowClosing(event) :
    frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

    // init the main panel :
    createContent();

    // initialize the actions :
    registerActions();

    // Handle status bar
    statusBar = new StatusBar();
    getFramePanel().add(statusBar, BorderLayout.SOUTH);

    StatusBar.show("application started.");
  }

  /**
   * Create the main content i.e. the observation setting panel
   */
  private void createContent() {

    this.settingPanel = new SettingPanel();

    // adds the panel in scrollPane
    final JScrollPane settingScrollPanel = new JScrollPane(this.settingPanel);

    getFramePanel().add(settingScrollPanel, BorderLayout.CENTER);
  }

  /**
   * Create the main actions present in the menu bar
   */
  private void registerActions() {
    // save observation :
    new SaveObservationAction();
  }

  /**
   * Window adapter to handle windowClosing event.
   */
  private static final class CloseFrameAdapter extends WindowAdapter {

    @Override
    public void windowClosing(final WindowEvent e) {
      // callback on exit :
      App.quitAction().actionPerformed(null);
    }
  }

  /**
   * Main entry point
   * @param args command line arguments
   */
  public static void main(final String[] args) {
    // Start application with the command line arguments
    new AsproGui(args);
  }
}
