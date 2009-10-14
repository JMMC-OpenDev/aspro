/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: MainFrame.java,v 1.5 2009-10-14 15:54:38 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 * Revision 1.4  2009/10/13 16:02:37  bourgesl
 * simple uv plot demo
 *
 * Revision 1.3  2009/10/02 15:20:18  bourgesl
 * updated model + stupid tree model
 *
 * Revision 1.2  2009/10/01 16:06:25  bourgesl
 * demo UI
 *
 * Revision 1.1  2009/09/21 15:38:50  bourgesl
 * initial jmcs gui + jaxb loader
 *
 *
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.gui.util.ComponentResizeAdapter;
import fr.jmmc.mcs.gui.App;
import fr.jmmc.mcs.gui.MainMenuBar;
import fr.jmmc.mcs.gui.StatusBar;
import fr.jmmc.mcs.gui.WindowCenterer;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;

/**
 * This class represents the main window frame of the ASPRO application
 * @author bourgesl
 */
public class MainFrame extends JFrame {

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.MainFrame";
  /** Class logger */
  private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);

  /* members */
  /** instance link */
  private static MainFrame instance = null;

  /* Swing Components */
  protected JTabbedPane tabs;
  /** Status Bar */
  protected StatusBar statusBar;

  /**
   * Return this singleton instance
   * @return singleton instance
   */
  public static MainFrame getInstance() {
    return instance;
  }

  /**
   * Creates the main frame
   */
  public MainFrame() {
    super(App.getSharedApplicationDataModel().getProgramName() + " v" + App.getSharedApplicationDataModel().getProgramVersion());

    instance = this;

    initComponents();
  }

  private void initComponents() {

    final Dimension dim = new Dimension(800, 600);
    setMinimumSize(dim);
    addComponentListener(new ComponentResizeAdapter(dim));

    // handle closing by mouse :
    addWindowListener(new CloseFrameAdapter());

    // previous adapter manages the windowClosing(event) :
    setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

    // Handle menu bar
    setJMenuBar(new MainMenuBar(this));

    // init the main panel :
    createContent();

    // Handle status bar
    statusBar = new StatusBar();
    getContentPane().add(statusBar, BorderLayout.SOUTH);

    // add default content :
    addDefaultTabs();

    pack();
    WindowCenterer.centerOnMainScreen(this);

    StatusBar.show("Application started");
  }

  /**
   * Create the main content :
   */
  private void createContent() {

    this.tabs = new JTabbedPane(JTabbedPane.TOP);

    getContentPane().add(tabs, BorderLayout.CENTER);
  }

  /**
   * Create the default tab (dev)
   */
  private void addDefaultTabs() {

    final SettingPanel setting = new SettingPanel();

    // adds the panel in scrollPane
    final JScrollPane settingScrollPane = new JScrollPane(setting);

    // The tab is added to the tabbed panel :
    int pos = tabs.getTabCount();
    tabs.insertTab("new settings", null, settingScrollPane, null, pos);
    tabs.setSelectedIndex(pos);

//    tabs.insertTab("plot", null, new UVChartPanel(), null, pos);
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
}
