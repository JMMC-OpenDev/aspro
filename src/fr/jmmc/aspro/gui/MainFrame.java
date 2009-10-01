/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: MainFrame.java,v 1.2 2009-10-01 16:06:25 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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

    final Dimension dim = new Dimension(1000, 750);
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

  private void createContent() {

    this.tabs = new JTabbedPane(JTabbedPane.TOP);

    getContentPane().add(tabs, BorderLayout.CENTER);
  }

  private void addDefaultTabs() {

    final SettingPanel setting = new SettingPanel();

    // adds the panel in scrollPane
    final JScrollPane settingScrollPane = new JScrollPane(setting);

    // The tab is added to the tabbed panel :
    final int pos = tabs.getTabCount();
    tabs.insertTab("new settings", null, settingScrollPane, null, pos);
    tabs.setSelectedIndex(pos);
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
