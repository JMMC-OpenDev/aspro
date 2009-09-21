/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: MainFrame.java,v 1.1 2009-09-21 15:38:50 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
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
  /** instance link */
  private static MainFrame instance = null;

  /* Swing Components */
  /** Status Bar */
  protected StatusBar statusBar;

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

    // Handle status bar
    statusBar = new StatusBar();
    getContentPane().add(statusBar, BorderLayout.SOUTH);

    pack();
    WindowCenterer.centerOnMainScreen(this);

    StatusBar.show("Application started");
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
