package edu.dartmouth;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.Stroke;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.awt.print.PrinterException;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Locale;
import javax.swing.AbstractButton;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JWindow;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.TableColumn;
import javax.swing.text.html.HTMLEditorKit;
import javax.xml.datatype.XMLGregorianCalendar;

/* JSkyCalc.java -- copyright 2007, John Thorstensen, Dartmouth College. */
/** TERMS OF USE -- Anyone is free to use this software for any purpose, and to
 modify it for their own purposes, provided that credit is given to the author
 in a prominent place.  For the present program that means that the green
 title and author banner appearing on the main window must not be removed,
 and may not be altered without premission of the author. */

/* JSkyCalc is used by ASPRO 2 only for sky display ... */
public final class JSkyCalc {

  public static void main(String[] args) {
    //   Locale locale = Locale.getDefault();  ... fixes the problems with German locale,
    //   Locale.setDefault(Locale.ENGLISH);    ... but, breaks the webstart!!

    // Use EDT to create JSkyCalc window:
    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        try {
          // Set cross-platform Java L&F (also called "Metal")
          UIManager.setLookAndFeel(
                  UIManager.getCrossPlatformLookAndFeelClassName());
        } catch (UnsupportedLookAndFeelException e) {
          System.out.printf("UnsupportedLookAndFeelException ... \n");
        } catch (ClassNotFoundException e) {
          System.out.printf("Class not found while trying to set look-and-feel ...\n");
        } catch (InstantiationException e) {
          System.out.printf("Instantiation exception while trying to set look-and-feel ...\n");
        } catch (IllegalAccessException e) {
          System.out.printf("Illegal access exception while trying to set look-and-feel ...\n");
        }

        new JSkyCalcWindow();
      }
    });
  }
  // LBO: JSkyCalc API integration
  /** allow Quit action */
  public static boolean ALLOW_QUIT = true;
  /** allow Site Window */
  public static boolean ALLOW_SITE_WINDOW = true;
  /** shared JSkyCalc window instance */
  private static JSkyCalcWindow jSkyCalcWin = null;

  /**
   * Show the JSkyCalc window for the given site and target (given its RA/DEC) at the given calendar date
   * @param site site to use
   * @param name target name(s) as string
   * @param ra target RA(s) as string
   * @param dec target DEC(s) as string
   * @param selected selected target
   * @param cal calendar date
   */
  public static void showJSkyCalc(final Site site, final String[] name, final String[] ra, final String[] dec,
          final String selected, final XMLGregorianCalendar cal) {

    if (jSkyCalcWin == null) {
      ALLOW_QUIT = false;
      ALLOW_SITE_WINDOW = false;
      jSkyCalcWin = new JSkyCalcWindow();
    }

    jSkyCalcWin.obsnamefield.setText(site.name);
    jSkyCalcWin.latitudefield.setText(site.lat.roundedDecString(0, " "));
    jSkyCalcWin.longitudefield.setText(site.longit.roundedLongitString(1, " ", false));

    jSkyCalcWin.stdzfield.setText(Double.toString(site.stdz));
    jSkyCalcWin.use_dstfield.setText(Integer.toString(site.use_dst));

    // ignore zonenamefield
    jSkyCalcWin.elevseafield.setText(Double.toString(site.elevsea));
    jSkyCalcWin.elevhorizfield.setText(Double.toString(site.elevhoriz));

    // choose user-defined site:
    final Enumeration<AbstractButton> siteButtons = jSkyCalcWin.SiteButtons.getElements();
    for (; siteButtons.hasMoreElements();) {
      AbstractButton b = siteButtons.nextElement();
      if ("x".equals(b.getActionCommand())) {
        b.doClick();
      }
    }
    jSkyCalcWin.synchSite();

    // update complete list of targets:
    final BufferedReader reader = new BufferedReader(new StringReader(prepareAstroObjs(name, ra, dec)));

    jSkyCalcWin.freeAstroObjs();
    jSkyCalcWin.LoadAstrObjs(reader);
    try {
      reader.close();
    } catch (IOException ioe) {
      System.out.println(ioe);
    }

    // define selected target:
    jSkyCalcWin.objnamefield.setText(selected.replace(' ', '_').trim());
    jSkyCalcWin.objnamefield.getActionListeners()[0].actionPerformed(null);
    jSkyCalcWin.synchOutput();

    // date:
    // choose Local date:
    final Enumeration<AbstractButton> dateButtons = jSkyCalcWin.UTbuttons.getElements();
    for (; dateButtons.hasMoreElements();) {
      AbstractButton b = dateButtons.nextElement();
      if ("Local".equals(b.getActionCommand())) {
        b.doClick();
      }
    }

    jSkyCalcWin.datefield.setText(cal.getYear() + " " + cal.getMonth() + " " + cal.getDay());
    jSkyCalcWin.timefield.setText("20:00");
    jSkyCalcWin.setToDate();

    // show anyway to force focus:
    jSkyCalcWin.frame.setVisible(true);
  }

  private static String prepareAstroObjs(final String[] name, final String[] ra, final String[] dec) {
    final int len = name.length;
    if (len != ra.length || len != dec.length) {
      return "";
    }
    final StringBuilder sb = new StringBuilder(64 * len);

    for (int i = 0; i < len; i++) {
      // name_no_blanks   hh mm ss   dd mm ss   equinox
      sb.append(name[i].replace(' ', '_').trim()).append(' ');
      sb.append(ra[i].replace(':', ' ').trim()).append(' ');
      sb.append(dec[i].replace(':', ' ').trim()).append(' ');
      sb.append("2000\n");
    }

    return sb.toString();
  }
}

/** This ENORMOUS class includes the entire user interface -- subwindows are
 implemented as subclasses. */
class JSkyCalcWindow extends JComponent {

  final JFrame frame;
  /*
   final UIManager.LookAndFeelInfo [] landfs  = UIManager.getInstalledLookAndFeels();
   final String className;
   className = landfs[2].getClassName();
   try  {
   UIManager.setLookAndFeel(className);
   } catch catch (Exception e) { System.out.println(e); }
   */

  /* I also tried to add a MouseListener to this, but it was ignored because the
   SkyDisplay subclass pre-empts it.  Mouse events are always dispatched to the
   "deepest" component in the hierarchy.  */
  WhenWhere w;
  InstantInTime i;
  Site s;
  Observation o; // oooh!
  Planets p;
  PlanetWindow PlWin;
  boolean planetframevisible = false;
  SiteWindow siteframe;
  boolean siteframevisible = false;
  HourlyWindow HrWin;
  boolean hourlyframevisible = false;
  NightlyWindow NgWin;
  NightlyAlmanac Nightly;
  boolean nightlyframevisible = false;
  Seasonal season;
  SeasonalWindow SeasonWin;
  boolean seasonframevisible = false;
  SkyDisplay SkyDisp;
  boolean skydisplayvisible = false;
  AltCoordWin AltWin;
  boolean altcoowinvisible = false;
  HelpWindow HelpWin;
  boolean helpwindowvisible = false;
  AirmassDisplay AirDisp;
  boolean airmasswindowvisible = false;
  long sleepinterval = 2000l;
  boolean autoupdaterunning = false;
  AutoUpdate au;
  boolean autosteprunning = false;
  AutoStep as;
  Thread thr;
  static AstrObj obj;
  AstrObjSelector ObjSelWin;
  boolean objselwinvisible = false;
  static String st = null;
  static HashMap<String, AstrObj> byname = new LinkedHashMap<String, AstrObj>();
  static HashMap<Double, AstrObj> byra = new HashMap<Double, AstrObj>();
  static Double rakey;
  static String[] RASelectors;  // real names, sorted by ra
  static String[] NameSelectors;  // real names, sorted by DEC.
  static HashMap<String, AstrObj> presenterKey = new HashMap<String, AstrObj>();
  static HashMap<String, Site> siteDict = new HashMap<String, Site>();
  AstrObjSelector AirmSelWin;
  static Object[] airmassPlotSelections = {null};
  // littleHints hints;
  JTextField datefield;
  JTextField timefield;
  JTextField JDfield;
  JTextField objnamefield;
  JTextField RAfield;
  JTextField decfield;
  JTextField equinoxfield;
  JTextField timestepfield;
  JTextField sleepfield;
  JTextField obsnamefield;
  JTextField longitudefield;
  JTextField latitudefield;
  JTextField stdzfield;
  JTextField use_dstfield;
  JTextField zonenamefield;
  JTextField elevseafield;
  JTextField elevhorizfield;
  JTextField siderealfield;
  JTextField HAfield;
  JTextField airmassfield;
  JTextField altazfield;
  JTextField parallacticfield;
  JTextField sunradecfield;
  JTextField sunaltazfield;
  JTextField ztwilightfield;
  JTextField moonphasefield;
  JTextField moonradecfield;
  JTextField moonaltazfield;
  JTextField illumfracfield;
  JTextField lunskyfield;
  JTextField moonobjangfield;
  JTextField baryjdfield;
  JTextField baryvcorfield;
  JTextField constelfield;
  JTextField planetproximfield;
  ButtonGroup UTbuttons;
  JRadioButton UTradiobutton;  // save references for swapper (later)
  JRadioButton Localradiobutton;
  ButtonGroup SiteButtons;
  JButton autoupdatebutton;
  JButton autostepbutton;
  Color outputcolor = new Color(230, 230, 230);  // light grey
  Color inputcolor = Color.WHITE;  // for input fields
  Color lightblue = new Color(150, 220, 255);
  Color lightpurple = new Color(221, 136, 255);
  Color sitecolor = new Color(255, 215, 215); // a little pink
  Color panelcolor = new Color(185, 190, 190); // a little darker
  Color brightyellow = new Color(255, 255, 0);  // brighteryellow
  Color runningcolor = new Color(154, 241, 162);  // pale green, for autostep highlighting
  Color objboxcolor = new Color(0, 228, 152);  // for object box on display

  JSkyCalcWindow() {

    frame = new JFrame("JSkyCalc") {
      /**
       * Hide all opened window if the JSkyCalcWindow is hidden
       * @param visible true to show the JSkyCalcWindow frame; false to hide it
       */
      @Override
      public void setVisible(final boolean visible) {
        if (!visible) {
          // hide all:
          PlWin.setVisible(false);
          siteframe.setVisible(false);
          HrWin.setVisible(false);
          NgWin.setVisible(false);
          SeasonWin.setVisible(false);
          SkyDisp.setVisible(false);
          AltWin.setVisible(false);
          HelpWin.setVisible(false);
          AirDisp.setVisible(false);
          ObjSelWin.setVisible(false);
          AirmSelWin.setVisible(false);

          // Stop automatic updates:
          if (autoupdaterunning) {
            autoupdatebutton.doClick();
          }
          if (autosteprunning) {
            autostepbutton.doClick();
          }

        } else {
          // show Sky display:
          SkyDisp.setVisible(true);
        }

        super.setVisible(visible);
      }
    };

    /* Put up site panel and grab the first site ... */

    siteframe = new SiteWindow();
    if (JSkyCalc.ALLOW_SITE_WINDOW) {
      siteframe.setVisible(true);
    }

//      String [] initialsite = {"Kitt Peak [MDM Obs.]",  "7.44111",  "31.9533",
//           "7.",  "0",  "Mountain",  "M",  "1925",  "700."};

    s = siteframe.firstSite();
    i = new InstantInTime(s.stdz, s.use_dst);
    w = new WhenWhere(i, s);
    o = new Observation(w, w.zenith2000());
    o.computeSky();
    o.w.makeLocalSun();
    o.w.makeLocalMoon();
    o.computeSunMoon();
    p = new Planets(w);
    o.computeBary(p);

    int iy = 0;

    /* make up the separate subpanels */

    /* ****************** DO NOT REMOVE THIS BANNER *********************************
     ******************************************************************************

     I am distributing this COPYRIGHTED code freely with the condition that this
     credit banner will not be removed.  */
    JPanel bannerpanel = new JPanel();
    JLabel bannerlabel = new JLabel("JSkyCalc v1.1.1: John Thorstensen, Dartmouth College");
    Color dartmouthgreen = new Color(0, 105, 62);  // Official Dartmouth Green
    bannerpanel.setBackground(dartmouthgreen);
    bannerlabel.setBackground(dartmouthgreen);
    bannerlabel.setForeground(Color.WHITE);
    bannerpanel.add(bannerlabel);

    /* ******************************************************************************
     ****************************************************************************** */

    /* Panel for text I/O fields and their labels ... */

    JPanel textpanel = new JPanel();
    textpanel.setLayout(new GridBagLayout());
    GridBagConstraints constraints = new GridBagConstraints();

    objnamefield = new JTextField(13);
    objnamefield.setToolTipText("If object list is loaded, you can select by name.");
    JLabel objnamelabel = new JLabel("Object: ", SwingConstants.RIGHT);

    RAfield = new JTextField(13);
    RAfield.setToolTipText("White fields accept input; Enter key synchs output.");
    JLabel RAlabel = new JLabel("RA: ", SwingConstants.RIGHT);

    decfield = new JTextField(13);
    JLabel declabel = new JLabel("dec: ", SwingConstants.RIGHT);

    equinoxfield = new JTextField(13);
    JLabel equinoxlabel = new JLabel("equinox: ", SwingConstants.RIGHT);

    datefield = new JTextField(13);
    JLabel datelabel = new JLabel("Date: ", SwingConstants.RIGHT);

    timefield = new JTextField(13);
    JLabel timelabel = new JLabel("Time: ", SwingConstants.RIGHT);

    JDfield = new JTextField(13);
    JDfield.setToolTipText("Hitting Enter in JD field forces time to JD.");
    JLabel JDlabel = new JLabel("JD: ", SwingConstants.RIGHT);

    timestepfield = new JTextField(13);
    timestepfield.setText("1 h");
    timestepfield.setToolTipText("Units can be h, m, s, d, t (sid. day), l (lunation)");
    JLabel timesteplabel = new JLabel("timestep: ", SwingConstants.RIGHT);

    sleepfield = new JTextField(13);
    sleepfield.setText("3");
    sleepfield.setToolTipText("Used in Auto Update and Auto Step");
    JLabel sleeplabel = new JLabel("sleep for (s): ", SwingConstants.RIGHT);

    obsnamefield = new JTextField(13);
    obsnamefield.setBackground(sitecolor);
    obsnamefield.setToolTipText("You must select 'Allow User Input' for sites not on menu");
    JLabel obsnamelabel = new JLabel("Site name: ", SwingConstants.RIGHT);

    longitudefield = new JTextField(13);
    longitudefield.setBackground(sitecolor);
    obsnamefield.setToolTipText("If these fields are pink, they're not accepting input.");
    JLabel longitudelabel = new JLabel("Longitude: ", SwingConstants.RIGHT);

    latitudefield = new JTextField(13);
    latitudefield.setBackground(sitecolor);
    JLabel latitudelabel = new JLabel("Latitude: ", SwingConstants.RIGHT);

    stdzfield = new JTextField(13);
    stdzfield.setBackground(sitecolor);
    JLabel stdzlabel = new JLabel("Time zone: ", SwingConstants.RIGHT);

    use_dstfield = new JTextField(13);
    use_dstfield.setBackground(sitecolor);
    JLabel use_dstlabel = new JLabel("DST code: ", SwingConstants.RIGHT);

    zonenamefield = new JTextField(13);
    zonenamefield.setBackground(sitecolor);
    JLabel zonenamelabel = new JLabel("Zone name: ", SwingConstants.RIGHT);

    elevseafield = new JTextField(13);
    elevseafield.setBackground(sitecolor);
    JLabel elevsealabel = new JLabel("Elevation: ", SwingConstants.RIGHT);

    elevhorizfield = new JTextField(13);
    elevhorizfield.setBackground(sitecolor);
    JLabel elevhorizlabel = new JLabel("Terrain elev: ", SwingConstants.RIGHT);

    siderealfield = new JTextField(16);
    siderealfield.setBackground(outputcolor);
    JLabel sidereallabel = new JLabel(" Sidereal ", SwingConstants.RIGHT);

    HAfield = new JTextField(16);
    HAfield.setBackground(outputcolor);
    JLabel HAlabel = new JLabel(" HA ", SwingConstants.RIGHT);

    airmassfield = new JTextField(16);
    airmassfield.setBackground(outputcolor);
    JLabel airmasslabel = new JLabel(" Airmass ", SwingConstants.RIGHT);

    altazfield = new JTextField(16);
    altazfield.setBackground(outputcolor);
    JLabel altazlabel = new JLabel(" AltAz ", SwingConstants.RIGHT);

    parallacticfield = new JTextField(16);
    parallacticfield.setBackground(outputcolor);
    JLabel parallacticlabel = new JLabel(" parallactic ", SwingConstants.RIGHT);

    sunradecfield = new JTextField(16);
    sunradecfield.setBackground(outputcolor);
    JLabel sunradeclabel = new JLabel(" SunRAdec ", SwingConstants.RIGHT);

    sunaltazfield = new JTextField(16);
    sunaltazfield.setBackground(outputcolor);
    JLabel sunaltazlabel = new JLabel(" SunAltAz ", SwingConstants.RIGHT);

    ztwilightfield = new JTextField(16);
    ztwilightfield.setBackground(outputcolor);
    JLabel ztwilightlabel = new JLabel(" ZTwilight", SwingConstants.RIGHT);

    moonphasefield = new JTextField(16);
    moonphasefield.setBackground(outputcolor);
    JLabel moonphaselabel = new JLabel(" MoonPhase ", SwingConstants.RIGHT);

    moonradecfield = new JTextField(16);
    moonradecfield.setBackground(outputcolor);
    JLabel moonradeclabel = new JLabel(" MoonRAdec ", SwingConstants.RIGHT);

    moonaltazfield = new JTextField(16);
    moonaltazfield.setBackground(outputcolor);
    JLabel moonaltazlabel = new JLabel(" MoonAltAz ", SwingConstants.RIGHT);

    illumfracfield = new JTextField(16);
    illumfracfield.setBackground(outputcolor);
    JLabel illumfraclabel = new JLabel(" MoonIllumFrac ", SwingConstants.RIGHT);

    lunskyfield = new JTextField(16);
    lunskyfield.setBackground(outputcolor);
    JLabel lunskylabel = new JLabel(" LunSkyBrght ", SwingConstants.RIGHT);

    moonobjangfield = new JTextField(16);
    moonobjangfield.setBackground(outputcolor);
    JLabel moonobjanglabel = new JLabel(" Moon-Obj ang. ", SwingConstants.RIGHT);

    baryjdfield = new JTextField(16);
    baryjdfield.setBackground(outputcolor);
    JLabel baryjdlabel = new JLabel(" Bary. JD ", SwingConstants.RIGHT);

    baryvcorfield = new JTextField(16);
    baryvcorfield.setBackground(outputcolor);
    JLabel baryvcorlabel = new JLabel(" Bary. Vcorrn. ", SwingConstants.RIGHT);

    constelfield = new JTextField(16);
    constelfield.setBackground(outputcolor);
    JLabel constellabel = new JLabel(" Constellation ", SwingConstants.RIGHT);

    planetproximfield = new JTextField(16);
    planetproximfield.setBackground(outputcolor);
    JLabel planetproximlabel = new JLabel("Planet Warning? ", SwingConstants.RIGHT);

    iy = 0;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(objnamelabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(objnamefield, constraints);
    objnamefield.setText("null");

    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(RAlabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(RAfield, constraints);

    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(declabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(decfield, constraints);

    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(equinoxlabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(equinoxfield, constraints);

    // Need to fill in values or synchOutput chokes later ...
    RAfield.setText(o.c.alpha.roundedRAString(2, ":"));
    decfield.setText(o.c.delta.roundedDecString(1, ":"));
    equinoxfield.setText(String.format(Locale.ENGLISH, "%7.2f", o.c.equinox));

    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(datelabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(datefield, constraints);

    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(timelabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(timefield, constraints);

    JPanel buttonpan = new JPanel();
    UTbuttons = new ButtonGroup();
    buttonpan.setBackground(panelcolor);

    buttonpan.add(Localradiobutton = new JRadioButton("Local", true));
    Localradiobutton.setActionCommand("Local");
    Localradiobutton.addActionListener(new ActionListener() {    // so it toggles time ...
      public void actionPerformed(ActionEvent e) {
        setToJD();
      }
    });
    UTbuttons.add(Localradiobutton);

    buttonpan.add(UTradiobutton = new JRadioButton("UT", false));
    UTradiobutton.setActionCommand("UT");
    UTradiobutton.addActionListener(new ActionListener() {    // so it toggles time ...
      public void actionPerformed(ActionEvent e) {
        setToJD();
      }
    });
    UTbuttons.add(UTradiobutton);

    JLabel buttonlabel = new JLabel("Time is:");
    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(buttonlabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(buttonpan, constraints);

    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(timesteplabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(timestepfield, constraints);

    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(sleeplabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(sleepfield, constraints);

    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(JDlabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(JDfield, constraints);

    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(obsnamelabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(obsnamefield, constraints);

    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(longitudelabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(longitudefield, constraints);

    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(latitudelabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(latitudefield, constraints);

    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(stdzlabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(stdzfield, constraints);

    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(use_dstlabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(use_dstfield, constraints);

    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(zonenamelabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(zonenamefield, constraints);

    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(elevsealabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(elevseafield, constraints);

    iy++;
    constraints.gridx = 0;
    constraints.gridy = iy;
    textpanel.add(elevhorizlabel, constraints);
    constraints.gridx = 1;
    constraints.gridy = iy;
    textpanel.add(elevhorizfield, constraints);


    // Right-hand column of output ...

    iy = 0;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(sidereallabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(siderealfield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(HAlabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(HAfield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(airmasslabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(airmassfield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(altazlabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(altazfield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(parallacticlabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(parallacticfield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(sunradeclabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(sunradecfield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(sunaltazlabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(sunaltazfield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(ztwilightlabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(ztwilightfield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(moonphaselabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(moonphasefield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(moonradeclabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(moonradecfield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(moonaltazlabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(moonaltazfield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(illumfraclabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(illumfracfield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(lunskylabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(lunskyfield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(moonobjanglabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(moonobjangfield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(baryjdlabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(baryjdfield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(baryvcorlabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(baryvcorfield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(constellabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(constelfield, constraints);

    iy++;
    constraints.gridx = 2;
    constraints.gridy = iy;
    textpanel.add(planetproximlabel, constraints);
    constraints.gridx = 3;
    constraints.gridy = iy;
    textpanel.add(planetproximfield, constraints);

    textpanel.setBackground(panelcolor);

//      /* A panel for the site buttons, to be housed in a separate frame ... */
//
//      siteframe = new SiteWindow();
//      siteframevisible = true;
//      siteframe.setVisible(siteframevisible);

    /* A panel for planets, to be hidden */

    PlWin = new PlanetWindow();
    PlWin.setTitle("Low-Precision Planetary Positions");
    PlWin.setVisible(planetframevisible);

    /* A panel for the nightly almanac */

    Nightly = new NightlyAlmanac(o.w);
    NgWin = new NightlyWindow();
    NgWin.setTitle("Nightly Almanac");
    NgWin.setVisible(nightlyframevisible);

    /* A panel for hourly airmass */

    HrWin = new HourlyWindow();
    HrWin.setTitle("Hourly Circumstances");
    HrWin.Update();
    HrWin.setVisible(hourlyframevisible);

    /* A panel for object lists */

    RASelectors = new String[1];
    RASelectors[0] = " "; // this needs to be initialized
    ObjSelWin = new AstrObjSelector(true);  // argument is "is_single"

    /* Sky display window .... */

    final int skywinxpix = 800;
    final int skywinypix = 700;
    SkyDisp = new SkyDisplay(skywinxpix, skywinypix); // LBO: initial size

    // Use SkyDisp and not SkyWin:
    SkyDisp.setLocation(50, 300);
    SkyDisp.setVisible(skydisplayvisible);

    /* A panel for alternate coords (e.g. current RA and DEC, galactic etc.) */

    AltWin = new AltCoordWin();
    AltWin.setVisible(altcoowinvisible);

    /* A seasonal observability window ... */
    season = new Seasonal(o);
    SeasonWin = new SeasonalWindow();
    SeasonWin.setVisible(seasonframevisible);

    AirmSelWin = new AstrObjSelector(false);
    AirmSelWin.setVisible(airmasswindowvisible);

    AirDisp = new AirmassDisplay(800, 500);
    AirDisp.setLocation(400, 200);
    AirDisp.setVisible(airmasswindowvisible);

    HelpWin = new HelpWindow();
    HelpWin.setVisible(helpwindowvisible);

    // control buttons go in a separate panel at the bottom ...

    JPanel controlbuttonpanel = new JPanel();
    controlbuttonpanel.setLayout(new FlowLayout());  // it's the default anyway
    Dimension controlbuttonpansize = new Dimension(540, 170);  // ugh
    controlbuttonpanel.setPreferredSize(controlbuttonpansize);

    JButton synchbutton = new JButton("Refresh output");
    synchbutton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setToDate();
      }
    });
    controlbuttonpanel.add(synchbutton);

    JButton nowbutton = new JButton("Set to Now");
    nowbutton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        SetToNow();
      }
    });
    controlbuttonpanel.add(nowbutton);

    JButton forwardbutton = new JButton("Step Forward");
    forwardbutton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        advanceTime();
      }
    });
    controlbuttonpanel.add(forwardbutton);

    JButton backbutton = new JButton("Step Back");
    backbutton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        advanceTime(false);
      }
    });
    controlbuttonpanel.add(backbutton);

    this.autoupdatebutton = new JButton("Auto Update");
    autoupdatebutton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (autosteprunning) {
          autostepbutton.doClick();
        }
        if (!autoupdaterunning) {
          sleepfield.setBackground(runningcolor);  // draw attention
          thr = new Thread(au);
          autoupdaterunning = true;
          thr.start();
          autoupdatebutton.setText("Stop Update");
          autoupdatebutton.setBackground(Color.ORANGE);
        } else {
          autoupdaterunning = false;
          thr.interrupt();
          autoupdatebutton.setText("Resume Update");
          sleepfield.setBackground(Color.WHITE);
          autoupdatebutton.setBackground(Color.WHITE);  // not quite right
        }
      }
    });
    controlbuttonpanel.add(autoupdatebutton);

    this.autostepbutton = new JButton("Auto Step");
    autostepbutton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (autoupdaterunning) {
          autoupdatebutton.doClick();
        }
        if (!autosteprunning) {
          sleepfield.setBackground(runningcolor);  // draw attention
          timestepfield.setBackground(runningcolor);  // draw attention
          thr = new Thread(as);
          autosteprunning = true;
          thr.start();
          autostepbutton.setText("Stop Stepping");
          autostepbutton.setBackground(Color.ORANGE);
        } else {
          autosteprunning = false;
          thr.interrupt();
          autostepbutton.setText("Resume Stepping");
          sleepfield.setBackground(Color.WHITE);
          timestepfield.setBackground(Color.WHITE);
          autostepbutton.setBackground(Color.WHITE);  // not quite right
        }
      }
    });
    controlbuttonpanel.add(autostepbutton);

//  -- I can't figure out how to alter the UT/local radiobuttons from within
//     the program.
//      JButton UTLocalbutton = new JButton("UT <-> Local");
//      UTLocalbutton.addActionListener(new ActionListener() {
//         public void actionPerformed(ActionEvent e) {
//            SwapUTLocal();
//         }
//      });
//      controlbuttonpan.add(UTLocalbutton);

    if (JSkyCalc.ALLOW_SITE_WINDOW) {
      JButton sitebutton = new JButton("Site\n Menu");
      sitebutton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          siteframe.setVisible(!siteframevisible);
        }
      });
      controlbuttonpanel.add(sitebutton);
    }

    JButton planetbutton = new JButton("Planet Table");
    planetbutton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        PlWin.setVisible(!planetframevisible);
      }
    });
    controlbuttonpanel.add(planetbutton);

    JButton hourlybutton = new JButton("Hourly Circumstances");
    hourlybutton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        HrWin.setVisible(!hourlyframevisible);
      }
    });
    controlbuttonpanel.add(hourlybutton);

    JButton nightlybutton = new JButton("Nightly Almanac");
    nightlybutton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        NgWin.setVisible(!nightlyframevisible);
      }
    });
    controlbuttonpanel.add(nightlybutton);

    JButton seasonalshow = new JButton("Seasonal Observability");
    seasonalshow.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        SeasonWin.setVisible(!seasonframevisible);
      }
    });
    controlbuttonpanel.add(seasonalshow);

    JButton objselshow = new JButton("Object Lists ...");
    objselshow.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        ObjSelWin.setVisible(!objselwinvisible);
      }
    });
    controlbuttonpanel.add(objselshow);

    JButton skydisplayshow = new JButton("Sky Display");
    skydisplayshow.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        SkyDisp.setVisible(!skydisplayvisible);
      }
    });
    controlbuttonpanel.add(skydisplayshow);

    JButton altwinshow = new JButton("Alt. Coordinates");
    altwinshow.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        AltWin.setVisible(!altcoowinvisible);
      }
    });
    controlbuttonpanel.add(altwinshow);

    JButton airmassshow = new JButton("Airmass Graphs");
    airmassshow.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        if (!airmasswindowvisible) {
          // will become visible (see below):
          synchOutput();
        }
        AirmSelWin.setVisible(!airmasswindowvisible);
        AirDisp.setVisible(!airmasswindowvisible);
      }
    });

    controlbuttonpanel.add(airmassshow);

    if (JSkyCalc.ALLOW_QUIT) {
      JButton stopper = new JButton("Quit");
      stopper.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          /* If you hate the confirm-exit, start killing lines here.... */
          int result = JOptionPane.showConfirmDialog(frame,
                  "Really quit JSkyCalc?");
          switch (result) {
            case JOptionPane.YES_OPTION:
              System.exit(1);      // protected exit call ...
              break;
            case JOptionPane.NO_OPTION:
            case JOptionPane.CANCEL_OPTION:
            case JOptionPane.CLOSED_OPTION:
              break;
          }
          /* ... and stop killing them here, then uncomment the line below. */
          // System.exit(1);    // ... naked exit call.
        }
      });
      stopper.setBackground(sitecolor);
      controlbuttonpanel.add(stopper);
    }

    JButton helpwinshow = new JButton("Help");
    helpwinshow.setBackground(runningcolor);
    helpwinshow.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        HelpWin.setVisible(!helpwindowvisible);
      }
    });

    controlbuttonpanel.add(helpwinshow);
//
//      JButton airmassshow = new JButton("Airmass Graphs");
//      airmassshow.addActionListener(new ActionListener() {
//         public void actionPerformed(ActionEvent e) {
//            if(!airmasswindowvisible) {
//               airmasswindowvisible = true;
//               AirWin.setVisible(true);
//               AirmSelWin.setVisible(true);
//            }
//            else {
//               airmasswindowvisible = false;
//               AirWin.setVisible(false);
//               AirmSelWin.setVisible(false);
//            }
//         }
//      });
//
//      controlbuttonpan.add(airmassshow);

    controlbuttonpanel.setBackground(panelcolor);

    /* action listeners for the text fields ... */

    objnamefield.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        try {
          String sel = objnamefield.getText();
          RAfield.setText(
                  presenterKey.get(sel).c.alpha.roundedRAString(3, " "));
          decfield.setText(
                  presenterKey.get(sel).c.delta.roundedDecString(2, " "));
          equinoxfield.setText(String.format(Locale.ENGLISH, "%7.2f",
                  presenterKey.get(sel).c.equinox));
          synchOutput();
        } catch (Exception exc) {
          objnamefield.setText("Not Found.");
        }
      }
    });

    RAfield.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setToDate();
      }
    });

    decfield.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setToDate();
      }
    });

    equinoxfield.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setToDate();
      }
    });

    datefield.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setToDate();
      }
    });

    timefield.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setToDate();
      }
    });

    JDfield.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setToJD();
      }
    });

    timestepfield.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        advanceTime();
      }
    });

    // Actions on observatory parameters lead to a setToJD()
    // to parallel behavior of the site menu.

    obsnamefield.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setToJD();
      }
    });

    longitudefield.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setToJD();
      }
    });

    latitudefield.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setToJD();
      }
    });

    stdzfield.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setToJD();
      }
    });

    use_dstfield.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setToJD();
      }
    });

    zonenamefield.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setToJD();
      }
    });

    elevseafield.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setToJD();
      }
    });

    elevhorizfield.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setToJD();
      }
    });

    Container outer = frame.getContentPane();
    // LBO: use BoxLayout instead of FlowLayout:
//    outer.setLayout(new FlowLayout());
    outer.setLayout(new BoxLayout(outer, BoxLayout.Y_AXIS));
    outer.add(bannerpanel);
    outer.add(textpanel);
    outer.add(controlbuttonpanel);
    outer.setBackground(panelcolor);

    // default is hide on close:
    if (JSkyCalc.ALLOW_QUIT) {
      frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }

    // LBO: do not use window.setSize()
    setMinimumSize(new Dimension(560, 620));
//      frame.setSize(560,620);    
    frame.setLocation(30, 30);
    frame.setContentPane(outer);

    synchSite();
    synchOutput();
    au = new AutoUpdate("x");   // instantiate for later start if desired
    as = new AutoStep("s");

    // hints = new littleHints();

    // LBO: use Swing pattern to size the window:
    frame.pack();

    frame.setVisible(true);
  }

  void synchOutput() {
    boolean is_ut;
    Color mooncolor;  // used for several fields
    double parallactic, altparallactic;

    o.c.UpdateFromStrings(RAfield.getText(), decfield.getText(),
            equinoxfield.getText());

    // special hook for equinox of date ...
    if (o.c.equinox < 0.) {
      o.c.equinox = o.w.when.julianEpoch();
    }

    // and repeat them back ...

    RAfield.setText(o.c.alpha.roundedRAString(2, " "));
    decfield.setText(o.c.delta.roundedDecString(1, " "));
    equinoxfield.setText(String.format(Locale.ENGLISH, "%7.2f", o.c.equinox));

    o.computeSky();
    o.computeSunMoon();

    p.Update(o.w);
    if (planetframevisible) {
      PlWin.DisplayUpdate();
    }
    o.computeBary(p);
    checkPlanets();

//      if(hourlyframevisible) {   // an expensive operation
//         HrWin.Update();
//      }

    if (nightlyframevisible) {
      if (!hourlyframevisible) {
        Nightly.Update(o.w);
      }
      NgWin.UpdateDisplay();
    }

    if (seasonframevisible) {
      SeasonWin.DisplayUpdate();
    }

    HAfield.setText(o.ha.roundedHAString(0, " "));
    HAfield.setBackground(HAWarningColor(o.ha.value));

    if (o.altitude < 0.) {
      airmassfield.setText("(down.)");
    } else if (o.airmass > 10.) {
      airmassfield.setText("> 10.");
    } else {
      airmassfield.setText(String.format(Locale.ENGLISH, "%6.3f", o.airmass));
    }
    airmassfield.setBackground(AirMassWarningColor(o.altitude, o.airmass));

    altazfield.setText(String.format(Locale.ENGLISH, "%5.1f  az = %6.1f", o.altitude, o.azimuth));

    parallactic = o.parallactic;
    while (parallactic < -180d) {
      parallactic += 360d;
    }
    while (parallactic >= 180d) {
      parallactic -= 360d;
    }
    altparallactic = parallactic + 180d;
    while (altparallactic < -180d) {
      altparallactic += 360d;
    }
    while (altparallactic >= 180d) {
      altparallactic -= 360d;
    }

    parallacticfield.setText(String.format(Locale.ENGLISH, "%5.1f  [%5.1f] degr.", parallactic,
            altparallactic));

    if (o.w.altsun > 0.) {
      ztwilightfield.setText("(Daytime.)");
    } else if (o.w.altsun < -18.) {
      ztwilightfield.setText("No twilight.");
    } else {
      ztwilightfield.setText(String.format(Locale.ENGLISH, "%5.1f mag (blue)", o.w.twilight));
    }
    ztwilightfield.setBackground(TwilightWarningColor(o.w.altsun, o.w.twilight));

    mooncolor = MoonWarningColor(o.w.altmoon, o.altitude, o.w.altsun, o.moonobj, o.moonlight);
    if (o.w.altmoon < -2.) {
      lunskyfield.setText("Moon is down.");
    } else if (o.altitude < 0.) {
      lunskyfield.setText("Target is down.");
    } else if (o.w.altsun < -12.) {
      lunskyfield.setText(String.format(Locale.ENGLISH, "%5.1f V mag/sq arcsec", o.moonlight));
    } else if (o.w.altsun < 0.) {
      lunskyfield.setText("(Bright twilight.)");
    } else {
      lunskyfield.setText("(Daytime.)");
    }
    lunskyfield.setBackground(mooncolor);

    baryjdfield.setText(String.format(Locale.ENGLISH, "%13.5f  [%6.1f s]",
            o.baryjd, o.barytcor));
    baryvcorfield.setText(String.format(Locale.ENGLISH, "%6.2f km/s", o.baryvcor));
    constelfield.setText(Constel.getconstel(o.c));

    String UTstring = UTbuttons.getSelection().getActionCommand();
    if (UTstring.equals("UT")) {
      is_ut = true;
    } else {
      is_ut = false;      // easy binary choice
    }
    if (is_ut) {
      datefield.setText(o.w.when.UTDate.roundedCalString(6, 0));
      timefield.setText(o.w.when.UTDate.roundedCalString(11, 1));
    } else {
      datefield.setText(o.w.when.localDate.roundedCalString(6, 0));
      timefield.setText(o.w.when.localDate.roundedCalString(11, 1));
    }
    JDfield.setText(String.format(Locale.ENGLISH, "%15.6f", o.w.when.jd));
    // w.siderealobj.setRA(w.sidereal);  // update
    siderealfield.setText(o.w.siderealobj.roundedRAString(0, " "));
    sunradecfield.setText(o.w.sun.topopos.shortstring());
    moonradecfield.setText(o.w.moon.topopos.shortstring());
    sunaltazfield.setText(String.format(Locale.ENGLISH, "%5.1f   az = %6.1f", o.w.altsun, o.w.azsun));
    moonaltazfield.setText(String.format(Locale.ENGLISH, "%5.1f   az = %6.1f", o.w.altmoon, o.w.azmoon));
    moonphasefield.setText(String.format(Locale.ENGLISH, "%s", o.w.moon.getPhaseDescription(o.w.when.jd)));
    illumfracfield.setText(String.format(Locale.ENGLISH, "%5.3f", o.w.moonillum));
    moonobjangfield.setText(String.format(Locale.ENGLISH, "%5.1f deg", o.moonobj));
    moonobjangfield.setBackground(mooncolor);

    if (altcoowinvisible) {
      AltWin.Refresh();
    }

    if (skydisplayvisible) {
      SkyDisp.repaint();
    }

    if (hourlyframevisible) {   // an expensive operation
      HrWin.Update();
    }

    if (airmasswindowvisible) {
      if (!hourlyframevisible) {
        Nightly.Update(o.w);
      }
      AirDisp.Update();
    }

    season.Update(o);

  }

  void synchSite() {

    String SiteString = SiteButtons.getSelection().getActionCommand();
    if (SiteString.equals("x")) {   // code for load from fields ...
      o.w.where.name = obsnamefield.getText();
      o.w.where.lat.setDec(latitudefield.getText());
      o.w.where.longit.setFromString(longitudefield.getText());
      o.w.where.stdz = Double.parseDouble(stdzfield.getText());
      o.w.where.use_dst = Integer.parseInt(use_dstfield.getText());
      o.w.where.timezone_name = zonenamefield.getText();
      String[] fields = elevseafield.getText().trim().split("\\s+");  // chuck unit
      o.w.where.elevsea = Double.parseDouble(fields[0]);
      fields = elevhorizfield.getText().trim().split("\\s+");  // chuck unit
      // System.out.printf("About to try to parse %s ... \n",fields[0]);
      double elevhoriz_in = Double.parseDouble(fields[0]);
      if (elevhoriz_in >= 0.) {
        o.w.where.elevhoriz = Double.parseDouble(fields[0]);
      } else {
        System.out.printf("Negative elev_horiz causes a square root error later.  Set to zero.\n");
        o.w.where.elevhoriz = 0.;
      }
    } else {
      o.w.changeSite(siteDict, SiteString);
    }
    // and spit them all back out ...
    obsnamefield.setText(o.w.where.name);
    longitudefield.setText(o.w.where.longit.roundedLongitString(1, " ", false));
    latitudefield.setText(o.w.where.lat.roundedDecString(0, " "));
    stdzfield.setText(String.format(Locale.ENGLISH, "%5.2f", o.w.where.stdz));
    use_dstfield.setText(String.format(Locale.ENGLISH, "%d", o.w.where.use_dst));
    zonenamefield.setText(o.w.where.timezone_name);
    elevseafield.setText(String.format(Locale.ENGLISH, "%4.0f m", o.w.where.elevsea));
    elevhorizfield.setText(String.format(Locale.ENGLISH, "%4.0f m", o.w.where.elevhoriz));
  }

  Color MoonWarningColor(double altmoon,
          double altitude, double altsun, double moonobj, double moonlight) {
    if (altmoon < 0. | altitude < 0.) {
      return outputcolor;
    }
    if (altsun > -12.) {
      return outputcolor;  // twilight dominates
    }
    if (moonobj > 25.) {   // not proximity
      if (moonlight > 21.5) {
        return outputcolor;
      } else if (moonlight > 19.5) {
        // System.out.printf("lightblue moon \n");
        return lightblue;
      } else {
        // System.out.printf("lightpurple moon\n");
        return lightpurple;
      }
    }
    if (moonobj < 10.) {
      return Color.RED;  // always flag < 10 deg
    }
    if (moonlight > 21.5) {
      return outputcolor;
    }
    if (moonlight > 19.5) {
      // System.out.printf("brightyellow moon \n");
      return brightyellow;
    }
    if (moonlight > 18.) {
      return Color.ORANGE;
    }
    return Color.RED;
  }

  Color AirMassWarningColor(double altitude, double airmass) {
    if (altitude < 0.) {
      return Color.RED;
    }
    if (airmass < 2.) {
      return outputcolor;
    }
    if (airmass > 4.) {
      return Color.RED;
    }
    if (airmass > 3.) {
      return Color.ORANGE;
    }
    return brightyellow;
  }

  Color TwilightWarningColor(double altsun, double twilight) {
    if (altsun < -18.) {
      return outputcolor;
    }
    if (altsun > 0.) {
      return lightblue;
    }
    if (twilight < 3.5) {
      return brightyellow;
    }
    if (twilight < 8.) {
      return Color.ORANGE;
    }
    return Color.RED;
  }

  Color HAWarningColor(Double haval) {
    if (Math.abs(haval) > 6.) {
      return Color.ORANGE;
    } else {
      return outputcolor;
    }
  }

  void SetToNow() {

    // Get site and UT options
    synchSite();

    o.w.setToNow();
    o.w.computeSunMoon();
    synchOutput();


  }

  class AutoUpdate implements Runnable {

    String mystr;

    AutoUpdate(String s) {
      this.mystr = s;   // constructor does basically nothing ...
    }

    public void run() {
      try {
        while (autoupdaterunning) {
          SetToNow();
          sleepinterval = 1000l * Integer.parseInt(sleepfield.getText());

          Thread.sleep(sleepinterval);
        }
      } catch (InterruptedException e) {
//          System.out.println("Auto-update interrupted.");
      }
//      System.out.println("Thread: exit");
    }
  }

  class AutoStep implements Runnable {

    String mystr;

    AutoStep(String s) {
      this.mystr = s;   // constructor does basically nothing ...
    }

    public void run() {
      try {
        while (autosteprunning) {
          advanceTime();
          sleepinterval = 1000l * Integer.parseInt(sleepfield.getText());

          Thread.sleep(sleepinterval);
        }
      } catch (InterruptedException e) {
//        System.out.println("Auto-step interrupted.");
      }
//      System.out.println("Thread: exit");
    }
  }
//   void SwapUTLocal() {
//      boolean is_ut;
//
//      // Get site and UT options
//      String SiteString = SiteButtons.getSelection().getActionCommand();
//      w.changeSite(SiteString);
//
//      System.out.printf("swapping ... ");
//
//      String UTstring = UTbuttons.getSelection().getActionCommand();
//      if (UTstring.equals("UT")) {
//          System.out.printf("UT was selected ... \n");
//          //UTbuttons.setSelected("UT",false);
//          //UTbuttons.setSelected("Local",true);
//          Localradiobutton.setSelected(true);
//          UTradiobutton.setSelected(false);
//          Localradiobutton.repaint();
//    //     UTradiobutton.setSelected(false);
//    //     UTbuttons.setSelectedJRadioButton(Localradiobutton,true);
//          is_ut = false;
//      }
//      else {
//          System.out.printf("Local was selected ... \n");
//     //     UTbuttons.setSelected("UT",true);
//          UTradiobutton.setSelected(true);
//          Localradiobutton.setSelected(false);
//          Localradiobutton.repaint();
//     //     Localradiobutton.setSelected(false);
//     //     UTradiobutton.setSelected(true);
//     //     UTbuttons.setSelectedJRadioButton(UTradiobutton,true);
//          is_ut = true;
//      }
//      System.out.println("ut " + UTradiobutton.isSelected());
//      setToDate();
//   }

  void advanceTime() {

    synchSite();

    String advanceString = timestepfield.getText();
    o.w.advanceWhen(advanceString);
    o.w.computeSunMoon();

    synchOutput();

  }

  void advanceTime(boolean forward) {

    synchSite();

    String advanceString = timestepfield.getText();
    o.w.advanceWhen(advanceString, forward);
    o.w.computeSunMoon();

    synchOutput();
  }

  void setToDate() {

    String dateTimeString;
    boolean is_ut;

    // Get site and UT options
    //String SiteString = SiteButtons.getSelection().getActionCommand();
    //o.w.changeSite(SiteString);
    synchSite();

    String UTstring = UTbuttons.getSelection().getActionCommand();
    if (UTstring.equals("UT")) {
      is_ut = true;
    } else {
      is_ut = false;      // easy binary choice
    }
    // grab date and time from input fields
    String dateString = datefield.getText();
    String[] datepieces = dateString.split("\\s+");
    // take first three fields in date -- to ignore day of week
    dateTimeString = String.format(Locale.ENGLISH, "%s %s %s %s", datepieces[0], datepieces[1],
            datepieces[2], timefield.getText());

    // System.out.printf("%s %s %s\n",dateTimeString,UTstring,is_ut);
    // set the actual time ...
    o.w.changeWhen(dateTimeString, is_ut);
    // and update the display with the new values.
    o.w.computeSunMoon();
    synchOutput();
  }

  void setToJD() {

    double jd;
    boolean is_ut;

    // Get site and UT options, and set them ...
    //String SiteString = SiteButtons.getSelection().getActionCommand();
    //o.w.changeSite(SiteString);
    synchSite();

    String UTstring = UTbuttons.getSelection().getActionCommand();
    if (UTstring.equals("UT")) {
      is_ut = true;
    } else {
      is_ut = false;      // easy binary choice
    }
    // grab the jd from the display
    jd = Double.parseDouble(JDfield.getText());

    // System.out.printf("jd = %f\n",jd);
    // change the time using current options
    o.w.changeWhen(jd);
    o.w.computeSunMoon();
    synchOutput();


  }

  class PlanetWindow extends JFrame {
    // a subclass

    String[] headings;
    Object[][] PlanetDispData;
    JTable ptable;
    double[] PlanObjAng;

    PlanetWindow() {
      int i = 0;
      int j = 0;

      headings = new String[]{"Name", "RA", "Dec", "HA", "airmass", "proximity"};
      JPanel container = new JPanel();
      // LBO: use BoxLayout instead of FlowLayout:
      container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS));

      p.Update(o.w);
      PlanObjAng = new double[9];

      PlanetDispData = new Object[8][6];
      for (i = 0; i < 9; i++) {
        if (i != 2) {  // skip earth
          PlanetDispData[j][0] = p.names[i];
          PlanetDispData[j][1] = p.PlanetObs[i].c.alpha.roundedRAString(-1, ":");
          PlanetDispData[j][2] = p.PlanetObs[i].c.delta.roundedDecString(-2, ":");
          PlanetDispData[j][3] = p.PlanetObs[i].ha.roundedHAString(-2, ":");
          if (p.PlanetObs[i].altitude < 0.) {
            PlanetDispData[j][4] = "(Down.)";
          } else if (p.PlanetObs[i].airmass > 10.) {
            PlanetDispData[j][4] = "> 10.";
          } else {
            PlanetDispData[j][4] = String.format(Locale.ENGLISH, "%5.2f",
                    p.PlanetObs[i].airmass);
          }
          PlanObjAng[i] = Const.DEG_IN_RADIAN * Spherical.subtend(o.c, p.PlanetObs[i].c);
          PlanetDispData[j][5] = String.format(Locale.ENGLISH, "%6.2f", PlanObjAng[i]);
          j = j + 1;
        }
      }

      ptable = new JTable(PlanetDispData, headings);
      JScrollPane scroller = new JScrollPane(ptable);
      ptable.setPreferredScrollableViewportSize(new Dimension(470, 130));
      container.add(scroller);

      JButton hider = new JButton("Hide Planet Table");
      hider.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setVisible(false);
        }
      });

      JButton printer = new JButton("Print Planet Table");
      printer.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          Print();  // JTable has this automated utility to do this ...
        }
      });

      JPanel buttonPanel = new JPanel();
      buttonPanel.add(hider);
      buttonPanel.add(printer);

      container.add(buttonPanel);

      // LBO: do not use window.setSize()
      setMinimumSize(new Dimension(490, 220));
//      this.setSize(490, 220);
//          this.setSize(520,260);
      this.setLocation(400, 200);
      this.setContentPane(container);

      // LBO: use Swing pattern to size the window:
      this.pack();
    }

    /**
     * LBO: avoid excessive repaints
     * @param visible 
     */
    @Override
    public void setVisible(final boolean visible) {
      if (visible) {
        if (planetframevisible) {
          return;
        }
        DisplayUpdate();   // refresh it ...
      } else {
        if (!planetframevisible) {
          return;
        }
      }
      planetframevisible = visible;
      super.setVisible(visible);
    }

    void DisplayUpdate() {  // assumes computations are already done
      int i = 0;
      int j = 0;
      p.Update(o.w);
      for (i = 0; i < 9; i++) {
        if (i != 2) {  // skip earth
          ptable.setValueAt(p.names[i], j, 0);
          ptable.setValueAt(p.PlanetObs[i].c.alpha.roundedRAString(-1, ":"), j, 1);
          ptable.setValueAt(p.PlanetObs[i].c.delta.roundedDecString(-2, ":"), j, 2);
          ptable.setValueAt(p.PlanetObs[i].ha.roundedHAString(-2, ":"), j, 3);
          if (p.PlanetObs[i].altitude < 0.) {
            ptable.setValueAt("(Down.)", j, 4);
          } else if (p.PlanetObs[i].airmass > 10.) {
            ptable.setValueAt("> 10.", j, 4);
          } else {
            ptable.setValueAt(String.format(Locale.ENGLISH, "%5.2f", p.PlanetObs[i].airmass), j, 4);
          }
          ;
          PlanObjAng[i] = Const.DEG_IN_RADIAN * Spherical.subtend(o.c, p.PlanetObs[i].c);
          ptable.setValueAt(String.format(Locale.ENGLISH, "%6.2f", PlanObjAng[i]), j, 5);
          j = j + 1;
        }
      }
    }

    void Print() {
      try {
        ptable.print();
      } catch (PrinterException e) {
        System.out.printf("Printer exception caught.\n");
      }
    }
  }

  class SeasonalWindow extends JFrame {
    // a subclass

    String[] headings;
    // Object[][] SeasonalDispData;
    JTable seasontable;

    SeasonalWindow() {

      headings = new String[]{"Moon", "Evening Date", "HA.eve", "airm.eve", "HA.ctr", "airm.ctr",
        "HA.morn", "airm.morn", "hrs<3", "hrs<2", "hrs<1.5"};

      JPanel container = new JPanel();
      // LBO: use BoxLayout instead of FlowLayout:
      container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS));

      season.Update(o);

//          SeasonalDispData = new Object[16][11];
//          for(i = 0; i < 9; i++) {
//            if(i != 2) {  // skip earth
//                PlanetDispData[j][0] = p.names[i];
//                PlanetDispData[j][1] = p.PlanetObs[i].c.alpha.roundedRAString(-1,":");
//                PlanetDispData[j][2] = p.PlanetObs[i].c.delta.roundedDecString(-2,":");
//                PlanetDispData[j][3] = p.PlanetObs[i].ha.roundedHAString(-2,":");
//                if(p.PlanetObs[i].altitude < 0.) PlanetDispData[j][4] = "(Down.)";
//                else if(p.PlanetObs[i].airmass > 10.) PlanetDispData[j][4] = "> 10.";
//                else PlanetDispData[j][4] = String.format(Locale.ENGLISH, "%5.2f",
//                      p.PlanetObs[i].airmass);
//                PlanObjAng[i] = Const.DEG_IN_RADIAN * Spherical.subtend(o.c,p.PlanetObs[i].c);
//                PlanetDispData[j][5] = String.format(Locale.ENGLISH, "%6.2f",PlanObjAng[i]);
//                j = j + 1;
//             }
//         }
//
      seasontable = new JTable(season.tabledata, headings);

      // complicated rigamarole to set column widths

      seasontable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
      TableColumn col = seasontable.getColumnModel().getColumn(0);
      col.setPreferredWidth(40);  // pixels
      col = seasontable.getColumnModel().getColumn(1);
      col.setPreferredWidth(105);  // pixels
      int colno;
      for (colno = 2; colno < 11; colno++) {
        col = seasontable.getColumnModel().getColumn(colno);
        col.setPreferredWidth(60);
      }

      seasontable.setPreferredScrollableViewportSize(new Dimension(690, 270));
      JScrollPane scroller = new JScrollPane(seasontable);
      container.add(scroller);

      JButton hider = new JButton("Hide Observability Table");
      hider.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setVisible(false);
        }
      });

      JButton printer = new JButton("Print Observability Table");
      printer.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          Print();  // JTable has this automated utility to do this ...
        }
      });

      JPanel buttonPanel = new JPanel();
      buttonPanel.add(hider);
      buttonPanel.add(printer);

      container.add(buttonPanel);

      // LBO: do not use window.setSize()
      setMinimumSize(new Dimension(710, 370));
//      this.setSize(710, 370);
//          this.setSize(730,420);
      this.setLocation(400, 200);
      this.setContentPane(container);

      // LBO: use Swing pattern to size the window:
      this.pack();
    }

    /**
     * LBO: avoid excessive repaints
     * @param visible 
     */
    @Override
    public void setVisible(final boolean visible) {
      if (visible) {
        if (seasonframevisible) {
          return;
        }
        SeasonWin.DisplayUpdate();
      } else {
        if (!seasonframevisible) {
          return;
        }
      }
      seasonframevisible = visible;
      super.setVisible(visible);
    }

    void DisplayUpdate() {  // assumes computations are already done
      int i = 0;
      int j = 0;
      season.Update(o); // this doesn't update unless it's warranted.
      if (season.wasupdated) {  // don't bother refreshing table unless the update happened.
        for (i = 0; i < 16; i++) {
          for (j = 0; j < 11; j++) {
            seasontable.setValueAt(season.tabledata[i][j], i, j);
          }
        }
      }
    }

    void Print() {
      try {
        seasontable.print();
      } catch (PrinterException e) {
        System.out.printf("Printer exception caught.\n");
      }
    }
  }

  class HourlyWindow extends JFrame {

    /** I'd hoped to use the JTable class for this, but the java developers
     in their infinite wisdom do not provide any handy way of changing the color
     of an individual cell.  So I will go with an array of entry boxes, which
     should be much easier to handle (though getting it to print will involve
     more labor). */
    JTextField[][] hrfield;

    HourlyWindow() {
      int i = 0;
      int j = 0;

      JPanel container = new JPanel();
      // LBO: use BoxLayout instead of FlowLayout:
      container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS));

      JPanel tablepanel = new JPanel();
      tablepanel.setLayout(new GridBagLayout());
      GridBagConstraints tableconstraints = new GridBagConstraints();

      JLabel[] headers = new JLabel[7];
      headers[0] = new JLabel("Local");
      headers[1] = new JLabel("UT");
      headers[2] = new JLabel("LST");
      headers[3] = new JLabel("HA");
      headers[4] = new JLabel("Airmass");
      headers[5] = new JLabel("moonalt");
      headers[6] = new JLabel("sunalt");
      tableconstraints.gridy = 0;
      for (j = 0; j < 7; j++) {
        tableconstraints.gridx = j;
        tablepanel.add(headers[j], tableconstraints);
        headers[j].setForeground(new Color(100, 100, 200));
      }

      hrfield = new JTextField[18][7];
      for (i = 0; i < 18; i++) {
        for (j = 0; j < 7; j++) {
          // System.out.printf("i %d j %d\n",i,j);
          if (j == 0) {
            hrfield[i][j] = new JTextField(11);
          } else {
            hrfield[i][j] = new JTextField(5);
          }
          tableconstraints.gridx = j;
          tableconstraints.gridy = i + 1;
          tablepanel.add(hrfield[i][j], tableconstraints);
          hrfield[i][j].setBackground(outputcolor);
        }
      }

      JButton hider = new JButton("Hide Hourly Table");
      hider.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setVisible(false);
        }
      });

      JButton printer = new JButton("Dump to 'jskycalc.out'");
      printer.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          DumpToFile();
        }
      });

      container.add(tablepanel);

      JPanel buttonPanel = new JPanel();
      buttonPanel.add(hider);
      buttonPanel.add(printer);

      container.add(buttonPanel);

      // LBO: do not use window.setSize()
      setMinimumSize(new Dimension(495, 440));
//      this.setSize(495, 440);
//        this.setSize(510,480);
      this.setLocation(400, 150);
      this.setContentPane(container);

      // LBO: use Swing pattern to size the window:
      this.pack();
    }

    /**
     * LBO: avoid excessive repaints
     * @param visible 
     */
    @Override
    public void setVisible(final boolean visible) {
      if (visible) {
        if (hourlyframevisible) {
          return;
        }
        HrWin.Update();
      } else {
        if (!hourlyframevisible) {
          return;
        }
      }
      hourlyframevisible = visible;
      super.setVisible(visible);
    }

    void Update() {
      int i, j, starthr, endhr, jdint;
      double jd, jdtemp;

      Color nodatacolor = new Color(100, 100, 100);

      Observation otmp = (Observation) o.clone();

      // compute nightly almanac ; start with first whole hour before
      // sunset, end with first whole hour after sunrise.

      // Had a horrible time debugging this because all the WhenWhere
      // instances in Nightly were pointing at the same When!!
      // They do all point to the same Where, which is actually OK.

      Nightly.Update(otmp.w);

      jdtemp = Nightly.sunset.when.jd;
      jdint = (int) jdtemp;
      starthr = (int) (24d * (jdtemp - jdint));

      jdtemp = Nightly.sunrise.when.jd + 1. / 24.;  // round up
      endhr = (int) (24. * (jdtemp - jdint));

      /*        System.out.printf("\nTabulation start and end:");
       otmp.w.changeWhen((double) jdint + (double) starthr / 24.);
       otmp.w.when.localDate.quickprint();
       System.out.printf("\n end:");
       otmp.w.changeWhen((double) jdint + (double) endhr / 24.);
       otmp.w.when.localDate.quickprint();
       System.out.printf("\n"); */

      i = starthr;
      j = 0;   // start with first row of table
      while (i <= endhr) {
        otmp.w.changeWhen((double) jdint + (double) i / 24.);
        otmp.computeSky();
        otmp.computeSunMoon();
        hrfield[j][0].setText(otmp.w.when.localDate.roundedCalString(2, 0));
        hrfield[j][0].setBackground(outputcolor);
        hrfield[j][1].setText(otmp.w.when.UTDate.roundedCalString(12, 0));
        hrfield[j][1].setBackground(outputcolor);
        RA tempsid = new RA(otmp.w.sidereal);
        hrfield[j][2].setText(tempsid.roundedRAString(-2, ":"));
        hrfield[j][2].setBackground(outputcolor);
        hrfield[j][3].setText(otmp.ha.roundedHAString(-2, ":"));
        hrfield[j][3].setBackground(HAWarningColor(otmp.ha.value));
        // System.out.printf("HAstr %s\n",otmp.ha.roundedHAString(-2,":"));
        if (otmp.altitude < 0.) {
          hrfield[j][4].setText("(Down.)");
        } else if (otmp.airmass >= 10.) {
          hrfield[j][4].setText("> 10.");
        } else {
          hrfield[j][4].setText(String.format(Locale.ENGLISH, "%5.2f", otmp.airmass));
        }
        hrfield[j][4].setBackground(AirMassWarningColor(otmp.altitude, otmp.airmass));
        if (otmp.w.altmoon < -3.) {
          hrfield[j][5].setText("---");
        } else {
          hrfield[j][5].setText(String.format(Locale.ENGLISH, "%5.1f", otmp.w.altmoon));
        }
        hrfield[j][5].setBackground(MoonWarningColor(otmp.w.altmoon, otmp.altitude,
                otmp.w.altsun, otmp.moonobj, otmp.moonlight));
        if (otmp.w.altsun < -18.) {
          hrfield[j][6].setText("---");
        } else {
          hrfield[j][6].setText(String.format(Locale.ENGLISH, "%5.1f",
                  otmp.w.altsun));
        }
        hrfield[j][6].setBackground(TwilightWarningColor(otmp.w.altsun, otmp.w.twilight));
        i++;
        j++;
      }
      while (j < 18) {
        for (i = 0; i < 7; i++) {
          hrfield[j][i].setText(" ");
          hrfield[j][i].setBackground(nodatacolor);
        }
        j++;
      }
    }

    void DumpToFile() {
      int i = 0;
      int j = 0;
      filewriter f = new filewriter("jskycalc.out");
      //f.pw.printf("Boo!  (not implemented yet.)\n");
      f.pw.printf("\n\n** Hourly circumstances for NoNameYet ** \n\n");
      f.pw.printf("Coordinates: %s\n", o.c.checkstring());
      f.pw.printf("       Site: %s     (Year %d)\n\n", o.w.where.name,
              o.w.when.localDate.year);
      f.pw.printf("  --- Local ---  ");
      f.pw.printf("      UT     LST      HA  airmass  moonAlt sunAlt \n\n");
      for (i = 0; i < 18; i++) {
        for (j = 0; j < 7; j++) {
          if (j == 0) {
            f.pw.printf("%14s  ", hrfield[i][j].getText());
          } else {
            f.pw.printf("%7s ", hrfield[i][j].getText());
          }
        }
        f.pw.printf("\n");
      }
      f.closer();
    }
  }

  class NightlyWindow extends JFrame {

    /** Displays phenomena (sunset, moonrise, etc) */
    JTextField[] phenfield;
    JLabel[] phenlabel;
    String[] labeltext = {"Sunset", "Twilight Ends", "LST Eve. Twi.", "Night Center", "Twilight Begins",
      "LST Morn. Twi.", "Sunrise", "Moonrise", "Moonset"};

    NightlyWindow() {
      int i;

      JPanel container = new JPanel();
      // LBO: use BoxLayout instead of FlowLayout:
      container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS));

      JPanel tablepanel = new JPanel();
      tablepanel.setLayout(new GridBagLayout());
      GridBagConstraints tableconstraints = new GridBagConstraints();

      phenfield = new JTextField[9];
      phenlabel = new JLabel[9];
      for (i = 0; i < 9; i++) {
        tableconstraints.gridy = i;
        tableconstraints.gridx = 0;
        phenlabel[i] = new JLabel(labeltext[i]);
        tablepanel.add(phenlabel[i], tableconstraints);
        tableconstraints.gridx = 1;
        phenfield[i] = new JTextField(11);
        tablepanel.add(phenfield[i], tableconstraints);
      }

      JButton hider = new JButton("Hide Nightly Almanac");
      hider.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setVisible(false);
        }
      });
      hider.setAlignmentX(0.5f);
      container.add(tablepanel);

      container.add(hider);

      // LBO: do not use window.setSize()
      setMinimumSize(new Dimension(250, 240));
//      this.setSize(250, 240);
//         this.setSize(280,270);
      this.setLocation(400, 100);
      this.setContentPane(container);

      // LBO: use Swing pattern to size the window:
      this.pack();
    }

    /**
     * LBO: avoid excessive repaints
     * @param visible 
     */
    @Override
    public void setVisible(final boolean visible) {
      if (visible) {
        if (nightlyframevisible) {
          return;
        }
        Nightly.Update(o.w);
        NgWin.UpdateDisplay();
      } else {
        if (!nightlyframevisible) {
          return;
        }
      }
      nightlyframevisible = visible;
      super.setVisible(visible);
    }

    void UpdateDisplay() {  // assumes Nightly has been updated separately.

      phenfield[0].setText(Nightly.sunset.when.localDate.roundedCalString(2, 0));
      phenfield[1].setText(Nightly.eveningTwilight18.when.localDate.roundedCalString(2, 0));
      phenfield[2].setText(Nightly.eveningTwilight18.siderealobj.roundedRAString(-2, " "));
      phenfield[3].setText(Nightly.nightcenter.when.localDate.roundedCalString(2, 0));
      phenfield[4].setText(Nightly.morningTwilight18.when.localDate.roundedCalString(2, 0));
      phenfield[5].setText(Nightly.morningTwilight18.siderealobj.roundedRAString(-2, " "));
      phenfield[6].setText(Nightly.sunrise.when.localDate.roundedCalString(2, 0));

      if (Nightly.moonrise.when.jd < Nightly.moonset.when.jd) {
        phenfield[7].setText(Nightly.moonrise.when.localDate.roundedCalString(2, 0));
        phenlabel[7].setText("Moonrise");
        phenfield[8].setText(Nightly.moonset.when.localDate.roundedCalString(2, 0));
        phenlabel[8].setText("Moonset");
      } else {
        phenfield[7].setText(Nightly.moonset.when.localDate.roundedCalString(2, 0));
        phenlabel[7].setText("Moonset");
        phenfield[8].setText(Nightly.moonrise.when.localDate.roundedCalString(2, 0));
        phenlabel[8].setText("Moonrise");
      }
    }
  }

  class AstrObjSelector extends JFrame {

    final boolean isSingle;
    JList selectorList;

    AstrObjSelector(boolean is_single) {
      isSingle = is_single;

      JPanel outer = new JPanel();
      // LBO: use BoxLayout instead of FlowLayout:
      outer.setLayout(new BoxLayout(outer, BoxLayout.Y_AXIS));

      selectorList = new JList(RASelectors);

      if (is_single) {
        selectorList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        this.setTitle("Object Selector");
      } else {
        selectorList.setSelectionMode(
                ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        this.setTitle("Airmass Graph Sel.");
        outer.setBackground(new Color(100, 100, 100));  // color this differently
      }

      selectorList.setPrototypeCellValue("xxxxxxxxxxxxxxxxxxxx");
      outer.add(new JScrollPane(selectorList));

      JButton objselbutton = new JButton("Load Object List");
      objselbutton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          LoadAstrObjs();
          SkyDisp.repaint();   // cause them to appear on display.
        }
      });
      objselbutton.setAlignmentX(0.5f);
      outer.add(objselbutton);

      JButton byra = new JButton("Sort by RA");
      byra.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          selectorList.setListData(RASelectors);
        }
      });
      byra.setAlignmentX(0.5f);
      outer.add(byra);

      JButton byname = new JButton("Alphabetical Order");
      byname.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          selectorList.setListData(NameSelectors);
          //   for(int i = 0; i < NameSelectors.length; i++)
          //    System.out.printf("%s\n",NameSelectors[i]);
        }
      });
      byname.setAlignmentX(0.5f);
      outer.add(byname);

      JButton clearbutton = new JButton("Clear list");
      clearbutton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          ClearAstrObjs();
          SkyDisp.repaint();
        }
      });
      clearbutton.setAlignmentX(0.5f);
      outer.add(clearbutton);

      if (!is_single) {

        JButton plotairmasses = new JButton("Plot airmasses");
        plotairmasses.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            airmassPlotSelections = selectorList.getSelectedValues();
            System.out.printf("%d objects selected\n", airmassPlotSelections.length);
            synchOutput();
          }
        });
        plotairmasses.setAlignmentX(0.5f);
        outer.add(plotairmasses);

        JButton deselector = new JButton("Deselect all");
        deselector.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent e) {
            selectorList.clearSelection();
          }
        });
        deselector.setAlignmentX(0.5f);
        outer.add(deselector);
      }

      JButton hider = new JButton("Hide Window");
      hider.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setVisible(false);
        }
      });
      hider.setAlignmentX(0.5f);
      outer.add(hider);

      this.add(outer);
//            this.setSize(180,340);
      // leave more room for the airmass selector ...

      if (is_single) {
        // LBO: do not use window.setSize()
        setMinimumSize(new Dimension(210, 340));
//        this.setSize(210, 340);
      } else {
        // LBO: do not use window.setSize()
        setMinimumSize(new Dimension(210, 390));
//        this.setSize(210, 390);
      }
      if (is_single) {
        this.setLocation(575, 100);
      } else {
        this.setLocation(700, 50);
      }

      if (is_single) {
        selectorList.addListSelectionListener(new ListSelectionListener() {
          public void valueChanged(ListSelectionEvent l) {
            /* Changing the list fires a selection event that can
             generate bad data.  Catch the resulting exceptions. */
            try {
              String sel = (String) selectorList.getSelectedValues()[0];
              /*                    System.out.printf("%d %s %s %s\n",i,
               Selectors[i],
               presenterKey.get(Selectors[i]).name,
               presenterKey.get(Selectors[i]).c.checkstring()
               ); */
              RAfield.setText(
                      presenterKey.get(sel).c.alpha.roundedRAString(3, " "));
              decfield.setText(
                      presenterKey.get(sel).c.delta.roundedDecString(2, " "));
              equinoxfield.setText(String.format(Locale.ENGLISH, "%7.2f",
                      presenterKey.get(sel).c.equinox));
              synchOutput();
            } catch (ArrayIndexOutOfBoundsException e) {
            }
          }
        });
      }
      // LBO: use Swing pattern to size the window:
      this.pack();
    }

    /**
     * LBO: avoid excessive repaints
     * @param visible 
     */
    @Override
    public void setVisible(final boolean visible) {
      if (isSingle) {
        if (visible) {
          if (objselwinvisible) {
            return;
          }
        } else {
          if (!objselwinvisible) {
            return;
          }
        }
        objselwinvisible = visible;
      }
      super.setVisible(visible);
    }

    void LoadListByRA() {
      selectorList.setListData(RASelectors);
    }

    void LoadListByName() {
      selectorList.setListData(NameSelectors);
    }
  }

  void LoadAstrObjs() {

    FileGrabber ff = new FileGrabber();

    // LBO: fixed NPE
    if (ff == null || ff.infile == null) {
      System.out.printf("No objects loaded.\n");
      return;
    }

    try {
      LoadAstrObjs(ff.br);
    } finally {
      ff.closer();
    }
  }

  void LoadAstrObjs(BufferedReader br) {

    try {
      while ((st = br.readLine()) != null) {
        obj = new AstrObj(st);
        if (obj.name != null & obj.c != null) {
          byname.put(obj.name.toLowerCase(), obj);
          rakey = (Double) obj.c.alpha.value;
          // ensure unique RA keys by inserting small tie-breaker offset
          while (byra.keySet().contains(rakey)) {
            rakey += 0.00001d;
          }
          byra.put(rakey, obj);
        }
      }
    } catch (IOException e) {
      System.out.println(e);
    }

    java.util.List<String> namekeys = new ArrayList<String>(byname.keySet());
    java.util.List<Double> rakeys = new ArrayList<Double>(byra.keySet());

// LBO: disable alphabetical sort to keep things ordered
//    Collections.sort(namekeys);
    Collections.sort(rakeys);

    RASelectors = new String[rakeys.size()];

    Iterator raiterator = rakeys.iterator();
    int i = 0;
    while (raiterator.hasNext()) {
      Object key = raiterator.next();
      AstrObj tempobj = byra.get(key);
      presenterKey.put(tempobj.name, tempobj);
      RASelectors[i] = tempobj.name;
      i++;
    }

    NameSelectors = new String[namekeys.size()];

    Iterator nameiterator = namekeys.iterator();
    i = 0;
    while (nameiterator.hasNext()) {
      Object key = nameiterator.next();
      AstrObj tempobj = byname.get(key);
      // presenterkey is alreary loaded ... but the NameSelector array will
      // be in alphabetical order.
      NameSelectors[i] = tempobj.name;
      i++;
    }
    // LBO: use name to keep order
    ObjSelWin.LoadListByName();
    AirmSelWin.LoadListByName();
  }

  void freeAstroObjs() {
    byname.clear();
    byra.clear();
    presenterKey.clear();
  }

  void ClearAstrObjs() {

    NameSelectors = new String[1];
    NameSelectors[0] = "null";
    RASelectors = new String[1];
    RASelectors[0] = "null";

    java.util.List<String> namekeys = new ArrayList<String>(byname.keySet());
    java.util.List<Double> rakeys = new ArrayList<Double>(byra.keySet());

    Iterator raiterator = rakeys.iterator();
    while (raiterator.hasNext()) {
      Object key = raiterator.next();
      AstrObj tempobj = byra.get(key);
      byra.remove(key);
      presenterKey.remove(tempobj.name);
    }

    Iterator nameiterator = namekeys.iterator();
    while (nameiterator.hasNext()) {
      Object key = nameiterator.next();
      byname.remove(key);
    }

    Celest nullc = new Celest(0., 0., 2000d);
    AstrObj nullobj = new AstrObj("null", nullc);
    byra.put(0., nullobj);
    byname.put("null", nullobj);
    presenterKey.put("null", nullobj);

    // AstrObjSelector objsel = new AstrObjSelector();
    ObjSelWin.LoadListByName();   // which is now empty.
    AirmSelWin.LoadListByName();
  }

  void SelObjByPos(Celest incel) {
    /* get input from the graphical display, or wherever, and get the nearest
     object on the list within a tolerance.  Precession is ignored. */
    double tolerance = 0.1;  // radians
    double decband = 6.;     // degrees
    double decin;
    Celest objcel;
    double sep, minsep = 1000000000000.;
    int i, minindex = 0;

    if (presenterKey.size() > 0) {
      decin = incel.delta.value;
      for (i = 0; i < RASelectors.length; i++) {
        objcel = presenterKey.get(RASelectors[i]).c;
        if (Math.abs(decin - objcel.delta.value) < decband) {  // guard expensive subtend
          sep = Spherical.subtend(incel, objcel);
          if (sep < minsep) {
            minsep = sep;
            minindex = i;
          }
        }
      }

      if (minsep < tolerance) {
        objcel = presenterKey.get(RASelectors[minindex]).c;
        objnamefield.setText(RASelectors[minindex]);
        RAfield.setText(objcel.alpha.roundedRAString(2, " "));
        decfield.setText(objcel.delta.roundedDecString(1, " "));
        equinoxfield.setText(String.format(Locale.ENGLISH, "%7.2f", objcel.equinox));


      }
    }
  }

//   class littleHints extends JWindow {
//
//      final JTextArea area;
//
//      littleHints() {
//         area = new JTextArea();
//         area.append("Hints: ");
//         area.append("- White fields are for input data\n");
//         area.append("- Output refreshes with 'Enter' in input\n");
//         area.append("- Control buttons are in bottom panel \n");
//         area.append("- Some pop extra windows (e.g. hourly)\n");
//         area.append("- To enter your own site params, you must first\n");
//         area.append("  select 'Allow User Input' on site menu. \n");
//         area.append("- Sky Display has several useful keyboard  \n");
//         area.append("  and mouse controls - menu on right button.\n");
//         area.setBackground(brightyellow);
//         this.setSize(275,150);
//         this.add(area);
//      }
//
//      void showme(int x, int y) {
//         this.setLocation(x, y);
//         this.setVisible(true);
//      }
//      void hideme(int x, int y) {
//         this.setVisible(false);
//      }
//   }
  class SiteWindow extends JFrame {
    // shoves this large block of code into its own subclass.

    String[] sitekeys;
    int nsites;

    SiteWindow() {
      int iy = 0;
      int i = 0;

      ReadSites();    // see below.

      JPanel sitepan = new JPanel();
      sitepan.setLayout(new GridBagLayout());
      GridBagConstraints constr = new GridBagConstraints();
      SiteButtons = new ButtonGroup();
      JRadioButton radioButton;  // need a generic one here

      constr.anchor = GridBagConstraints.LINE_START;

      constr.gridx = 0;
      constr.gridy = iy;
      sitepan.add(radioButton = new JRadioButton("Enable User Input", false), constr);
      radioButton.setActionCommand("x");  // we'll wait before refreshing.
      radioButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          ColorUserInput(true);  // change site to input color
        }
      });

      SiteButtons.add(radioButton);

      for (i = 0; i < nsites; i++) {
        String name = sitekeys[i];

        iy++;
        constr.gridx = 0;
        constr.gridy = iy;
        sitepan.add(radioButton = new JRadioButton(name, true), constr);
        radioButton.setActionCommand(name);
        radioButton.addActionListener(new ActionListener() {  // ugly to do it to all...
          public void actionPerformed(ActionEvent e) {
            setToJD();
            ColorUserInput(false);
          }
        });
        SiteButtons.add(radioButton);
      }

      iy++;
      constr.gridx = 0;
      constr.gridy = iy;
      JButton sitehider = new JButton("Hide Site Chooser");
      sitehider.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setVisible(false);
        }
      });
      sitepan.add(sitehider, constr);

      Container sitecontainer = this.getContentPane();
      sitecontainer.add(sitepan);

      // LBO: do not use window.setSize()
      setMinimumSize(new Dimension(180, 580));
//      this.setSize(180, 580);
//         this.setSize(210,640);
      this.setLocation(585, 20);
      this.setContentPane(sitepan);

      // LBO: use Swing pattern to size the window:
      this.pack();
    }

    /**
     * LBO: avoid excessive repaints
     * @param visible 
     */
    @Override
    public void setVisible(final boolean visible) {
      if (visible) {
        if (siteframevisible) {
          return;
        }
      } else {
        if (!siteframevisible) {
          return;
        }
      }
      siteframevisible = visible;
      super.setVisible(visible);
    }

    Site firstSite() {
      // just pops out first site on list for initilization ...
      return siteDict.get(sitekeys[0]);
    }

    void ReadSites() {

      BufferedReader br = null;
      boolean inquote = false;
      String[] fields = new String[14];  // too many but that's ok
      int i, j;

      sitekeys = new String[40];         // hard-coded, sorry
      nsites = 0;

      try {
        InputStream is = this.getClass().getResourceAsStream("skycalcsites.dat");
        br = new BufferedReader(new InputStreamReader(is));
      } catch (Exception e) {
        System.out.printf("Problem opening skycalcsites.dat for input.\n");
      }

      // read the site info character-by-character to preserve quoted values.
      // there's undoubtedly a better way, but this works well enough.

      try {
        while ((st = br.readLine()) != null) {
          // System.out.printf("read: %s\n",st);
          if (st.length() > 0) {
            if (st.charAt(0) != '#') {
              j = 0;   // field counter
              fields[j] = "";
              for (i = 0; i < st.length(); i++) {
                char[] thischar = {st.charAt(i)};
                if (st.charAt(i) == '"') {
                  if (inquote) {
                    inquote = false;
                  } else {
                    inquote = true;
                  }
                } else {
                  if (inquote) {
                    fields[j] = fields[j] + new String(thischar);
                  } else {
                    if (st.charAt(i) == ',') {
                      j = j + 1;
                      fields[j] = "";
                    } else {
                      fields[j] = fields[j] + new String(thischar);
                    }
                  }
                }
              }
              siteDict.put(fields[0], new Site(fields));
              sitekeys[nsites] = fields[0];  // so they'll come out in order ...
              nsites++;
              //               for(j = 0; j < fields.length; j++) System.out.printf("%s ",fields[j]);
              //              System.out.printf("\n");
            }
          }
        }
      } catch (IOException e) {
        System.out.printf("IO exception\n");
      }
    }

    void ColorUserInput(boolean allowed) {
      /* sets the background color in all the site param boxes according to whether
       user input is allowed or not. */
      if (allowed) {
        obsnamefield.setBackground(inputcolor);
        longitudefield.setBackground(inputcolor);
        latitudefield.setBackground(inputcolor);
        stdzfield.setBackground(inputcolor);
        use_dstfield.setBackground(inputcolor);
        zonenamefield.setBackground(inputcolor);
        elevseafield.setBackground(inputcolor);
        elevhorizfield.setBackground(inputcolor);
      } else {
        obsnamefield.setBackground(sitecolor);
        longitudefield.setBackground(sitecolor);
        latitudefield.setBackground(sitecolor);
        stdzfield.setBackground(sitecolor);
        use_dstfield.setBackground(sitecolor);
        zonenamefield.setBackground(sitecolor);
        elevseafield.setBackground(sitecolor);
        elevhorizfield.setBackground(sitecolor);
      }
    }
  }

  class AltCoordWin extends JFrame {
    // shoves this large block of code into its own subclass.

    JTextField currentrafield;
    JTextField currentdecfield;
    JTextField currenteqfield;
    JTextField galactlongitfield;
    JTextField galactlatitfield;
    JTextField ecliptlatitfield;
    JTextField ecliptlongitfield;

    AltCoordWin() {

      JPanel coopan = new JPanel();
      coopan.setLayout(new GridBagLayout());
      GridBagConstraints constr = new GridBagConstraints();

      int iy = 0;
      int fw = 13;

      currentrafield = new JTextField(fw);
      JLabel currentralabel = new JLabel("Current RA: ", SwingConstants.RIGHT);
      constr.gridx = 0;
      constr.gridy = iy;
      coopan.add(currentralabel, constr);
      constr.gridx = 1;
      constr.gridy = iy;
      coopan.add(currentrafield, constr);
      currentrafield.setBackground(outputcolor);

      currentdecfield = new JTextField(fw);

      JLabel currentdeclabel = new JLabel("Current dec: ", SwingConstants.RIGHT);
      iy++;
      constr.gridx = 0;
      constr.gridy = iy;
      coopan.add(currentdeclabel, constr);
      constr.gridx = 1;
      constr.gridy = iy;
      coopan.add(currentdecfield, constr);
      currentdecfield.setBackground(outputcolor);

      currenteqfield = new JTextField(fw);
      JLabel currenteqlabel = new JLabel("Current Equinox: ", SwingConstants.RIGHT);
      iy++;
      constr.gridx = 0;
      constr.gridy = iy;
      coopan.add(currenteqlabel, constr);
      constr.gridx = 1;
      constr.gridy = iy;
      coopan.add(currenteqfield, constr);
      currenteqfield.setBackground(outputcolor);

      galactlongitfield = new JTextField(fw);
      JLabel galactlongitlabel = new JLabel("Galactic longit: ", SwingConstants.RIGHT);
      iy++;
      constr.gridx = 0;
      constr.gridy = iy;
      coopan.add(galactlongitlabel, constr);
      constr.gridx = 1;
      constr.gridy = iy;
      coopan.add(galactlongitfield, constr);
      galactlongitfield.setBackground(inputcolor);

      galactlatitfield = new JTextField(fw);
      JLabel galactlatitlabel = new JLabel("Galactic latit: ", SwingConstants.RIGHT);
      iy++;
      constr.gridx = 0;
      constr.gridy = iy;
      coopan.add(galactlatitlabel, constr);
      constr.gridx = 1;
      constr.gridy = iy;
      coopan.add(galactlatitfield, constr);
      galactlatitfield.setBackground(inputcolor);

      galactlongitfield.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          double galactlongit = Double.parseDouble(galactlongitfield.getText());
          double galactlatit = Double.parseDouble(galactlatitfield.getText());
          Celest ctmp = Spherical.gal2Cel(galactlongit, galactlatit);
          // get equinox from main window
          double eq = Double.parseDouble(equinoxfield.getText());
          ctmp.selfprecess(eq);
          RAfield.setText(ctmp.alpha.roundedRAString(2, " "));
          decfield.setText(ctmp.delta.roundedDecString(1, " "));
          setToDate();
        }
      });

      galactlatitfield.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          double galactlongit = Double.parseDouble(galactlongitfield.getText());
          double galactlatit = Double.parseDouble(galactlatitfield.getText());
          // System.out.printf("read %f %f from long/lat fields\n",galactlongit,galactlatit);
          Celest ctmp = Spherical.gal2Cel(galactlongit, galactlatit);
          // get equinox from main window
          double eq = Double.parseDouble(equinoxfield.getText());
          ctmp.selfprecess(eq);
          RAfield.setText(ctmp.alpha.roundedRAString(2, " "));
          decfield.setText(ctmp.delta.roundedDecString(1, " "));
          setToDate();
        }
      });

      ecliptlongitfield = new JTextField(fw);
      JLabel ecliptlongitlabel = new JLabel("Ecliptic longit.: ", SwingConstants.RIGHT);
      iy++;
      constr.gridx = 0;
      constr.gridy = iy;
      coopan.add(ecliptlongitlabel, constr);
      constr.gridx = 1;
      constr.gridy = iy;
      coopan.add(ecliptlongitfield, constr);
      ecliptlongitfield.setBackground(outputcolor);

      ecliptlatitfield = new JTextField(fw);
      JLabel ecliptlatitlabel = new JLabel("Ecliptic latit: ", SwingConstants.RIGHT);
      iy++;
      constr.gridx = 0;
      constr.gridy = iy;
      coopan.add(ecliptlatitlabel, constr);
      constr.gridx = 1;
      constr.gridy = iy;
      coopan.add(ecliptlatitfield, constr);
      ecliptlatitfield.setBackground(outputcolor);

      // LBO: do not use window.setSize()
      setMinimumSize(new Dimension(330, 200));
//      this.setSize(330, 200);
//        this.setSize(350,250);
      this.setLocation(400, 150);
      this.setContentPane(coopan);
      // LBO: use Swing pattern to size the window:
      this.pack();
    }

    /**
     * LBO: avoid excessive repaints
     * @param visible 
     */
    @Override
    public void setVisible(final boolean visible) {
      if (visible) {
        if (altcoowinvisible) {
          return;
        }
        AltWin.Refresh();
      } else {
        if (!altcoowinvisible) {
          return;
        }
      }
      altcoowinvisible = visible;
      super.setVisible(visible);
    }

    void Refresh() {
      // to be called at the end of a synchOutput so site etc are all done.

      currentrafield.setText(o.current.alpha.roundedRAString(2, " "));
      currentdecfield.setText(o.current.delta.roundedDecString(1, " "));
      currenteqfield.setText(String.format(Locale.ENGLISH, "%7.2f", o.current.equinox));
      o.c.galactic();
      galactlongitfield.setText(String.format(Locale.ENGLISH, "%6.2f", o.c.galong));
      galactlatitfield.setText(String.format(Locale.ENGLISH, "%6.2f", o.c.galat));
      double[] eclonglat = Ecliptic.Cel2Ecl(o);
      ecliptlongitfield.setText(String.format(Locale.ENGLISH, "%6.2f", eclonglat[0]));
      ecliptlatitfield.setText(String.format(Locale.ENGLISH, "%6.2f", eclonglat[1]));
    }
  }

  void checkPlanets() {
    int i;
    double sepn;

    String warningtext = "";
    int warninglevel = 0;  // save worst warning ... 0 none, 1 orange, 2 red.

    for (i = 0; i < 9; i++) {
      if (i != 2) {  // skip earth ....
        sepn = Spherical.subtend(o.c, p.PlanetObs[i].c) * Const.DEG_IN_RADIAN;
        if (sepn < 3.) {
          if (i > 0 & i < 6) {  // Venus through Saturn
            warningtext = warningtext + String.format(Locale.ENGLISH, "%s - %4.2f deg ", p.names[i], sepn);
            if (sepn < 1.) {
              warninglevel = 2;
            } else if (warninglevel < 2) {
              warninglevel = 1;
            }
          } else {   // the rest of the planets
            if (sepn < 1.) {
              warningtext = warningtext + String.format(Locale.ENGLISH, "%s - %4.2f deg ", p.names[i], sepn);
              if (warninglevel < 1) {
                warninglevel = 1;
              }
            }
          }
        }
      }
    }
    if (warningtext.equals("")) {
      planetproximfield.setText(" --- ");
      planetproximfield.setBackground(outputcolor);
    } else {
      planetproximfield.setText(warningtext);
      if (warninglevel == 1) {
        planetproximfield.setBackground(Color.ORANGE);
      } else {
        planetproximfield.setBackground(Color.RED);


      }
    }
  }

  class HelpWindow extends JFrame {

    HelpWindow() {

      JPanel container = new JPanel();
      // LBO: use BoxLayout instead of FlowLayout:
      container.setLayout(new BoxLayout(container, BoxLayout.Y_AXIS));

      JEditorPane pane = null;

      InputStream is = this.getClass().getResourceAsStream("helptext.html");
      // try {
      //      infile = new File("helptext.html");
      //      fr = new FileReader(infile);
      //   } catch (Exception e) {System.out.printf("Input reader didn't open right\n"); }

      try {
        pane = new JEditorPane();
        pane.setEditorKit(new HTMLEditorKit());
        // pane.read(fr,"HTMLDocument");
        pane.read(is, "HTMLDocument");
      } catch (Exception e) {
        System.out.printf("Manual text didn't open.\n");
      }
      pane.setEditable(false);
      JScrollPane scroller = new JScrollPane(pane);
      scroller.setPreferredSize(new Dimension(680, 640));
      // container.add(scroller,BorderLayout.NORTH);
      container.add(scroller);

      JButton hideme = new JButton("Hide Help Text");
      hideme.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setVisible(false);
        }
      });
      hideme.setAlignmentX(0.5f);

      // container.add(hideme,BorderLayout.SOUTH);
      container.add(hideme);
      add(container);

      // LBO: do not use window.setSize()
      setMinimumSize(new Dimension(700, 740));
//      setSize(700, 740);
//         setSize(720,780);
      setLocation(400, 10);

      // LBO: use Swing pattern to size the window:
      this.pack();
    }

    /**
     * LBO: avoid excessive repaints
     * @param visible 
     */
    @Override
    public void setVisible(final boolean visible) {
      if (visible) {
        if (helpwindowvisible) {
          return;
        }
      } else {
        if (!helpwindowvisible) {
          return;
        }
      }
      helpwindowvisible = visible;
      super.setVisible(visible);
    }
  }

  class SkyDisplay extends JFrame
          implements MouseMotionListener,
          KeyListener, MouseListener {

    double xpix, ypix, aspect;  // number of pixels in x and y directions, double
    double halfwidthy;          // sets the scale factor.
    double halfwidthx;
    double pixperunit;
    double xmid, ymid;          // x, y coords of the center of the screen.
    double halfwidthxfull, halfwidthyfull, pixperunitfull;
    Font smallfont;
    FontMetrics smallfontmetrics;
    Font mediumfont;
    FontMetrics mediumfontmetrics;
    Font largefont;
    FontMetrics largefontmetrics;
    BrightStar[] bs;
    double currentlat;       // keep track of this to avoid recomputing the HAgrid
    GeneralPath[] HAgrid;   // polylines that draw out hour angle paths and equator
    Color gridcolor;
    MouseEvent mousevent;
    KeyEvent keyevent;
    double zoomedby;
    tinyHelp tinyhelp;

    SkyDisplay(final int xpixIn, final int ypixIn) {
      super("Sky Display");
      // LBO: use SetPreferredSize and inherit Frame to get correct window size
      final Dimension initialDim = new Dimension(xpixIn + 15, ypixIn + 35);
      setMinimumSize(initialDim);
      setPreferredSize(initialDim);

      onResize(initialDim);

      xmid = 0.;
      ymid = 0.;
      zoomedby = 1.;
      /* 
       System.out.printf("xpix %f ypix %f aspect %f\n",xpix,ypix,aspect);
       System.out.printf("halfwidthx %f halfwidthy %f\n",halfwidthx, halfwidthy);
       System.out.printf("pixperunit %f\n",pixperunit);
       */

      smallfont = new Font("Dialog", Font.PLAIN, 11);
      mediumfont = new Font("Dialog", Font.PLAIN, 15);
      largefont = new Font("Dialog", Font.PLAIN, 18);
      gridcolor = new Color(153, 0, 0);  // dark red

      LoadBright();

      currentlat = o.w.where.lat.value;
      makeHAgrid(currentlat);   // computes it, doesn't plot it.

//      setFocusable(true);
      addKeyListener(this);
      addMouseMotionListener(this);
      addMouseListener(this);
      tinyhelp = new tinyHelp();

      addComponentListener(new ComponentAdapter() {
        @Override
        public void componentResized(final ComponentEvent e) {
          final Component c = e.getComponent();
          final Dimension d = c.getSize();

          onResize(new Dimension(d.width, d.height));
        }
      });
      // LBO: use Swing pattern to size the window:
      pack();
    }

    /**
     * LBO: avoid excessive repaints
     * @param visible 
     */
    @Override
    public void setVisible(final boolean visible) {
      if (visible) {
        if (skydisplayvisible) {
          return;
        }
      } else {
        if (!skydisplayvisible) {
          return;
        }
      }
      skydisplayvisible = visible;
      super.setVisible(visible);
    }

    /**
     * LBO fixed window resize
     * @param dim new dimension
     */
    private void onResize(final Dimension dim) {
      // update internal fields:
      xpix = dim.getWidth();
      ypix = dim.getHeight();
      aspect = xpix / ypix;
      halfwidthy = 0.88;
      halfwidthx = halfwidthy * aspect;
      pixperunit = ypix / (2d * halfwidthy);

      halfwidthxfull = halfwidthx;
      halfwidthyfull = halfwidthy;
      pixperunitfull = pixperunit;

      // reset currentlat to redraw HA grid:
      currentlat = 0d;
      repaint();
    }

    public void mousePressed(MouseEvent e) {
      Point parentloc = SkyDisp.getLocation();
      if (e.getButton() == MouseEvent.BUTTON3) {
        tinyhelp.show((int) parentloc.getX() + e.getX(),
                (int) parentloc.getY() + e.getY());
      }
    }

    public void mouseReleased(MouseEvent e) {
      if (e.getButton() == MouseEvent.BUTTON3) {
        tinyhelp.hide(e.getX(), e.getY());
      }
    }

    public void mouseClicked(MouseEvent e) {
      mousevent = e;
      if (mousevent.getButton() == MouseEvent.BUTTON1) {   // select from list
//              System.out.printf("%d %d\n",mousevent.getX(),mousevent.getY());
        Celest markedC = pixtocelest(mousevent.getX(), mousevent.getY());
        SelObjByPos(markedC);
        synchOutput();
      } else if (mousevent.getButton() == MouseEvent.BUTTON2) {  // middle sets to coords
//               System.out.printf("%d %d\n",mousevent.getX(),mousevent.getY());
        Celest markedC = pixtocelest(mousevent.getX(), mousevent.getY());
        RAfield.setText(markedC.alpha.roundedRAString(0, " "));
        decfield.setText(markedC.delta.roundedDecString(0, " "));
        synchOutput();
      }
    }

    public void mouseExited(MouseEvent e) {
    }

    public void mouseEntered(MouseEvent e) {
      this.requestFocusInWindow();
    }

    public void mouseDragged(MouseEvent e) {
    }  // not used for anything.

    public void mouseMoved(MouseEvent e) {    // keeps track of cursor location.
      mousevent = e;
      this.requestFocusInWindow();
//            System.out.printf("Mouse moved %d %d\n",mousevent.getX(),mousevent.getY());
//            if(this.isFocusOwner()) System.out.printf("We have focus.\n");
//            else System.out.printf("We Don't have focus.\n");
      // Focus appears flaky.  When window border is pink, kb input is accepted.
    }

    public void keyReleased(KeyEvent k) {
      // we don't care about these but need empty methods for the interface.
    }

    public void keyPressed(KeyEvent k) {
    }

// This has proven a little awkward under fvwm2 because focus doesn't follow mouse very
// faithfully.  But once you have focus it all works perfectly!
    public void keyTyped(KeyEvent k) {
      keyevent = k;
//           System.out.printf("Key typed at coords %d %d\n",mousevent.getX(),
//              mousevent.getY());    // yup, when we have focus, it works.
      if (k.getKeyChar() == 'f') {
        advanceTime();
      } else if (k.getKeyChar() == 'b') {
        advanceTime(false);
      } else if (k.getKeyChar() == 'c') {
        //   System.out.printf("%d %d\n",mousevent.getX(),mousevent.getY());
        Celest markedC = pixtocelest(mousevent.getX(), mousevent.getY());
        RAfield.setText(markedC.alpha.roundedRAString(0, " "));
        decfield.setText(markedC.delta.roundedDecString(0, " "));
        synchOutput();
      } else if (k.getKeyChar() == 's') {
        //   System.out.printf("%d %d\n",mousevent.getX(),mousevent.getY());
        Celest markedC = pixtocelest(mousevent.getX(), mousevent.getY());
        SelObjByPos(markedC);
        synchOutput();
      } else if (k.getKeyChar() == 'z') {
        zoom(mousevent.getX(), mousevent.getY(), 2.);
      } else if (k.getKeyChar() == 'o') {
        zoom(mousevent.getX(), mousevent.getY(), 0.5);
      } else if (k.getKeyChar() == 'p') {
        pan(mousevent.getX(), mousevent.getY());
      } else if (k.getKeyChar() == 'r') {
        restorefull();
      } else if (k.getKeyChar() == 'h') {  // HR star ...
        Celest markedC = pixtocelest(mousevent.getX(), mousevent.getY());
        SelBrightByPos(markedC);
        synchOutput();
      } else if (k.getKeyChar() == 'q' || k.getKeyChar() == 'x') {
        SkyDisp.setVisible(false);
      }
    }

    void SelBrightByPos(Celest incel) {
      /* get input from the graphical display, or wherever, and get the nearest
       bright star on the list within a tolerance.  Precession is ignored. */
      double tolerance = 0.1;  // radians
      double decband = 6.;     // degrees
      double decin;
      Celest objcel;
      double sep, minsep = 1000000000000.;
      int i, minindex = 0;
      decin = incel.delta.value;

      for (i = 0; i < bs.length; i++) {
        if (Math.abs(decin - bs[i].c.delta.value) < decband) {  // guard expensive subtend
          sep = Spherical.subtend(incel, bs[i].c);
          if (sep < minsep) {
            minsep = sep;
            minindex = i;
          }
        }
      }

      if (minsep < tolerance) {
        objnamefield.setText(bs[minindex].name);
        RAfield.setText(bs[minindex].c.alpha.roundedRAString(2, " "));
        decfield.setText(bs[minindex].c.delta.roundedDecString(1, " "));
        equinoxfield.setText(String.format(Locale.ENGLISH, "%7.2f", bs[minindex].c.equinox));
      }
    }

    void makeHAgrid(double latit) {

      int i;
      double ha, hamiddle, haend;
      double decstart, decmiddle, decend;
      double[] xystart;
      double[] xystartpix = {0., 0.};
      double[] xymiddle;
      double[] xymiddlepix = {0., 0.};
      double[] xyend;
      double[] xyendpix = {0., 0.};

      double coslat = Math.cos(latit / Const.DEG_IN_RADIAN);
      double sinlat = Math.sin(latit / Const.DEG_IN_RADIAN);

      HAgrid = new GeneralPath[8];

      // draw lines of constant HA from -6h to +6h
      for (i = 0; i < 7; i++) {
        HAgrid[i] = new GeneralPath();
        ha = (double) (2 * i - 6);
        decstart = 90d;
        decmiddle = 85d;
        decend = 80d;
        xystart = SkyProject(ha, decstart, coslat, sinlat);
        xystartpix = xytopix(xystart[0], xystart[1]);

        HAgrid[i].moveTo((float) xystartpix[0], (float) xystartpix[1]);
        while (decend > -91.) {
          xymiddle = SkyProject(ha, decmiddle, coslat, sinlat);
          xymiddlepix = xytopix(xymiddle[0], xymiddle[1]);
          xyend = SkyProject(ha, decend, coslat, sinlat);
          xyendpix = xytopix(xyend[0], xyend[1]);
          HAgrid[i].quadTo((float) xymiddlepix[0], (float) xymiddlepix[1],
                  (float) xyendpix[0], (float) xyendpix[1]);
          decmiddle = decend - 10.;
          decend = decend - 10.;
        }
      }
      // draw equator -- tuck it in as the last path in HAgrid.
      HAgrid[i] = new GeneralPath();
      ha = -6.;
      hamiddle = -5.5;
      haend = -5.;
      xystart = SkyProject(ha, 0., coslat, sinlat);
      xystartpix = xytopix(xystart[0], xystart[1]);
      HAgrid[i].moveTo((float) xystartpix[0], (float) xystartpix[1]);
      while (haend < 6.1) {
        xymiddle = SkyProject(hamiddle, 0., coslat, sinlat);
        xymiddlepix = xytopix(xymiddle[0], xymiddle[1]);
        xyend = SkyProject(haend, 0., coslat, sinlat);
        xyendpix = xytopix(xyend[0], xyend[1]);
        HAgrid[i].quadTo((float) xymiddlepix[0], (float) xymiddlepix[1],
                (float) xyendpix[0], (float) xyendpix[1]);
        hamiddle = hamiddle + 1.0;
        haend = haend + 1.0;
      }
//      System.out.printf("\n");
    }

    public void paint(Graphics g) {   // this method does the graphics.
      double[] xy1 = {0., 0.};
      double[] xy2 = {0., 0.};
      int i;

      // LBO: use antialiasing:
      Graphics2D g2 = (Graphics2D) g;
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
      g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);

      smallfontmetrics = g2.getFontMetrics(smallfont);
      mediumfontmetrics = g2.getFontMetrics(mediumfont);
      largefontmetrics = g2.getFontMetrics(largefont);
      g2.setBackground(skycolor());
      g2.clearRect(0, 0, (int) xpix, (int) ypix);

      // there's some attention given here to stacking these in the best order, since the
      // last thing painted scribbles over previous things.

      // LBO:
      if (o.w.where.lat.value != currentlat) {
        currentlat = o.w.where.lat.value;
        makeHAgrid(currentlat);
      }

      g2.setPaint(gridcolor);
      g2.setStroke(new BasicStroke(0.7f));
      for (i = 0; i < HAgrid.length; i++) {
        g2.draw(HAgrid[i]);
      }

      drawcircle(g2, 0., 0., 1., "solid", 0.7f);   // horizon
      drawcircle(g2, 0., 0., 0.57735, "dotted", 0.7f); // 2 airmasses
      drawcircle(g2, 0., 0., 0.70711, "dotted", 0.7f); // 3 airmasses
      drawcircle(g2, 0., 0., 0.7746, "dotted", 0.7f); // 4 airmasses
      g2.setStroke(new BasicStroke(1.0f));

      labelDirections(g2);

      PlotBright(g2);

      markcoords(g2);
      plotsun(g2);
      plotmoon(g2);
      plotplanets(g2);

      // LBO: fixed clock position
      drawclock(g2, xmid + 0.85 * halfwidthx, ymid + 0.75 * halfwidthy, 0.10 * halfwidthx);

      puttext(g2, xmid + 0.7 * halfwidthx, ymid - 0.90 * halfwidthy, o.w.where.name,
              mediumfont, mediumfontmetrics);
      puttext(g2, xmid + 0.7 * halfwidthx, ymid - 0.95 * halfwidthy,
              o.w.when.UTDate.roundedCalString(7, 0) + " UT", mediumfont,
              mediumfontmetrics);

      plotobjects(g2);
    }

    Color skycolor() {
      if (o.w.altsun < -18) {
        return Color.BLACK;
      }
      if (o.w.altsun > -0.8) {
        return new Color(61, 122, 140);  // daytime sky color
      }
      if (o.w.twilight > 5.) {
        double fac = (o.w.twilight - 5.) / 10.;
        return new Color(51 + (int) (fac * 10.2), 69 + (int) (fac * 54.),
                112 + (int) (fac * 28.));
      } else {
        double fac = (o.w.twilight + 4.) / 9.;
        return new Color((int) (51. * fac), (int) (68.85 * fac), (int) (112.2 * fac));
      }
    }

    void drawclock(Graphics2D g2, double x, double y, double radius) {

      int i;
      double angle, tickx1, tickx2, ticky1, ticky2;
      double cosang, sinang;
      double minutes;
      double seconds;
      double timeval;
      boolean isAM;
      double pixrad;

      double[] xy1 = {0., 0.};
      double[] xy2 = {0., 0.};

      g2.setPaint(Color.CYAN);
      xy1 = xytopix(x - radius, y + radius);
      pixrad = radius * pixperunit;
      g2.draw(new Ellipse2D.Double(xy1[0], xy1[1], 2. * pixrad, 2. * pixrad));

      for (i = 0; i < 12; i++) {
        angle = 0.523599 * (double) i;  // constants is 30 degrees expressed in rad
        cosang = Math.cos(angle);
        sinang = Math.sin(angle);
        tickx1 = x + 0.93 * radius * cosang;
        tickx2 = x + radius * cosang;
        ticky1 = y + 0.93 * radius * sinang;
        ticky2 = y + radius * sinang;
        drawline(g2, tickx1, ticky1, tickx2, ticky2);
      }
      timeval = o.w.when.localDate.timeofday.value;
      isAM = true;
      if (timeval >= 12.) {
        isAM = false;
      }
      while (timeval >= 12.) {
        timeval -= 12.;
      }
      // System.out.printf("Timeval = %f\n",timeval);

      // draw hands
      angle = 0.523599 * timeval;
      cosang = Math.cos(angle);
      sinang = Math.sin(angle);
      g2.setStroke(new BasicStroke(2.5f));
      drawline(g2, x, y, x + 0.65 * radius * sinang, y + 0.65 * radius * cosang);
      g2.setStroke(new BasicStroke(1.5f));

      angle = 6.2831853 * (timeval - (int) timeval);
      cosang = Math.cos(angle);
      sinang = Math.sin(angle);
      drawline(g2, x, y, x + 0.83 * radius * sinang, y + 0.83 * radius * cosang);

      /* Uncomment to plot a second hand   */
      g2.setPaint(Color.RED);
      minutes = 60. * (timeval - (int) timeval);
      angle = 6.2831853 * (minutes - (int) minutes);
      cosang = Math.cos(angle);
      sinang = Math.sin(angle);
      g2.setStroke(new BasicStroke(1.0f));
      drawline(g2, x + 0.7 * radius * sinang, y + 0.7 * radius * cosang,
              x + 0.88 * radius * sinang, y + 0.88 * radius * cosang);
      g2.setStroke(new BasicStroke(1.5f));
      g2.setPaint(Color.CYAN);
      /* */
      String ampm = "AM";
      if (!isAM) {
        ampm = "PM";
      }
      String dststr = "S";
      if (o.w.when.dstInEffect) {
        dststr = "D";
      }

      String outstr = String.format(Locale.ENGLISH, "%s %s%sT", ampm, o.w.where.zone_abbrev,
              dststr);
      puttext(g2, x, y - 1.4 * radius, outstr, mediumfont,
              mediumfontmetrics);
      if (o.w.altsun > -0.9) {
        outstr = "Daytime";
      } else if (o.w.altsun < -18.) {
        outstr = "Nighttime";
      } else {
        outstr = "Twilight";
      }
      // outstr = String.format(Locale.ENGLISH, "%s",o.w.where.name);
      puttext(g2, x, y - 1.7 * radius, outstr, mediumfont,
              mediumfontmetrics);
//           String dststr = "Standard";
      //          if (o.w.when.dstInEffect) dststr = "Daylight";
      //         outstr = String.format(Locale.ENGLISH, "%s %s Time",o.w.where.timezone_name,dststr);
      //        puttext(x - 0.9 * radius, y - 1.5 * radius, outstr, mediumfont);
    }

    double[] xytopix(double x, double y) {
      double[] retvals = {0., 0.,};
      retvals[0] = 0.5 * xpix * (1. + (x - xmid) / halfwidthx);
      retvals[1] = 0.5 * ypix * (1. - (y - ymid) / halfwidthy);
      return retvals;
    }

    double[] pixtoxy(int xpixel, int ypixel) {
      double x, y;  // map coords, zero at zenith, r = 1 at horizon, radius = tan z/2.
      double[] retvals = {0., 0.};

      retvals[0] = xmid + halfwidthx * ((double) (2d * xpixel) / (double) xpix - 1d);
      retvals[1] = ymid + halfwidthy * (1d - (double) (2d * ypixel) / (double) ypix);

      return retvals;
    }

    Celest pixtocelest(int xpixel, int ypixel) {
      double x, y;  // map coords, zero at zenith, r = 1 at horizon, radius = tan z/2.
      double alt, az;
      double[] retvals = {0., 0.};
      double xt, yt, zt;  // topocentric xyz, zt toward zenith, yt south, xt west

      x = xmid + halfwidthx * ((double) (2 * xpixel) / (double) xpix - 1.);
      y = ymid + halfwidthy * (1. - (double) (2 * ypixel) / (double) ypix);
      double mod = Math.sqrt(x * x + y * y);
      // retvals[0] = (Const.PI / 2. - 2. * Math.atan(mod)) * Const.DEG_IN_RADIAN;  // alt

      if (o.w.where.lat.value < 0.) {
        x = x * -1.;
        y = y * -1.;    // plot is inverted in s hemishphere ...
      }
      alt = (Const.PI / 2. - 2. * Math.atan(mod)); // alt
      az = Math.atan2(-1. * x, y); //  * Const.DEG_IN_RADIAN;
      // retvals[1] = az;
      // System.out.printf("%d %d -> %f %f -> %f %f\n",xpixel,ypixel,x,y,retvals[0],retvals[1]);
      zt = Math.sin(alt);
      xt = -1. * Math.cos(alt) * Math.sin(az);
      yt = -1. * Math.cos(alt) * Math.cos(az);
      // rotate around x axis, zenith to the pole ...
      double coslat = Math.cos(o.w.where.lat.radians());
      double sinlat = Math.sin(o.w.where.lat.radians());
      double yc = yt * sinlat + zt * coslat;
      double zc = -1. * yt * coslat + zt * sinlat;
      double[] hadecdist = Celest.xyzCel(xt, yc, zc);  // x remains the same
      // hadecdist[0] is the hour angle ...
      RA Alph = new RA(hadecdist[0] - 6. + o.w.sidereal);
      DEC Delt = new DEC(hadecdist[1]);
      Celest cel = new Celest(Alph, Delt, 2000d); // coord eq is sloppy ...
//           System.out.printf("%s %s\n",cel.alpha.roundedRAString(0,":"),
      //                 cel.delta.roundedDecString(-1,":"));
      return (cel);
    }

    void drawline(Graphics2D g2, double x1, double y1, double x2, double y2) {  // user coords
      double[] xy1;
      double[] xy2;
      xy1 = xytopix(x1, y1);
      xy2 = xytopix(x2, y2);
      g2.draw(new Line2D.Double(xy1[0], xy1[1], xy2[0], xy2[1]));
    }

    void drawcircle(Graphics2D g2, double x1, double y1, double radius, String style, float thickness) {
      // centered on x1, y1 user coords, radius is also in user coords,
      // string can be "solid" or "dashed"
      double[] xy = {0., 0.};
      xy = xytopix(x1 - radius, y1 + radius);   // upper left corner
      double diam = 2. * pixperunit * radius;

      if (style.equals("dashed")) {
        float[] dash1 = {10.0f};
        BasicStroke dashed = new BasicStroke(thickness, BasicStroke.CAP_BUTT,
                BasicStroke.JOIN_MITER, 10.0f, dash1, 0.0f);
        g2.setStroke(dashed);
      }
      if (style.equals("dotted")) {
        float[] dot1 = {3.0f};
        BasicStroke dotted = new BasicStroke(thickness, BasicStroke.CAP_BUTT,
                BasicStroke.JOIN_MITER, 3.0f, dot1, 0.0f);
        g2.setStroke(dotted);
      }

      g2.draw(new Ellipse2D.Double(xy[0], xy[1], diam, diam));
      g2.setStroke(new BasicStroke(thickness));
    }

    void drawbox(Graphics2D g2, double x1, double y1, double edge, String style, float thickness) {
      // centered on x1, y1 user coords, radius is also in user coords,
      // string can be "solid" or "dashed"
      double[] xy = {0., 0.};
      xy = xytopix(x1 - edge / 2., y1 + edge / 2.);   // upper left corner
      double edgepix = pixperunit * edge;

      if (style.equals("dashed")) {
        float[] dash1 = {10.0f};
        BasicStroke dashed = new BasicStroke(thickness, BasicStroke.CAP_BUTT,
                BasicStroke.JOIN_MITER, 10.0f, dash1, 0.0f);
        g2.setStroke(dashed);
      }
      if (style.equals("dotted")) {
        float[] dot1 = {3.0f};
        BasicStroke dotted = new BasicStroke(thickness, BasicStroke.CAP_BUTT,
                BasicStroke.JOIN_MITER, 3.0f, dot1, 0.0f);
        g2.setStroke(dotted);
      }

      g2.draw(new Rectangle2D.Double(xy[0], xy[1], edgepix, edgepix));
      g2.setStroke(new BasicStroke(thickness));
    }

    void markcoords(Graphics2D g2) {
      double lst;
      double xy[];

      lst = o.w.sidereal;
      double coslat = Math.cos(o.w.where.lat.radians());
      double sinlat = Math.sin(o.w.where.lat.radians());
      xy = SkyProject(lst - o.current.alpha.value,
              o.current.delta.value, coslat, sinlat);
      g2.setPaint(objboxcolor);
      drawbox(g2, xy[0], xy[1], 0.05, "solid", 1.3f);
    }

    void plotsun(Graphics2D g2) {
      double lst;
      double xy[];

      lst = o.w.sidereal;
      double coslat = Math.cos(o.w.where.lat.radians());
      double sinlat = Math.sin(o.w.where.lat.radians());
      xy = SkyProject(lst - o.w.sun.topopos.alpha.value,
              o.w.sun.topopos.delta.value, coslat, sinlat);
      putDot(g2, xy[0], xy[1], 0.009, brightyellow);
      g2.setPaint(brightyellow);
      drawcircle(g2, xy[0], xy[1], 0.02, "solid", 1.5f);
    }

    void puttext(Graphics2D g2, double x, double y, String text, Font font, FontMetrics metrics) {
      double[] xy;
      // write text centered left-to-right on user coords x and y.
      xy = xytopix(x, y);

      g2.setFont(font);
      int adv = metrics.stringWidth(text);
      // System.out.printf("adv = %d\n",adv);
      g2.drawString(text, (int) xy[0] - adv / 2, (int) xy[1]);
    }

    void labelDirections(Graphics2D g2) {
      if (o.w.where.lat.value > 0.) {
        puttext(g2, -0.97, 0., "E", largefont, largefontmetrics);
        puttext(g2, 0.97, 0., "W", largefont, largefontmetrics);
        puttext(g2, 0.0, 0.80, "N", largefont, largefontmetrics);
        puttext(g2, 0.0, -0.83, "S", largefont, largefontmetrics);
      } else {
        puttext(g2, -0.97, 0., "W", largefont, largefontmetrics);
        puttext(g2, 0.97, 0., "E", largefont, largefontmetrics);
        puttext(g2, 0.0, 0.80, "S", largefont, largefontmetrics);
        puttext(g2, 0.0, -0.83, "N", largefont, largefontmetrics);
      }
    }

    void putDot(Graphics2D g2, double x, double y, double size, Color color) {
      double[] xy;
      // dotsize, unfortunately, is in user units.
      xy = xytopix(x - size / 2., y + size / 2.);
      g2.setPaint(color);
      g2.fill(new Ellipse2D.Double(xy[0], xy[1], pixperunit * size, pixperunit * size));
    }

    void LoadBright() {

      int i = 0;
      bs = new BrightStar[904];
      File infile = null;
      FileReader fr = null;
      BufferedReader br = null;
      String st;

      try {
        InputStream is = this.getClass().getResourceAsStream("brightest.dat");
        // infile = new File("brightest.dat");
        //  fr = new FileReader(infile);
        br = new BufferedReader(new InputStreamReader(is));
      } catch (Exception e) {
        System.out.printf("Problem opening brightest.dat for input.\n");
      }

      try {
        while ((st = br.readLine()) != null) {
          bs[i] = new BrightStar(st);
          i++;
        }
      } catch (IOException e) {
        System.out.println(e);
      }

//      System.out.printf("%d bright stars read.\n", bs.length);
    }

    void PrecessBrightStars(double equinox) {
      // precesses the stars in the BrightStar list bs.  This is more efficient
      // with a special method because the matrix only gets computed once.

      // code is largely copied from the precess method of Celest.
      double ti, tf, zeta, z, theta;
      double cosz, coszeta, costheta, sinz, sinzeta, sintheta, cosdelt;
      double[][] p = {{0., 0., 0.,}, {0., 0., 0.}, {0., 0., 0.}};
      double[] orig = {0., 0., 0.};
      double[] fin = {0., 0., 0.};
      double[] radecdist = {0., 0., 0.};

      int i, j, ist;

      // compute the precession matrix ONCE.
      ti = (bs[0].c.equinox - 2000d) / 100.;
      tf = (equinox - 2000d - 100. * ti) / 100.;

      zeta = (2306.2181 + 1.39656 * ti + 0.000139 * ti * ti) * tf
              + (0.30188 - 0.000344 * ti) * tf * tf + 0.017998 * tf * tf * tf;
      z = zeta + (0.79280 + 0.000410 * ti) * tf * tf + 0.000205 * tf * tf * tf;
      theta = (2004.3109 - 0.8533 * ti - 0.000217 * ti * ti) * tf - (0.42665 + 0.000217 * ti) * tf * tf - 0.041833 * tf * tf * tf;

      cosz = Math.cos(z / Const.ARCSEC_IN_RADIAN);
      coszeta = Math.cos(zeta / Const.ARCSEC_IN_RADIAN);
      costheta = Math.cos(theta / Const.ARCSEC_IN_RADIAN);
      sinz = Math.sin(z / Const.ARCSEC_IN_RADIAN);
      sinzeta = Math.sin(zeta / Const.ARCSEC_IN_RADIAN);
      sintheta = Math.sin(theta / Const.ARCSEC_IN_RADIAN);

      p[0][0] = coszeta * cosz * costheta - sinzeta * sinz;
      p[0][1] = -1. * sinzeta * cosz * costheta - coszeta * sinz;
      p[0][2] = -1. * cosz * sintheta;

      p[1][0] = coszeta * sinz * costheta + sinzeta * cosz;
      p[1][1] = -1. * sinzeta * sinz * costheta + coszeta * cosz;
      p[1][2] = -1. * sinz * sintheta;

      p[2][0] = coszeta * sintheta;
      p[2][1] = -1. * sinzeta * sintheta;
      p[2][2] = costheta;

      for (ist = 0; ist < bs.length; ist++) {
        cosdelt = Math.cos(bs[ist].c.delta.radians());
        orig[0] = cosdelt * Math.cos(bs[ist].c.alpha.radians());
        orig[1] = cosdelt * Math.sin(bs[ist].c.alpha.radians());
        orig[2] = Math.sin(bs[ist].c.delta.radians());
        for (i = 0; i < 3; i++) {  // matrix multiplication
          fin[i] = 0.;
          //System.out.printf("orig[%d] = %f\n",i,orig[i]);
          for (j = 0; j < 3; j++) {
            //System.out.printf("%d%d: %f  ",i,j,p[i][j]);
            fin[i] += p[i][j] * orig[j];
          }
          //System.out.printf("\nfin[%d] = %f\n\n",i,fin[i]);
        }
        radecdist = Celest.xyzCel(fin[0], fin[1], fin[2]);
        bs[ist].c.alpha.setRA(radecdist[0]);
        bs[ist].c.delta.setDec(radecdist[1]);
        bs[ist].c.equinox = equinox;
      }
    }

    double[] SkyProject(double hain, double decin, double coslat, double sinlat) {

      double[] retvals = {0., 0.};

      // This will be called many times so code it for speed!

      double ha = hain / Const.HRS_IN_RADIAN;
      double dec = decin / Const.DEG_IN_RADIAN;
      double x, y, z;

      double cosdec = Math.cos(dec);

      x = cosdec * Math.sin(ha);
      y = cosdec * Math.cos(ha);
      z = Math.sin(dec);

      double ypr = sinlat * y - coslat * z;   // rotating backward, by CO-Latitude, so sin
      double zpr = coslat * y + sinlat * z;   // and cos switch around.

      double zdist = Math.acos(zpr);
      double r = Math.tan(zdist / 2.);
      double inground = Math.sqrt(x * x + ypr * ypr);
      retvals[0] = r * (x / inground);
      retvals[1] = -1. * r * (ypr / inground);
      if (sinlat < 0.) {  // invert for south
        retvals[0] = -1. * retvals[0];
        retvals[1] = -1. * retvals[1];
      }

      return retvals;
    }

    void PlotBright(Graphics2D g2) {
      // stripped down for speed -- avoids the OO stuff .
      // Precesses only if mismatch is > 1 year.
      double ha, dec, lst;
      double equinoxnow;
      double xy[];
      double magconst1 = 0.002, magslope = 0.002;  //
      double magzpt = 4.7;
      int i;

      lst = o.w.sidereal;

      // precess the list if the epoch mismatch is > 1 yr
      equinoxnow = o.w.when.julianEpoch();
      if (Math.abs(equinoxnow - bs[0].c.equinox) > 1.) {
        // System.out.printf("%s -> ",bs[0].c.checkstring());
        PrecessBrightStars(equinoxnow);
        // System.out.printf("%s \n",bs[0].c.checkstring());
      }

      double coslat = Math.cos(o.w.where.lat.radians());
      double sinlat = Math.sin(o.w.where.lat.radians());
      for (i = 0; i < bs.length; i++) {
        xy = SkyProject(lst - bs[i].c.alpha.value, bs[i].c.delta.value, coslat, sinlat);
        //System.out.printf("PB %f %f\n",xy[0],xy[1]);
        putDot(g2, xy[0], xy[1], magconst1 + magslope * (magzpt - bs[i].m), bs[i].col);
      }
    }

    void plotmoon(Graphics2D g2) {

      int nseg = 21;

      double[][] limbxy = new double[2][nseg];    // in system where cusp axis = y
      double[][] termxy = new double[2][nseg];

      double[][] limbxypr = new double[2][nseg];  // in system where North = y
      double[][] termxypr = new double[2][nseg];

      double theta;
      double cos_sunmoon;
      int i, j, k;

      // set up points describing terminator and limb -- then rotate into
      // position.
      cos_sunmoon = Math.cos(o.w.sunmoon / Const.DEG_IN_RADIAN);
      // System.out.printf("sunmoon %f cos_sunmoon %f\n",o.w.sunmoon,cos_sunmoon);
      double dtheta = Const.PI / (double) (nseg - 1);
      for (j = 0; j < nseg; j++) {
        theta = dtheta * (double) j;
        limbxy[0][j] = Math.cos(theta);
        limbxy[1][j] = Math.sin(theta);
        termxy[0][j] = limbxy[0][j];   // need a second copy later
        termxy[1][j] = limbxy[1][j] * cos_sunmoon;
      }

      // rotate to appropriate position angle

      double pa = o.w.cusppa + (Const.PI) / 2.;   // cusppa already in rad.
      double[][] turnmoon = {{Math.cos(pa), Math.sin(pa)},
        {-1. * Math.sin(pa), Math.cos(pa)}};
      for (j = 0; j < nseg; j++) {
        for (i = 0; i < 2; i++) {
          limbxypr[i][j] = 0.;
          termxypr[i][j] = 0.;
          for (k = 0; k < 2; k++) {
            limbxypr[i][j] += turnmoon[i][k] * limbxy[k][j];
            termxypr[i][j] += turnmoon[i][k] * termxy[k][j];
          }
        }
      }

      double zover2 = (90d - o.w.altmoon) / (Const.DEG_IN_RADIAN * 2.);
      double coszover2 = Math.cos(zover2);
      double moonsize = 3. * coszover2;

      double rafac = 15. * Math.cos(o.w.moon.topopos.delta.radians());

      double[] ralimb = new double[nseg];
      double[] declimb = new double[nseg];
      double[] raterm = new double[nseg];
      double[] decterm = new double[nseg];

      double racent = o.w.moon.topopos.alpha.value;  // saving verbosity
      double deccent = o.w.moon.topopos.delta.value;

      // double moonsize = 3.;

      for (i = 0; i < nseg; i++) {
        ralimb[i] = o.w.sidereal - // Hereafter actually HA, not RA
                (racent + limbxypr[0][i] * moonsize / rafac);
        declimb[i] = deccent + limbxypr[1][i] * moonsize;
        raterm[i] = o.w.sidereal
                - (racent + termxypr[0][i] * moonsize / rafac);
        decterm[i] = deccent + termxypr[1][i] * moonsize;
      }

      double coslat = Math.cos(o.w.where.lat.radians());
      double sinlat = Math.sin(o.w.where.lat.radians());

      GeneralPath limbpath = new GeneralPath();
      GeneralPath termpath = new GeneralPath();

      double[] xy = SkyProject(ralimb[0], declimb[0], coslat, sinlat);
      double[] xypix = xytopix(xy[0], xy[1]);
      limbpath.moveTo((float) xypix[0], (float) xypix[1]);

      xy = SkyProject(raterm[0], decterm[0], coslat, sinlat);
      xypix = xytopix(xy[0], xy[1]);
      termpath.moveTo((float) xypix[0], (float) xypix[1]);

//          for(i = 1; i < 11; i++) {
//
//             xy = SkyProject(ralimb[i],declimb[i],coslat,sinlat);
//             xypix = xytopix(xy[0],xy[1]);
//             limbpath.lineTo((float) xypix[0], (float) xypix[1]);
//
//             xy = SkyProject(raterm[i],decterm[i],coslat,sinlat);
//             xypix = xytopix(xy[0],xy[1]);
//             termpath.lineTo((float) xypix[0], (float) xypix[1]);
//
//          }

      // try rendering with quadratic curves

      double[] xy2 = {0., 0.};
      double[] xypix2 = {0., 0.};

      for (i = 1; i < nseg - 1; i = i + 2) {

        xy = SkyProject(ralimb[i], declimb[i], coslat, sinlat);
        xypix = xytopix(xy[0], xy[1]);
        xy2 = SkyProject(ralimb[i + 1], declimb[i + 1], coslat, sinlat);
        xypix2 = xytopix(xy2[0], xy2[1]);
        limbpath.quadTo((float) xypix[0], (float) xypix[1],
                (float) xypix2[0], (float) xypix2[1]);

        xy = SkyProject(raterm[i], decterm[i], coslat, sinlat);
        xypix = xytopix(xy[0], xy[1]);
        xy2 = SkyProject(raterm[i + 1], decterm[i + 1], coslat, sinlat);
        xypix2 = xytopix(xy2[0], xy2[1]);
        termpath.quadTo((float) xypix[0], (float) xypix[1],
                (float) xypix2[0], (float) xypix2[1]);

      }

      g2.setPaint(brightyellow);

      g2.draw(limbpath);
      g2.draw(termpath);

    }

    void plotplanets(Graphics2D g2) {
      int i;
      double xy[] = {0., 0.};
      double xypix[] = {0., 0.};
      double ha, dec, lst;
      double magconst1 = 0.003, magslope = 0.002;  //

      p.computeMags();

      lst = o.w.sidereal;
      double coslat = Math.cos(o.w.where.lat.radians());
      double sinlat = Math.sin(o.w.where.lat.radians());

      for (i = 0; i < 9; i++) {
        if (i != 2) {
          xy = SkyProject(lst - p.PlanetObs[i].c.alpha.value,
                  p.PlanetObs[i].c.delta.value, coslat, sinlat);
          if (p.mags[i] < 4.7) {
            putDot(g2, xy[0], xy[1], magconst1 + magslope * (4.5 - p.mags[i]), Color.YELLOW);
          } else {
            putDot(g2, xy[0], xy[1], magconst1, Color.YELLOW);
          }
          puttext(g2, xy[0], xy[1] + 0.01, p.names[i], smallfont, smallfontmetrics);
        }
      }
    }

    void plotobjects(Graphics2D g2) {  // plots out the objects on the user list.
      int i;
      double xy[] = {0., 0.};
      double xypix[] = {0., 0.};
      double ha, dec, lst;
      double magconst1 = 0.003, magslope = 0.002;  //
      AstrObj obj;
      Celest cel;
      double currenteq = o.w.when.julianEpoch();

      double coslat = Math.cos(o.w.where.lat.radians());
      double sinlat = Math.sin(o.w.where.lat.radians());
      lst = o.w.sidereal;
      g2.setPaint(Color.GREEN);

      for (i = 0; i < presenterKey.size(); i++) {
        // System.out.printf("i %d RASelectors[i] = %s, name = \n",i,RASelectors[i]);
        obj = presenterKey.get(RASelectors[i]);
        if (!obj.name.equals("null")) {
          cel = obj.c.precessed(currenteq);
          xy = SkyProject(lst - cel.alpha.value,
                  cel.delta.value, coslat, sinlat);
          puttext(g2, xy[0], xy[1], obj.name, smallfont, smallfontmetrics);
        }
      }
    }

    void zoom(int xpixin, int ypixin, double zoomfac) {
      // user hands in pixel coordinates around which to zoom.

      double[] xycent = pixtoxy(xpixin, ypixin);
      xmid = xycent[0];
      ymid = xycent[1];
      halfwidthy /= zoomfac;
      halfwidthx /= zoomfac;
      pixperunit *= zoomfac;
      zoomedby *= zoomfac;
      currentlat = o.w.where.lat.value;
      makeHAgrid(currentlat);
      repaint();
    }

    void pan(int xpixin, int ypixin) {
      // user hands in pixel coordinates around which to zoom.

      double[] xycent = pixtoxy(xpixin, ypixin);
      xmid = xycent[0];
      ymid = xycent[1];
      currentlat = o.w.where.lat.value;
      makeHAgrid(currentlat);
      repaint();
    }

    void restorefull() {
      xmid = 0.;
      ymid = 0.;
      halfwidthx = halfwidthxfull;
      halfwidthy = halfwidthyfull;
      pixperunit = pixperunitfull;
      zoomedby = 1.;
      currentlat = o.w.where.lat.value;
      makeHAgrid(currentlat);
      repaint();
    }

    class tinyHelp extends JWindow {

      final JTextArea area;

      tinyHelp() {
        area = new JTextArea();
        area.append("Middle button or 'c'  - sets to coords\n");
        area.append("Left button or 's' - sets to nearest listed obj\n");
        area.append("'f'  - step time Forward \n");
        area.append("'b'  - step time Backward \n");
        area.append("'z'  - Zoom in at mouse position\n");
        area.append("'o'  - zoom Out at mouse position\n");
        area.append("'p'  - Pan, recenter at mouse position \n");
        area.append("'r'  - Restore original scaling \n");
        area.append("'h'  - Set to nearest HR star.\n");
        area.append("'q' or 'x'  - Hide display window.\n");
        area.append("Keys don't respond unless window has focus.\n");
        area.setBackground(brightyellow);
        this.setSize(275, 150);
//               this.setSize(300,220);
        this.add(area);
      }

      void show(int x, int y) {
        this.setLocation(x, y);
        this.setVisible(true);
      }

      void hide(int x, int y) {  // giving it arguments avoids an override
        // problem.
        this.setVisible(false);
      }
    }
  }

  class AirmassDisplay extends JFrame
          //       implements MouseMotionListener,
          implements MouseListener {

    int xpix, ypix;             // window size
    double xwidth, yheight;      // same, but double
    double xlobord, xhibord, ylobord, yhibord;   // user coords of extreme edge
    double xlo, xhi, ylo, yhi;      // user coordinates of frame
    double endfade, startfade;      // beginning and end of "twilight" for fade
    double xvplo, xvphi, yvplo, yvphi;  // borders of frame as fraction of viewport
    double jdstart, jdend;
    WhenWhere w;
    Font smallfont;
    FontMetrics smallfontmetrics;
    Font mediumfont;
    FontMetrics mediumfontmetrics;
    Font largefont;
    FontMetrics largefontmetrics;
    MouseEvent mousevent;
    Color[] objcolors = {Color.RED, Color.GREEN, Color.CYAN, Color.MAGENTA};

    AirmassDisplay(int xpixin, int ypixin) {  // constructed after Nightly has been updated ...
      super("Airmass Display");

      // LBO: use SetPreferredSize and inherit Frame to get correct window size
      final Dimension initialDim = new Dimension(xpixin, ypixin + 35);
      setMinimumSize(initialDim);
      setPreferredSize(initialDim);

      onResize(initialDim);

      ylo = 3.2;
      yhi = 0.9;
      xvplo = 0.08;
      xvphi = 0.88;
      yvplo = 0.15;
      yvphi = 0.90;

      smallfont = new Font("Dialog", Font.PLAIN, 11);
      mediumfont = new Font("Dialog", Font.PLAIN, 15);

      setFocusable(true);
      //  addMouseMotionListener(this);
      addMouseListener(this);

      addComponentListener(new ComponentAdapter() {
        @Override
        public void componentResized(final ComponentEvent e) {
          final Component c = e.getComponent();
          final Dimension d = c.getSize();

          onResize(new Dimension(d.width, d.height));
        }
      });

      Update();
    }

    /**
     * LBO: avoid excessive repaints
     * @param visible 
     */
    @Override
    public void setVisible(final boolean visible) {
      if (visible) {
        if (airmasswindowvisible) {
          return;
        }
      } else {
        if (!airmasswindowvisible) {
          return;
        }
      }
      airmasswindowvisible = visible;
      super.setVisible(visible);
    }

    /**
     * LBO fixed window resize
     * @param dim new dimension
     */
    private void onResize(final Dimension dim) {
      // update internal fields:
      xpix = dim.width;
      ypix = dim.height;

      xwidth = (double) dim.width;
      yheight = (double) dim.height;
      repaint();
    }

    void Update() {
      double timespan;

      w = (WhenWhere) Nightly.sunset.clone();
      jdstart = w.when.jd;
      xlo = w.when.UTDate.timeofday.value;

      jdend = Nightly.sunrise.when.jd;
      timespan = (jdend - jdstart) * 24.;
      // System.out.printf("timespan = %f hrs\n",timespan);
      xhi = xlo + timespan;
      endfade = xlo + (Nightly.eveningTwilight18.when.jd - jdstart) * 24.;
      //System.out.printf("xlo %f dt %f Endfade = %f\n",xlo,
      //      (Nightly.eveningTwilight18.when.jd - jdstart) * 24.,endfade);
      startfade = xlo + (Nightly.morningTwilight18.when.jd - jdstart) * 24.;

      double span = xhi - xlo;
      double fracx = xvphi - xvplo;
      xlobord = xlo - xvplo * span / fracx;
      xhibord = xhi + (1 - xvphi) * span / fracx;

      span = yhi - ylo;
      double fracy = yvphi - yvplo;
      ylobord = ylo - yvplo * span / fracy;
      yhibord = yhi + (1 - yvphi) * span / fracy;

      // System.out.printf("start %f end %f\n",jdstart, jdend);
      repaint();
    }

    public void mouseClicked(MouseEvent e) {
      // New feature -- set time to the time clicked in the AirmassDisplay window
      mousevent = e;
      double mousetimeofday = xlobord + ((float) mousevent.getX() / (float) xpix) * (xhibord - xlobord);
      double since_xlo = mousetimeofday - xlo;
      double jdmouse = jdstart + since_xlo / 24.;
      JDfield.setText(String.format(Locale.ENGLISH, "%15.6f", jdmouse));
      setToJD();
      // System.out.printf("x %d y %d mousetimeofday %f since_xlo %f jdmouse %f\n",
      //    mousevent.getX(),mousevent.getY(),mousetimeofday,since_xlo,jdmouse);

    }
    // need to define all the rest of the MouseEvent interface of course ...

    public void mousePressed(MouseEvent e) {
    }

    public void mouseReleased(MouseEvent e) {
    }

    public void mouseEntered(MouseEvent e) {
    }

    public void mouseExited(MouseEvent e) {
    }

    public void paint(Graphics g) {
      Graphics2D g2 = (Graphics2D) g;
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
      g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);

      int i, j;
      double xtick;
      double[] xy1 = {0., 0.};
      double[] xy2 = {0., 0.};
      double[] xy3 = {0., 0.};
      double[] xy4 = {0., 0.};
      double jd, ut, local;

      Observation omoon = (Observation) o.clone();

      Stroke defStroke = g2.getStroke();   // store the default stroke.
      Color gridcolor = new Color(153, 0, 0);  // dark red

      float[] dot1 = {1.0f, 4.0f};
      BasicStroke dotted = new BasicStroke(0.3f, BasicStroke.CAP_BUTT,
              BasicStroke.JOIN_MITER, 1.0f, dot1, 0.0f);

      smallfontmetrics = g2.getFontMetrics(smallfont);
      mediumfontmetrics = g2.getFontMetrics(mediumfont);

      g2.setBackground(Color.BLACK);
      g2.clearRect(0, 0, xpix, ypix);

      //Color dayColor = new Color(70,130,180);
      // Color dayColor = new Color(60,80,180);
      Color dayColor = new Color(60, 105, 190);
      Color deepTwilight = new Color(10, 20, 30);   // faint bluish-grey
      // Color deepTwilight = new Color(40,40,40);   // Brighter grey for testing.

      g2.setPaint(dayColor);                     // left bdy = day color
      xy1 = xytopix(xlo, yhi);
      // g2.fill(new Rectangle2D.Double(0.,0.,xy1[0],yheight));

      xy2 = xytopix(endfade, yhi);
      // System.out.printf("xy1 %f %f  xy2 %f %f\n",xy1[0],xy1[1],xy2[0],xy2[1]);

      GradientPaint daytonight = new GradientPaint((int) xy1[0], 0, dayColor,
              (int) xy2[0], 0, deepTwilight);
      g2.setPaint(daytonight);
      xy2 = xytopix(endfade, ylo);
      //g2.fill(new Rectangle2D.Double(xy1[0],0.,
      //                              xy2[0],yheight));
      g2.fill(new Rectangle2D.Double(xy1[0], xy1[1],
              xy2[0], xy2[1]));

      xy1 = xytopix(startfade, yhi);   // beginning of twilight

      g2.setPaint(Color.BLACK);       // have to overpaint the black ...
      g2.fill(new Rectangle2D.Double(xy2[0], 0., xy1[0], yheight));

      xy2 = xytopix(xhi, yhi);
      GradientPaint nighttoday = new GradientPaint((int) xy1[0], 0, deepTwilight, (int) xy2[0], 0, dayColor);
      g2.setPaint(nighttoday);
      xy2 = xytopix(xhi, ylo);
      g2.fill(new Rectangle2D.Double(xy1[0], xy1[1], xy2[0], xy2[1]));

      g2.setPaint(Color.BLACK);   // for some reason have to overpaint this ...
      g2.fill(new Rectangle2D.Double(xy2[0], 0., xwidth, yheight));

      // overpaint the bottom, too ...
      xy1 = xytopix(xlo, ylo);
      g2.fill(new Rectangle2D.Double(0, (int) xy1[1], xpix, ypix));

      g2.setPaint(Color.WHITE);

      g2.setStroke(new BasicStroke(1.0f));
      // draw the box
      drawline(g2, xlo, yhi, xhi, yhi);
      drawline(g2, xlo, yhi, xlo, ylo);
      drawline(g2, xlo, ylo, xhi, ylo);
      drawline(g2, xhi, ylo, xhi, yhi);

      double span = xhi - xlo;

      xy1 = xytopix(xlo - 0.07 * span, (ylo + yhi) / 2.);
      g2.rotate(-1. * Const.PI / 2., xy1[0], xy1[1]);
      puttext(g2, xlo - 0.07 * span, (ylo + yhi) / 2., "Airmass", mediumfont, mediumfontmetrics);
      // g2.transform(saveXform);
      g2.rotate(Const.PI / 2, xy1[0], xy1[1]);

      span = yhi - ylo;
      double ytoplabel = yhi + 0.02 * span;
      double ybottomlabel = ylo - 0.05 * span;
      double ybanner = yhi + 0.07 * span;

      span = xhi - xlo;
      double ticklen = (yhi - ylo) / 80.;
      double minordiv = 0.25; // hard code divisions, sorry ...

      puttext(g2, xlo - span * 0.04, ytoplabel, "UT:", mediumfont, mediumfontmetrics);
      puttext(g2, xlo - span * 0.05, ybottomlabel, "Local:", mediumfont, mediumfontmetrics);

      w.changeWhen(jdstart);

      String banner = String.format("%s - Evening date %s",
              w.where.name, w.when.localDate.roundedCalString(6, 0));
      puttext(g2, (xlo + xhi) / 2., ybanner, banner, mediumfont, mediumfontmetrics);

      xtick = (double) ((int) xlo + 1);
      double xminortick;

      for (j = 1; j < 4; j++) {  // fill in start ..
        xminortick = xtick - j * minordiv;
        if (xminortick > xlo) {
          drawline(g2, xminortick, yhi, xminortick, yhi - ticklen / 2.);
          drawline(g2, xminortick, ylo, xminortick, ylo + ticklen / 2.);
        }
      }

      while (xtick < xhi) {
        w.changeWhen(jdstart + (xtick - xlo) / 24.);
        ut = w.when.UTDate.timeofday.value;
        local = w.when.localDate.timeofday.value;
        puttext(g2, xtick, ytoplabel, String.format("%02.0f", ut),
                mediumfont, mediumfontmetrics);
        puttext(g2, xtick, ybottomlabel, String.format("%02.0f", local),
                mediumfont, mediumfontmetrics);

        drawline(g2, xtick, yhi, xtick, yhi - ticklen);
        drawline(g2, xtick, ylo, xtick, ylo + ticklen);
        g2.setStroke(dotted);
        g2.setPaint(gridcolor);
        drawline(g2, xtick, yhi, xtick, ylo);
        g2.setStroke(defStroke);
        g2.setPaint(Color.WHITE);
        for (j = 1; j < 4; j++) {
          xminortick = xtick + j * minordiv;
          if (xminortick < xhi) {
            drawline(g2, xminortick, yhi, xminortick, yhi - ticklen / 2.);
            drawline(g2, xminortick, ylo, xminortick, ylo + ticklen / 2.);
          }
        }

        omoon.w.changeWhen(jdstart + (xtick - xlo) / 24.);
        omoon.computeSky();
        omoon.computeSunMoon();
        if (omoon.w.altmoon > 0.) {
          plotmoon(g2);
        }

        xtick += 1.;
      }

      // Draw a vertical line at current time ...

      // System.out.printf("o.w.when.jd = %f, jdstart = %f, ",o.w.when.jd,jdstart);
      double hrs_since = xlo + (o.w.when.jd - jdstart) * 24.;
      // System.out.printf("hrs_since = %f\n",hrs_since);

      if (hrs_since > xlo && hrs_since < xhi) {
        Color nowColor = new Color(128, 128, 128); // grey
        // g2.setStroke(dotted);
        g2.setPaint(nowColor);
        drawline(g2, hrs_since, yhi, hrs_since, ylo);
        // g2.setStroke(defStroke);
        g2.setPaint(Color.WHITE);
      }


      double yticklen = (xhi - xlo) / 120.;
      double yminortick;
      double ytick = 1.0;
      double labelx = xlo - (xhi - xlo) / 30.;
      double ylabeloffset = (yhi - ylo) / 60.;
      while (ytick < ylo) {
        drawline(g2, xlo, ytick, xlo + yticklen, ytick);
        drawline(g2, xhi, ytick, xhi - yticklen, ytick);
        puttext(g2, labelx, ytick - ylabeloffset, String.format("%3.1f", ytick),
                mediumfont, mediumfontmetrics);
        g2.setStroke(dotted);
        g2.setPaint(gridcolor);
        drawline(g2, xlo, ytick, xhi, ytick);
        g2.setStroke(defStroke);
        g2.setPaint(Color.WHITE);
        for (j = 1; j < 5; j++) {
          yminortick = ytick + j * 0.1;
          if (yminortick < ylo) {
            drawline(g2, xlo, yminortick, xlo + yticklen / 2., yminortick);
            drawline(g2, xhi, yminortick, xhi - yticklen / 2., yminortick);
          }
        }
        ytick = ytick + 0.5;
      }

      // draw a separate vertical axis for moon altitude, off to the side.
      // scale is not the same as for the airmass axes.

      g2.setPaint(Color.YELLOW);
      span = xhi - xlo;
      double xmoonaxis = xhi + span * 0.04;
      drawline(g2, xmoonaxis, ylo, xmoonaxis, yhi);
      double tickstep = (ylo - yhi) / 9.;  // 10 degrees
      for (i = 0; i < 10; i++) {
        ytick = yhi + i * tickstep;
        drawline(g2, xmoonaxis - yticklen, ytick, xmoonaxis, ytick);
        if (i % 3 == 0) {
          puttext(g2, xmoonaxis + span / 40., ytick + 0.05, String.format("%d", (9 - i) * 10),
                  mediumfont, mediumfontmetrics);
        }
      }

      double xmoonlabel = xmoonaxis + span * 0.05;
      xy1 = xytopix(xmoonlabel, (ylo + yhi) / 2.);
      g2.rotate(Const.PI / 2, xy1[0], xy1[1]);
      puttext(g2, xmoonlabel, (ylo + yhi) / 2., "Moon Altitude [deg]", mediumfont,
              mediumfontmetrics);
      g2.rotate(-1. * Const.PI / 2, xy1[0], xy1[1]);

      g2.setPaint(Color.WHITE);

      plotAirmass(g2, o.w, o.c, " ", Color.WHITE);  // white = main window object.

      if (airmassPlotSelections.length > 0 && airmassPlotSelections[0] != null) {
        String sel;
        int cindex = 0;
        for (i = 0; i < airmassPlotSelections.length; i++) {
          sel = (String) airmassPlotSelections[i];
          plotAirmass(g2, o.w, presenterKey.get(sel).c, sel, objcolors[cindex]);
          cindex++;
          if (cindex == objcolors.length) {
            cindex = 0;  // cycle colors
          }
        }
      }
    }

    void drawline(Graphics2D g2, double x1, double y1, double x2, double y2) {  // user coords
      double[] xy1;
      double[] xy2;
      xy1 = xytopix(x1, y1);
      xy2 = xytopix(x2, y2);
      //     System.out.printf("x2 y2 %f %f xy2 %f %f\n",x2,y2,xy2[0],xy2[1]);
      g2.draw(new Line2D.Double(xy1[0], xy1[1], xy2[0], xy2[1]));
    }

    double[] xytopix(double x, double y) {
      double[] retvals = {0., 0.};
      retvals[0] = xpix * (x - xlobord) / (xhibord - xlobord);
      retvals[1] = ypix * (1. - (y - ylobord) / (yhibord - ylobord));
      return (retvals);
    }

//       double [] pixtoxy(double pixx, double pixy) {
//            double [] retvals = {0.,0.};
//            retvals[0] = xpix * (x - xlobord) / (xhibord - xlobord);
//            retvals[1] = ypix * (1. - (y - ylobord) / (yhibord - ylobord));
//            return(retvals);
//       }
    void puttext(Graphics2D g2, double x, double y, String text, Font font, FontMetrics metrics) {
      double[] xy;
      // write text centered left-to-right on user coords x and y.
      xy = xytopix(x, y);

      g2.setFont(font);
      int adv = metrics.stringWidth(text);
      g2.drawString(text, (int) xy[0] - adv / 2, (int) xy[1]);
    }

    void plotAirmass(Graphics2D g2, WhenWhere wIn, Celest cIn, String objectname, Color objcolor) {

      g2.setPaint(objcolor);

      Observation oairm = new Observation((WhenWhere) wIn.clone(),
              (Celest) cIn.clone());
      // System.out.printf("%s\n",oairm.c.alpha.roundedRAString(2,":"));
      double jd = jdstart;
      double dt = 0.005;
      double[] xy1 = {0., 0.};

      float[] dot1 = {2.0f};
      BasicStroke dotted = new BasicStroke(1.0f, BasicStroke.CAP_BUTT,
              BasicStroke.JOIN_MITER, 3.0f, dot1, 0.0f);

      GeneralPath airpath = new GeneralPath();
      GeneralPath airpath2 = new GeneralPath();
//          GeneralPath airpath3 = new GeneralPath();
//          GeneralPath airpath4 = new GeneralPath();

      Stroke defStroke = g2.getStroke();

      boolean firstptfound = false;
      boolean uptwice = false;
      double xpl = 0.;

      while (!firstptfound && jd < jdend) {
        oairm.w.changeWhen(jd);
        oairm.computeSky();
        // System.out.printf("%s %f\n",oairm.w.when.UTDate.roundedCalString(0,0),oairm.airmass);
        if (oairm.airmass < ylo && oairm.airmass > 0.99 && Math.abs(oairm.ha.value) < 6.) {
          // System.out.printf("firstptfound, airmass %f\n",oairm.airmass);
          firstptfound = true;
          xpl = xlo + (jd - jdstart) * 24.;
          xy1 = xytopix(xpl, oairm.airmass);
          airpath.moveTo((float) xy1[0], (float) xy1[1]);
        }
        jd += dt;
      }
      // System.out.printf("Out, airmass %f, jd %f\n",oairm.airmass,jd);
      while (oairm.airmass < ylo && oairm.airmass > 0.99 && jd < jdend && Math.abs(oairm.ha.value) < 6.) {
        // System.out.printf("plt - %s %f\n",oairm.w.when.UTDate.roundedCalString(0,0),oairm.airmass);
        oairm.w.changeWhen(jd);
        oairm.computeSky();
        xpl = xlo + (jd - jdstart) * 24.;
        xy1 = xytopix(xpl, oairm.airmass);
        if (oairm.airmass > 0.99 && oairm.airmass < ylo && Math.abs(oairm.ha.value) < 6.) {   // don't follow it past max airm
          //if(Math.abs(oairm.ha.value) > 6.) g2.setStroke(dotted);
          //else g2.setStroke(defStroke);
          airpath.lineTo((float) xy1[0], (float) xy1[1]);
        }
        jd += dt;
      }


      if (firstptfound) {  // label tracks near their ends ....
        if (jd > jdend) {
          puttext(g2, xpl - 0.5, oairm.airmass, objectname,
                  smallfont, smallfontmetrics);
        } else if (oairm.airmass > ylo) {
          puttext(g2, xpl + 0.2, ylo - 0.05, objectname,
                  smallfont, smallfontmetrics);
        } else {
          puttext(g2, xpl + 0.2, oairm.airmass, objectname, smallfont,
                  smallfontmetrics);
        }
      }

      // objects can come up a second time ...

      firstptfound = false;
      while (!firstptfound && jd < jdend) {
        oairm.w.changeWhen(jd);
        oairm.computeSky();
        if (oairm.airmass < ylo && oairm.airmass > 0.99 && Math.abs(oairm.ha.value) < 6.) {
          firstptfound = true;
          uptwice = true;
          xpl = xlo + (jd - jdstart) * 24.;
          xy1 = xytopix(xpl, oairm.airmass);
          airpath2.moveTo((float) xy1[0], (float) xy1[1]);
        }
        jd += dt;
      }
      while (oairm.airmass < ylo && oairm.airmass > 0.99 && jd < jdend && Math.abs(oairm.ha.value) < 6.) {
        oairm.w.changeWhen(jd);
        oairm.computeSky();
        xpl = xlo + (jd - jdstart) * 24.;
        xy1 = xytopix(xpl, oairm.airmass);
        if (oairm.airmass > 0.99 && oairm.airmass < ylo) {   // don't follow it past 3.5
          //if(Math.abs(oairm.ha.value) > 6.) g2.setStroke(dotted);
          //else g2.setStroke(defStroke);
          airpath2.lineTo((float) xy1[0], (float) xy1[1]);
        }
        jd += dt;
      }
      // now we're sure to have it all.
      g2.draw(airpath);
      if (uptwice) {
        g2.draw(airpath2);  // second part, if there is one.
      }
      g2.setPaint(Color.WHITE);
    }

    void plotmoon(Graphics2D g2) {

      Observation omoon = new Observation(w, w.moon.topopos);
      omoon.computeSky();
      omoon.computeSunMoon();

      int nseg = 21;

      double[][] limbxy = new double[2][nseg];    // in system where cusp axis = y
      double[][] termxy = new double[2][nseg];

      double[][] limbxypr = new double[2][nseg];  // in system where North = y
      double[][] termxypr = new double[2][nseg];

      double theta;
      double cos_sunmoon;
      int i, j, k;

      // set up points describing terminator and limb -- then rotate into
      // position.
      cos_sunmoon = Math.cos(omoon.w.sunmoon / Const.DEG_IN_RADIAN);
      // System.out.printf("sunmoon %f cos_sunmoon %f\n",o.w.sunmoon,cos_sunmoon);
      double dtheta = Const.PI / (double) (nseg - 1);
      for (j = 0; j < nseg; j++) {
        theta = dtheta * (double) j;
        limbxy[0][j] = Math.cos(theta);
        limbxy[1][j] = Math.sin(theta);
        termxy[0][j] = limbxy[0][j];   // need a second copy later
        termxy[1][j] = limbxy[1][j] * cos_sunmoon;
      }

      // rotate to appropriate position angle

//          double pa = w.cusppa + (Const.PI)/2. +
      //                    omoon.parallactic / Const.DEG_IN_RADIAN;
//          double pa = w.cusppa -
      //                   omoon.parallactic / Const.DEG_IN_RADIAN;
      double pa = omoon.w.cusppa - (Const.PI) / 2.;
      // cusppa already in rad.
      if (w.where.lat.value < 0.) {
        pa += Const.PI;  // flip in S.
      }
      double[][] turnmoon = {{Math.cos(pa), Math.sin(pa)},
        {-1. * Math.sin(pa), Math.cos(pa)}};
      for (j = 0; j < nseg; j++) {
        for (i = 0; i < 2; i++) {
          limbxypr[i][j] = 0.;
          termxypr[i][j] = 0.;
          for (k = 0; k < 2; k++) {
            limbxypr[i][j] += turnmoon[i][k] * limbxy[k][j];
            termxypr[i][j] += turnmoon[i][k] * termxy[k][j];
          }
        }
      }

      double ycent = ylo + (omoon.w.altmoon / 90d) * (yhi - ylo);  // scale ...
      double xcent = xlo + (omoon.w.when.jd - jdstart) * 24.;
      double[] xypix = xytopix(xcent, ycent);
//          System.out.printf("jd %f xcent ycent %f %f altmoon %f \n",
//                          o.w.when.jd,xcent,ycent,omoon.w.altmoon);
//          System.out.printf(" -> pix %f %f\n",xypix[0],xypix[1]);
      double moonsizeinpix = 8.;  // radius
      // double moonsize = 3.;

      GeneralPath limbpath = new GeneralPath();
      GeneralPath termpath = new GeneralPath();

      limbpath.moveTo((float) (xypix[0] + limbxypr[0][0] * moonsizeinpix),
              (float) (xypix[1] + limbxypr[1][0] * moonsizeinpix));

      termpath.moveTo((float) (xypix[0] + termxypr[0][0] * moonsizeinpix),
              (float) (xypix[1] + termxypr[1][0] * moonsizeinpix));

      double x1, y1, x2, y2;

      for (i = 1; i < nseg - 1; i = i + 2) {
        x1 = xypix[0] + termxypr[0][i] * moonsizeinpix;
        y1 = xypix[1] + termxypr[1][i] * moonsizeinpix;
        x2 = xypix[0] + termxypr[0][i + 1] * moonsizeinpix;
        y2 = xypix[1] + termxypr[1][i + 1] * moonsizeinpix;
        termpath.quadTo((float) x1, (float) y1, (float) x2, (float) y2);

        x1 = xypix[0] + limbxypr[0][i] * moonsizeinpix;
        y1 = xypix[1] + limbxypr[1][i] * moonsizeinpix;
        x2 = xypix[0] + limbxypr[0][i + 1] * moonsizeinpix;
        y2 = xypix[1] + limbxypr[1][i + 1] * moonsizeinpix;
        limbpath.quadTo((float) x1, (float) y1, (float) x2, (float) y2);

      }

      g2.setPaint(brightyellow);
      g2.draw(limbpath);
      g2.draw(termpath);
      g2.setPaint(Color.WHITE);
    }
  }
}

class filewriter {
  // a little class to make it easy to open and close an outfile.

  File outfile;
  PrintWriter pw = null;
  FileWriter fw = null;

  filewriter(String fname) {
    outfile = new File(fname);
    fw = null;
    try {
      fw = new FileWriter(outfile, true);
    } catch (IOException e) {
      System.out.printf("File writer didn't open for %s.\n", fname);
    }
    pw = new PrintWriter(fw);

  }

  void closer() {
    try {
      fw.close();
    } catch (IOException e) {
      System.out.printf("File writer didn't close.\n");
    }
  }
}

class FileGrabber extends JFrame {
// similar utility class for opening an infile, pops a file chooser.

  File infile = null;
  BufferedReader br = null;
  FileReader fr = null;

  FileGrabber() {
    JFileChooser chooser = new JFileChooser();
    int result = chooser.showOpenDialog(this);
    if (result != JFileChooser.CANCEL_OPTION) {
      try {
        infile = chooser.getSelectedFile();
        br = null;
        fr = new FileReader(infile);
      } catch (Exception e) {
        System.out.println("File opening error of some kind.");
      }
      if (fr != null) {
        br = new BufferedReader(fr);
      }
    }
  }

  void closer() {
    try {
      if (br != null) {
        br.close();
        br = null;
        fr = null;
      }
      if (fr != null) {
        fr.close();
        fr = null;
      }
    } catch (IOException e) {
      System.out.println("File reader didn't close.");
    }
  }
}

class AstrObj {
  // simple class for storing info from coord files ...

  String name;
  Celest c;

  // String comment;   // may be added later ...
  // double pmx, pmy;
  AstrObj(String instuff) {
    String[] fields;
    instuff = instuff.replace(":", " ");
    fields = instuff.split("\\s+");  // whitespace separated.
    name = fields[0];
    try {
      String raf = fields[1] + " " + fields[2] + " " + fields[3];
      String decf = fields[4] + " " + fields[5] + " " + fields[6];
      //  System.out.printf("raf %s decf %s\n",raf,decf);
      c = new Celest(raf, decf, fields[7]);
    } catch (Exception e) {
      System.out.printf("Unconvertable input in AstrObj ... %s\n", instuff);
    }
  }

  AstrObj(String sIn, Celest cIn) {
    name = sIn;
    c = cIn;
  }
}

class BrightStar {
  // A class for storing info from bright star list.  These are to be read in from
  // brightest.dat, which is a file of the 518 stars with m < 4.

  String name;
  Celest c;
  double m;   // apparent magnitude.
  Color col;  // color used for plotting the star, based on spectral type.

  BrightStar(String instuff) {
    // constructor reads from a file with lines like:
    // 3.158278  44.85722  3.80 " 27Kap Per"
    // try {
    String[] fields;
    String[] fields2;
    try {
      fields = instuff.split("\"");   // split out the quoted name
      name = fields[1];
      fields[0] = fields[0].trim();
      fields2 = fields[0].split("\\s+");  // and the rest
      c = new Celest(Double.parseDouble(fields2[0]), Double.parseDouble(fields2[1]), 2000d);
      m = Double.parseDouble(fields2[2]);
      col = new Color(Integer.parseInt(fields2[3]), Integer.parseInt(fields2[4]),
              Integer.parseInt(fields2[5]));
    } catch (Exception e) {
      System.out.printf("Unreadable line in bright star file input: %s\n", instuff);
    }
  }
  // time to rationalize the precession in the display map ... which means we need:
}
