/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.model.event.OIFitsEvent;
import fr.jmmc.aspro.model.event.ObservationEvent;
import fr.jmmc.aspro.model.event.ObservationListener;
import fr.jmmc.aspro.util.XmlFactory;
import fr.jmmc.oitools.model.OIFitsFile;
import fr.jmmc.oitools.model.XmlOutputVisitor;
import java.io.IOException;
import java.io.StringReader;
import java.util.logging.Level;
import javax.swing.JEditorPane;
import javax.swing.text.Document;
import javax.swing.text.html.HTMLEditorKit;

/**
 * This panel presents a simple HTML representation of the current OIFits file
 * @author bourgesl
 */
public final class OIFitsPanel extends javax.swing.JPanel implements ObservationListener {

  /** default serial UID for Serializable interface */
  private static final long serialVersionUID = 1;
  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.gui.OIFitsPanel";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** XSLT file path */
  private final static String XSLT_FILE = "fr/jmmc/aspro/gui/oiview.xsl";
  /** empty document */
  private static Document emptyDocument = null;

  /* member */
  /** flag to know if the last document was empty to avoid JEditorPane refresh calls */
  private boolean isEmpty = false;

  /** Creates new form OIFitsPanel */
  public OIFitsPanel() {
    initComponents();
  }

  /**
   * Handle the given event on the given observation =
   * If the oifits is computed, refresh the UI widgets
   * @param event event
   */
  public void onProcess(final ObservationEvent event) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("event [" + event.getType() + "] process IN");
    }
    switch (event.getType()) {
      case REFRESH:
        // reset content :
        this.updateOIFits(null);
        break;
      case OIFITS_DONE:
        this.updateOIFits(((OIFitsEvent) event).getOIFitsFile());
        break;
      default:
    }
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("event [" + event.getType() + "] process OUT");
    }
  }

  /**
   * Update widgets with the computed OIFits structure
   * @param oiFitsFile computed OIFits structure
   */
  private void updateOIFits(final OIFitsFile oiFitsFile) {
    String document = "";

    if (oiFitsFile != null) {
      final long start = System.nanoTime();

      document = XmlOutputVisitor.getXmlDesc(oiFitsFile, true, true);

      // use an XSLT to transform the XML document to an HTML representation :
      document = XmlFactory.transform(document, XSLT_FILE);

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("update : " + 1e-6d * (System.nanoTime() - start) + " ms.");
      }
    }

    if (document.length() > 0) {

      final long start = System.nanoTime();
      try {
        this.jOutputPane.read(new StringReader(document), null);
        this.jOutputPane.setCaretPosition(0);
      } catch (IOException ioe) {
        logger.log(Level.SEVERE, "IO exception : ", ioe);
      }

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("html : " + 1e-6d * (System.nanoTime() - start) + " ms.");
      }

      this.isEmpty = false;
    } else {
      if (!isEmpty) {
        // reset content when the observation changed :
        this.jOutputPane.setDocument(emptyDocument);
        this.isEmpty = true;
      }
    }
  }

  /**
   * This method is called from within the constructor to
   * initialize the form.
   * WARNING: Do NOT modify this code. The content of this method is
   * always regenerated by the Form Editor.
   */
  @SuppressWarnings("unchecked")
  // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
  private void initComponents() {

    jScrollPane = new javax.swing.JScrollPane();
    jOutputPane = createEditorPane();

    setLayout(new java.awt.BorderLayout());

    jScrollPane.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
    jScrollPane.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    jScrollPane.setPreferredSize(new java.awt.Dimension(300, 300));
    jScrollPane.setViewportView(jOutputPane);

    add(jScrollPane, java.awt.BorderLayout.CENTER);
  }// </editor-fold>//GEN-END:initComponents
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JEditorPane jOutputPane;
  private javax.swing.JScrollPane jScrollPane;
  // End of variables declaration//GEN-END:variables

  /**
   * Create an html editor pane
   * @return html editor pane
   */
  private static JEditorPane createEditorPane() {
    final JEditorPane pane = new JEditorPane();

    // add a HTMLEditorKit to the editor pane
    pane.setEditorKit(new HTMLEditorKit());

    pane.setContentType("text/html");
    pane.setEditable(false);

    emptyDocument = pane.getEditorKit().createDefaultDocument();

    return pane;
  }
}
