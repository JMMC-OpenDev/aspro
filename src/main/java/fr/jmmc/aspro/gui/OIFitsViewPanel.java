/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.gui.action.ExportOIFitsAction;
import fr.jmmc.aspro.model.OIFitsData;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.event.OIFitsEvent;
import fr.jmmc.aspro.model.event.ObservationEvent;
import fr.jmmc.aspro.model.event.ObservationListener;
import fr.jmmc.jmcs.gui.component.Disposable;
import fr.jmmc.oiexplorer.core.export.DocumentExportable;
import fr.jmmc.oiexplorer.core.export.DocumentOptions;
import fr.jmmc.oiexplorer.core.gui.PlotChartPanel;
import fr.jmmc.oiexplorer.core.gui.PlotDefinitionEditor;
import fr.jmmc.oiexplorer.core.gui.PlotView;
import fr.jmmc.oiexplorer.core.gui.action.ExportDocumentAction;
import fr.jmmc.oiexplorer.core.model.OIFitsCollectionManager;
import fr.jmmc.oiexplorer.core.model.PlotDefinitionFactory;
import fr.jmmc.oiexplorer.core.model.oi.SubsetDefinition;
import fr.jmmc.oiexplorer.core.model.plot.ColorMapping;
import fr.jmmc.oiexplorer.core.model.plot.PlotDefinition;
import fr.jmmc.oitools.model.OIDataListHelper;
import fr.jmmc.oitools.model.OIFitsFile;
import java.awt.BorderLayout;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import org.jfree.chart.ChartColor;
import org.jfree.chart.ui.Drawable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This panel embeds the OIFitsExplorer into Aspro2
 * @author bourgesl
 */
public final class OIFitsViewPanel extends javax.swing.JPanel implements Disposable, ObservationListener, DocumentExportable {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(OIFitsViewPanel.class.getName());

    static {
        // Disable the expression editor until it is ready for production:
        PlotDefinitionEditor.setEnableExpressionEditor(false);

        // Hack to always show flagged data (error undefined):
        for (PlotDefinition plotDef : PlotDefinitionFactory.getInstance().getDefaults()) {
            plotDef.setSkipFlaggedData(false);
        }
    }
    /* members */
    /** OIFitsCollectionManager singleton */
    private transient final OIFitsCollectionManager ocm = OIFitsCollectionManager.getInstance();
    /** chart data */
    private transient OIFitsData oiFitsData = null;
    /** Oifits explorer Plot view */
    private PlotView plotView;
    /** Oifits explorer Plot chart panel */
    private PlotChartPanel plotChartPanel;

    /**
     * Constructor
     */
    public OIFitsViewPanel() {
        initComponents();

        postInit();
    }

    /**
     * Free any ressource or reference to this instance:
     * remove the plot chart panel from OIFitsCollectionManager event notifiers 
     * and this instance from ObservationManager listeners
     * 
     * @see PlotChartPanel#dispose() 
     */
    @Override
    public void dispose() {
        // forward dispose() to child components:
        if (plotView != null) {
            plotView.dispose();
        }

        // unregister the OIFits viewer panel for the next event :
        ObservationManager.getInstance().unregister(this);
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

        jLabelMessage = new javax.swing.JLabel();

        setBackground(new java.awt.Color(255, 255, 255));
        setLayout(new java.awt.BorderLayout());

        jLabelMessage.setHorizontalAlignment(javax.swing.SwingConstants.CENTER);
        jLabelMessage.setText("LABEL");
        jLabelMessage.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        add(jLabelMessage, java.awt.BorderLayout.NORTH);
    }// </editor-fold>//GEN-END:initComponents

    /**
     * Export the component as a document using the given action:
     * the component should check if there is something to export ?
     * @param action export action to perform the export action
     */
    @Override
    public void performAction(final ExportDocumentAction action) {
        if (getOIFitsData() == null) {
            return;
        }
        final List<OIFitsFile> oiFitsFiles = getOIFitsData().checkAndGetOIFitsList();

        if (oiFitsFiles != null && oiFitsFiles != OIFitsData.IGNORE_OIFITS_LIST) {
            action.process(this);
        }
    }

    /**
     * Return the default file name
     * @param fileExtension  document's file extension
     * @return default file name
     */
    @Override
    public String getDefaultFileName(final String fileExtension) {
        return this.plotChartPanel.getDefaultFileName(fileExtension);
    }

    /**
     * Prepare the page layout before doing the export:
     * Performs layout and modifies the given options
     * @param options document options used to prepare the document
     */
    @Override
    public void prepareExport(final DocumentOptions options) {
        this.plotChartPanel.prepareExport(options);
    }

    /**
     * Return the page to export given its page index
     * @param pageIndex page index (1..n)
     * @return Drawable array to export on this page
     */
    @Override
    public Drawable[] preparePage(final int pageIndex) {
        return this.plotChartPanel.preparePage(pageIndex);
    }

    /**
     * Callback indicating the export is done to reset the component's state
     */
    @Override
    public void postExport() {
        this.plotChartPanel.postExport();
    }

    /**
     * This method is useful to set the models and specific features of initialized swing components :
     */
    private void postInit() {
        ocm.start();

        this.jLabelMessage.setForeground(ChartColor.DARK_RED);

        // define which plot to use:
        final String plotId = OIFitsCollectionManager.CURRENT_VIEW;

        this.plotView = new PlotView(plotId);
        this.plotView.setName("plotView"); // FEST

        this.plotChartPanel = this.plotView.getPlotPanel();
        this.plotChartPanel.setName("plotChartPanel"); // FEST

        add(this.plotView, BorderLayout.CENTER);
    }

    /**
     * Handle the changed event to plot the new OIFits data synchronously.
     * @param event event
     */
    @Override
    public void onProcess(final ObservationEvent event) {
        if (logger.isDebugEnabled()) {
            logger.debug("event [{}] process IN", event.getType());
        }
        switch (event.getType()) {
            case OIFITS_DONE:
                if (event instanceof OIFitsEvent) {
                    this.plot(((OIFitsEvent) event).getOIFitsData());
                }
                break;
            default:
        }
        if (logger.isDebugEnabled()) {
            logger.debug("event [{}] process OUT", event.getType());
        }
    }

    /**
     * Return the OIFits data
     * @return OIFits data
     */
    private OIFitsData getOIFitsData() {
        return this.oiFitsData;
    }

    /**
     * Define the OIFits data
     * @param oiFitsData OIFits data
     */
    private void setOIFitsData(final OIFitsData oiFitsData) {
        this.oiFitsData = oiFitsData;
    }

    /**
     * Plot OIFits data using embedded OIFitsExplorer Plot panel
     * This code must be executed by the Swing Event Dispatcher thread (EDT)
     * @param oiFitsData OIFits data
     */
    private void plot(final OIFitsData oiFitsData) {
        logger.debug("plot : {}", oiFitsData);

        // memorize chart data (used by export PDF):
        setOIFitsData(oiFitsData);

        if (oiFitsData == null) {
            this.jLabelMessage.setText("No OIFits data available: the target is not observable or has no model.");

            display(false, true);

            // reset:
            ocm.reset();

        } else {
            final List<OIFitsFile> oiFitsList = oiFitsData.getOIFitsList();
            final List<String> warnMags = oiFitsData.getWarningMissingMags();

            if (warnMags != null) {
                final StringBuilder sb = new StringBuilder(256);
                sb.append("<html>");
                for (String msg : warnMags) {
                    sb.append(msg).append("<br>");
                }
                sb.append("</html>");

                this.jLabelMessage.setText(sb.toString());
                display(true, true);
            } else {
                display(true, false);
            }

            // Fix file paths ie generate file names ?
            for (OIFitsFile oiFitsFile : oiFitsList) {
                oiFitsFile.setAbsoluteFilePath(ExportOIFitsAction.getDefaultFileName(oiFitsFile));
            }

            // remove all oifits files:
            ocm.removeAllOIFitsFiles();

            for (OIFitsFile oiFitsFile : oiFitsList) {
                ocm.addOIFitsFile(oiFitsFile);
            }

            // get current subset definition (copy):
            final SubsetDefinition subsetCopy = ocm.getCurrentSubsetDefinition();

            // display OIFITS data for the selected target (SCI):
            subsetCopy.getFilter().setTargetUID(oiFitsData.getTargetName());
            // use all data files (default):
            subsetCopy.getFilter().getTables().clear();

            // fire subset changed event (generates OIFitsSubset and then plot asynchronously):
            ocm.updateSubsetDefinition(this, subsetCopy);

            // Get unique configurations:
            final Set<String> distinctStaConfs = new LinkedHashSet<String>(4);
            for (OIFitsFile oiFitsFile : oiFitsList) {
                distinctStaConfs.addAll(OIDataListHelper.getDistinctStaConfs(oiFitsFile.getOiDataList())); // requires analysis
            }
            logger.debug("distinctStaConfs: {}", distinctStaConfs);

            if (distinctStaConfs.size() > 1) {
                // change the plot definition to force color mapping in multi conf:
                final PlotDefinition plotDefCopy = ocm.getCurrentPlotDefinition();

                // use configuration color mapping if useful then wavelength mapping:
                plotDefCopy.setColorMapping(ColorMapping.CONFIGURATION);

                ocm.updatePlotDefinition(this, plotDefCopy);
            }
        }
    }

    /**
     * Show message or plot
     * @param showPlot flag indicating to show the plot view
     * @param showMessage flag indicating to show the message label
     */
    private void display(final boolean showPlot, final boolean showMessage) {
        this.jLabelMessage.setVisible(showMessage);
        this.plotView.setVisible(showPlot);
    }

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel jLabelMessage;
    // End of variables declaration//GEN-END:variables
}
