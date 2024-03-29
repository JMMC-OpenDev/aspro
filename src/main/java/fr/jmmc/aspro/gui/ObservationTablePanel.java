/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.Aspro2;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.event.ObservationEvent;
import fr.jmmc.aspro.model.event.ObservationListener;
import fr.jmmc.aspro.model.oi.ObservationCollection;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.util.TargetObservation;
import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.gui.component.BasicTableColumnMovedListener;
import fr.jmmc.jmcs.gui.component.BasicTableSorter;
import fr.jmmc.jmcs.gui.component.EnhancedTableCellEditor;
import fr.jmmc.jmcs.gui.component.EnhancedTableCellRenderer;
import fr.jmmc.jmcs.gui.util.AutofitTableColumns;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.model.TableEditorPanel;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author bourgesl
 */
public final class ObservationTablePanel extends javax.swing.JPanel implements BasicTableColumnMovedListener, ObservationListener {

    private static final long serialVersionUID = 1L;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(ObservationTablePanel.class.getName());

    /** Table key to remember dialog dimensions */
    private final static String TABLE_EDITOR_DIMENSION_KEY = "___ASPRO2_OBS_TABLE_EDITOR_DIMENSION";

    /* members */
    private List<TargetObservation> obsTargetList = null;
    private final ObservationTableModel obsModel;
    private final BasicTableSorter obsTableSorter;

    /** Creates new form ObservationTablePanel */
    public ObservationTablePanel() {
        this.obsModel = new ObservationTableModel();

        initComponents();

        // Configure table sorting
        obsTableSorter = new BasicTableSorter(obsModel, jTableObs.getTableHeader());

        // Process the listeners last to first, so register before jtable, not after:
        obsTableSorter.addTableModelListener(new TableModelListener() {

            @Override
            public void tableChanged(final TableModelEvent e) {
                // If the table structure has changed, reapply the custom renderer/editor on columns + auto-fit
                if ((e.getSource() != obsTableSorter)
                        || (e.getFirstRow() == TableModelEvent.HEADER_ROW)) {

                    AutofitTableColumns.autoResizeTable(jTableObs, false, true);
                }
            }
        });
        obsTableSorter.setTableHeaderChangeListener(this);

        jTableObs.setModel(obsTableSorter);

        // Fix row height:
        SwingUtils.adjustRowHeight(jTableObs);

        // Use enhanced cell renderer/editor:
        final EnhancedTableCellRenderer rdr = new EnhancedTableCellRenderer(obsTableSorter);
        final EnhancedTableCellEditor editor = new EnhancedTableCellEditor(obsTableSorter);

        // match all types:
        jTableObs.setDefaultRenderer(String.class, rdr);
        jTableObs.setDefaultEditor(String.class, editor);
        jTableObs.setDefaultRenderer(Number.class, rdr);
        jTableObs.setDefaultEditor(Number.class, editor);

        // load user preference for columns:
        final List<String> prefVisibleColumnNames = Preferences.getInstance().getObsTableVisibleColumns();
        obsTableSorter.setVisibleColumnNames(
                !prefVisibleColumnNames.isEmpty() ? prefVisibleColumnNames : obsModel.getColumnNames()
        );

        // Decorate scrollpane corner:
        final JButton cornerButton = new JButton();
        cornerButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final List<String> prevVisibleColumns = obsTableSorter.getVisibleColumnNames();

                // show the table editor dialog to select displayed columns:
                final List<String> newVisibleColumns = new ArrayList<>();
                TableEditorPanel.showEditor(
                        obsModel.getColumnNames(), prevVisibleColumns, // initial columns 
                        obsModel.getColumnNames(), obsModel.getColumnNames(), // default columns (button reset)
                        new ArrayList<>(0), newVisibleColumns, // resulting columns
                        TABLE_EDITOR_DIMENSION_KEY
                );

                // Update visible columns if needed:
                if (!prevVisibleColumns.equals(newVisibleColumns)) {
                    setVisibleColumnNames(newVisibleColumns);
                }
            }
        });
        jScrollPaneTable.setCorner(JScrollPane.LOWER_RIGHT_CORNER, cornerButton);
    }

    @Override
    public void tableColumnMoved(BasicTableSorter source) {
        // save preference after resultSetTableSorter updated:
        updateVisibleColumnsPreferences();
    }

    /** 
     * modify the user selected columns in BasicTableSorter
     * Used when Table Editor dialog returns and we must apply the user choices.
     * @param visibleColumnNames the new list of columns selected by user.
     */
    private void setVisibleColumnNames(final List<String> visibleColumnNames) {
        logger.debug("setVisibleColumnNames: {}", visibleColumnNames);

        obsTableSorter.setVisibleColumnNames(visibleColumnNames);
        // save preference after resultSetTableSorter updated:
        updateVisibleColumnsPreferences();
    }

    private void updateVisibleColumnsPreferences() {
        final List<String> visibleColumnNames = obsTableSorter.getVisibleColumnNames();
        logger.debug("updateVisibleColumnsPreferences: {}", visibleColumnNames);
        Preferences.getInstance().setObsTableVisibleColumns(visibleColumnNames);
    }

    /**
     * Handle the given event on the given observation = 
     * add the missing plot panels
     * 
     * @param event event
     */
    @Override
    public void onProcess(final ObservationEvent event) {

        if (logger.isDebugEnabled()) {
            logger.debug("event [{}] process IN", event.getType());
        }
        switch (event.getType()) {
            case REFRESH:
            case REFRESH_UV:
                setData(prepareObsTargetList(event.getObservationCollection()));
                break;
            // TODO: use observaebilityDone event to get obsData too ?
            default:
        }
        if (logger.isDebugEnabled()) {
            logger.debug("event [{}] process OUT", event.getType());
        }
    }

    public ObservationTablePanel setData(final List<TargetObservation> obsTargetList) {
        this.obsTargetList = obsTargetList;
        filterData();
        return this;
    }

    public void resetFilters() {

    }

    private void filterData() {
        List<TargetObservation> filtered = null;

        if (obsTargetList != null) {
            // TODO: filter observations (and targets)
            // no filter:
            filtered = obsTargetList;
        }
        obsModel.setData(filtered);
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    @SuppressWarnings("unchecked")
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {
        java.awt.GridBagConstraints gridBagConstraints;

        jPanelHeader = new javax.swing.JPanel();
        jLabelFilterBy = new javax.swing.JLabel();
        jPanelHeaderFiller = new javax.swing.JPanel();
        jScrollPaneTable = new javax.swing.JScrollPane();
        jTableObs = new javax.swing.JTable();

        setName("Form"); // NOI18N
        setLayout(new java.awt.BorderLayout());

        jPanelHeader.setName("jPanelHeader"); // NOI18N
        jPanelHeader.setLayout(new java.awt.GridBagLayout());

        jLabelFilterBy.setText("Filter by: General filters, coming soon ...");
        jLabelFilterBy.setName("jLabelFilterBy"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelHeader.add(jLabelFilterBy, gridBagConstraints);

        jPanelHeaderFiller.setName("jPanelHeaderFiller"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        gridBagConstraints.weightx = 0.9;
        jPanelHeader.add(jPanelHeaderFiller, gridBagConstraints);

        add(jPanelHeader, java.awt.BorderLayout.NORTH);

        jScrollPaneTable.setHorizontalScrollBarPolicy(javax.swing.ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS);
        jScrollPaneTable.setVerticalScrollBarPolicy(javax.swing.ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
        jScrollPaneTable.setAutoscrolls(true);
        jScrollPaneTable.setName("jScrollPaneTable"); // NOI18N
        jScrollPaneTable.setPreferredSize(new java.awt.Dimension(100, 100));
        jScrollPaneTable.setViewportView(null);

        jTableObs.setModel(obsModel);
        jTableObs.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jTableObs.setMinimumSize(new java.awt.Dimension(50, 50));
        jTableObs.setName("jTableObs"); // NOI18N
        jScrollPaneTable.setViewportView(jTableObs);

        add(jScrollPaneTable, java.awt.BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel jLabelFilterBy;
    private javax.swing.JPanel jPanelHeader;
    private javax.swing.JPanel jPanelHeaderFiller;
    private javax.swing.JScrollPane jScrollPaneTable;
    private javax.swing.JTable jTableObs;
    // End of variables declaration//GEN-END:variables

    // --- TEST ---
    private static void showFrameData(final int frameCloseOperation, final String title,
                                      final List<TargetObservation> obsTargetList) {
        final JFrame frame = new JFrame(title);
        frame.setDefaultCloseOperation(frameCloseOperation);

        frame.setMinimumSize(new Dimension(800, 800)); // test

        frame.add(new ObservationTablePanel().setData(obsTargetList));
        frame.pack();
        frame.setVisible(true);
    }

    public static void main(String[] args) {
        // Start application without GUI:
        Bootstrapper.launchApp(new Aspro2(args), true, true, false);

        final File file = new File("/home/bourgesl/ASPRO2/obsportal_targets.asprox");

        SwingUtils.invokeLaterEDT(new Runnable() {
            /**
             * Show the application frame using EDT
             */
            @Override
            public void run() {
                try {
                    ObservationManager.getInstance().load(file);
                    final ObservationSetting observation = ObservationManager.getInstance().getMainObservation();

                    showFrameData(JFrame.EXIT_ON_CLOSE, "Observations", prepareObsTargetList(observation));

                } catch (IOException ioe) {
                    logger.error("IO exception occured:", ioe);
                }
            }
        });
    }

    private static List<TargetObservation> prepareObsTargetList(final ObservationSetting observation) {
        if (observation == null) {
            return null;
        }
        // Mimic ObservationManager.getInstance().getObservationCollection():
        final List<TargetObservation> obsTargetList = new ArrayList<TargetObservation>();
        //for (ObservationSetting observation : obs collection)
        for (Target target : observation.getDisplayTargets()) {
            obsTargetList.add(new TargetObservation(observation, target));
        }
        return obsTargetList;
    }

    private static List<TargetObservation> prepareObsTargetList(final ObservationCollection obsCollection) {
        if (obsCollection == null) {
            return null;
        }
        // Mimic ObservationManager.getInstance().getObservationCollection():
        final List<TargetObservation> obsTargetList = new ArrayList<TargetObservation>();
        for (ObservationSetting observation : obsCollection.getObservations()) {
            for (Target target : observation.getDisplayTargets()) {
                obsTargetList.add(new TargetObservation(observation, target));
            }
        }
        return obsTargetList;
    }

}
