/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.Aspro2;
import fr.jmmc.aspro.Preferences;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.event.ObservationEvent;
import fr.jmmc.aspro.model.event.ObservationListener;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
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
import java.util.Collections;
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
public final class TargetTablePanel extends javax.swing.JPanel implements BasicTableColumnMovedListener, ObservationListener {

    private static final long serialVersionUID = 1L;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(TargetTablePanel.class.getName());

    /** Table key to remember dialog dimensions */
    private final static String TABLE_EDITOR_DIMENSION_KEY = "___ASPRO2_TARGET_TABLE_EDITOR_DIMENSION";

    /* members */
    private List<Target> targetList = null;
    private TargetUserInformations targetUserInfos = null;

    private final TargetTableModel targetModel;
    private final BasicTableSorter targetTableSorter;

    /** Creates new form ObservationTablePanel */
    public TargetTablePanel() {
        this.targetModel = new TargetTableModel();

        initComponents();

        // Configure table sorting
        targetTableSorter = new BasicTableSorter(targetModel, jTableTarget.getTableHeader());

        // Process the listeners last to first, so register before jtable, not after:
        targetTableSorter.addTableModelListener(new TableModelListener() {

            @Override
            public void tableChanged(final TableModelEvent e) {
                // If the table structure has changed, reapply the custom renderer/editor on columns + auto-fit
                if ((e.getSource() != targetTableSorter)
                        || (e.getFirstRow() == TableModelEvent.HEADER_ROW)) {

                    AutofitTableColumns.autoResizeTable(jTableTarget, true, true);
                }
            }
        });
        targetTableSorter.setTableHeaderChangeListener(this);

        jTableTarget.setModel(targetTableSorter);

        // Fix row height:
        SwingUtils.adjustRowHeight(jTableTarget);

        // Use enhanced cell renderer/editor:
        final EnhancedTableCellRenderer rdr = new EnhancedTableCellRenderer(targetTableSorter);
        final EnhancedTableCellEditor editor = new EnhancedTableCellEditor(targetTableSorter);

        // match all types:
        jTableTarget.setDefaultRenderer(String.class, rdr);
        jTableTarget.setDefaultEditor(String.class, editor);
        jTableTarget.setDefaultRenderer(Number.class, rdr);
        jTableTarget.setDefaultEditor(Number.class, editor);

        // load user preference for columns:
        final List<String> prefVisibleColumnNames = Collections.emptyList(); //Preferences.getInstance().getObsTableVisibleColumns();
        targetTableSorter.setVisibleColumnNames(!prefVisibleColumnNames.isEmpty() ? prefVisibleColumnNames : targetModel.getColumnNames());

        // Decorate scrollpane corner:
        final JButton cornerButton = new JButton();
        cornerButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                final List<String> prevVisibleColumns = targetTableSorter.getVisibleColumnNames();

                // show the table editor dialog to select displayed columns:
                final List<String> newVisibleColumns = new ArrayList<>();
                TableEditorPanel.showEditor(targetModel.getColumnNames(), prevVisibleColumns, // initial columns 
                        targetModel.getColumnNames(), targetModel.getColumnNames(), // default columns (button reset)
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

        targetTableSorter.setVisibleColumnNames(visibleColumnNames);
        // save preference after resultSetTableSorter updated:
        updateVisibleColumnsPreferences();
    }

    private void updateVisibleColumnsPreferences() {
        final List<String> visibleColumnNames = targetTableSorter.getVisibleColumnNames();
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
            case TARGET_CHANGED:
                setData(event.getObservation());
                break;
            default:
        }
        if (logger.isDebugEnabled()) {
            logger.debug("event [{}] process OUT", event.getType());
        }
    }

    public TargetTablePanel setData(final ObservationSetting observation) {
        this.targetList = observation.getTargets();
        this.targetUserInfos = observation.getOrCreateTargetUserInfos();
        filterData();
        return this;
    }

    public void resetFilters() {
        // TODO
    }

    private void filterData() {
        List<Target> filtered = null;

        if (targetList != null) {
            // TODO: filter targets
            // no filter:
            filtered = targetList;
        }
        targetModel.setData(filtered, this.targetUserInfos);
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
        jTableTarget = new javax.swing.JTable();

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

        jTableTarget.setModel(targetModel);
        jTableTarget.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jTableTarget.setMinimumSize(new java.awt.Dimension(50, 50));
        jTableTarget.setName("jTableTarget"); // NOI18N
        jScrollPaneTable.setViewportView(jTableTarget);

        add(jScrollPaneTable, java.awt.BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel jLabelFilterBy;
    private javax.swing.JPanel jPanelHeader;
    private javax.swing.JPanel jPanelHeaderFiller;
    private javax.swing.JScrollPane jScrollPaneTable;
    private javax.swing.JTable jTableTarget;
    // End of variables declaration//GEN-END:variables

    // --- TEST ---
    private static void showFrameData(final int frameCloseOperation, final String title,
                                      final ObservationSetting observation) {
        final JFrame frame = new JFrame(title);
        frame.setDefaultCloseOperation(frameCloseOperation);

        frame.setMinimumSize(new Dimension(800, 800)); // test

        frame.add(new TargetTablePanel().setData(observation));
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

                    showFrameData(JFrame.EXIT_ON_CLOSE, "Observations", observation);

                } catch (IOException ioe) {
                    logger.error("IO exception occured:", ioe);
                }
            }
        });
    }

}
