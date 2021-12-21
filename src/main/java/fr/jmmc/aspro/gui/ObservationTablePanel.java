/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

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
import fr.jmmc.jmcs.gui.util.AutofitTableColumns;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.model.TableEditorPanel;
import fr.jmmc.jmcs.service.BrowserLauncher;
import fr.jmmc.jmcs.util.NumberUtils;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.swing.AbstractCellEditor;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.border.Border;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellEditor;
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

        // Use the internal cell renderer:
        final EnhancedTableCellRenderer rdr = new EnhancedTableCellRenderer();
        final EnhancedTableCellEditor editor = new EnhancedTableCellEditor();
        // match all types:
        jTableObs.setDefaultRenderer(String.class, rdr);
        jTableObs.setDefaultEditor(String.class, editor);
        jTableObs.setDefaultRenderer(Number.class, rdr);
        jTableObs.setDefaultEditor(Number.class, editor);

        // load user preference for columns:
        obsTableSorter.setVisibleColumnNames(Preferences.getInstance().getObsTableVisibleColumns());

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
                        obsModel.getColumnNames(), prevVisibleColumns, // default columns (button reset)
                        new ArrayList<>(0), newVisibleColumns, // resulting columns
                        TABLE_EDITOR_DIMENSION_KEY
                );

                if (newVisibleColumns != null) {
                    // Update visible columns if needed:
                    if (!prevVisibleColumns.equals(newVisibleColumns)) {
                        setVisibleColumnNames(newVisibleColumns);
                    }
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

    private void filterData() {
        List<TargetObservation> filtered = null;

        if (obsTargetList != null) {
            // TODO: filter observations (and targets)
            // no filter:
            filtered = obsTargetList;
        }
        obsModel.setData(filtered);
    }

    private boolean hasURL(final int column) {
        return obsModel.hasURL(column);
    }

    private String getURL(final int column, final int row) {
        return obsModel.getURL(column, row);
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

    /**
     * Used to render cells.
     *
     * @warning: No trace log implemented as this is very often called (performance).
     */
    private final class EnhancedTableCellRenderer extends DefaultTableCellRenderer {

        /** default serial UID for Serializable interface */
        private static final long serialVersionUID = 1;
        /* members */
        /** derived italic font */
        private Font _derivedItalicFont = null;
        /** orange border for selected cell */
        private final Border _orangeBorder = BorderFactory.createLineBorder(Color.ORANGE, 2);
        /** internal string buffer */
        final StringBuilder _buffer = new StringBuilder(128);

        /**
         * TableCellColors  -  Constructor
         */
        EnhancedTableCellRenderer() {
            super();
        }

        /**
         * Sets the <code>String</code> object for the cell being rendered to
         * <code>value</code>.
         *
         * @param value  the string value for this cell; if value is
         *          <code>null</code> it sets the text value to an empty string
         * @see JLabel#setText
         *
         */
        @Override
        public void setValue(final Object value) {
            String text = "";
            if (value != null) {
                if (value instanceof Double) {
                    final double val = ((Double) value).doubleValue();
                    if (!Double.isNaN(val)) {
                        text = NumberUtils.format(val);
                    }
                } else if (value instanceof Boolean) {
                    text = ((Boolean) value).booleanValue() ? "True" : "False";
                } else {
                    text = value.toString();
                }
            }
            setText(text);
        }

        /**
         * getTableCellRendererComponent  -  return the component with renderer (Table)
         * @param table JTable
         * @param value Object
         * @param isSelected boolean
         * @param hasFocus boolean
         * @param row int
         * @param column int
         * @return Component
         */
        @Override
        public Component getTableCellRendererComponent(final JTable table, final Object value,
                                                       final boolean isSelected, final boolean hasFocus,
                                                       final int row, final int column) {

            // Set default renderer to the component
            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

            // always use right alignment:
            setHorizontalAlignment(JLabel.RIGHT);

            final int colIndex = table.convertColumnIndexToModel(column);

            if (colIndex == -1) {
                logger.warn("Error searching in the table model while trying to render cell "
                        + "at column {} table.getColumnCount() = {}", column, table.getColumnCount());
                return this;
            }

            // Do not change color if cell is located on a selected row
            if (table.isRowSelected(row)) {
                // Except if it is the selected cell itself (to highlight found tokens)
                if (table.isColumnSelected(column)) {
                    setBorder(_orangeBorder);
                    setBackground(Color.YELLOW);
                    setForeground(Color.BLACK);

                    // Put the corresponding row font in italic:
                    if (_derivedItalicFont == null) {
                        // cache derived Font:
                        final Font cellFont = getFont();
                        _derivedItalicFont = cellFont.deriveFont(cellFont.getStyle() | Font.ITALIC);
                    }
                    setFont(_derivedItalicFont);
                }

                return this;
            }

            final StringBuilder sb = _buffer;
            final int modelColumn = obsTableSorter.columnModelIndex(colIndex);

            // Compose catalog URL
            if (value != null && hasURL(modelColumn)) {
                setText(sb.append("<html><a href='#empty'>").append(value).append("</a></html>").toString());
                sb.setLength(0); // recycle buffer
            }

            Color foregroundColor = Color.BLACK;
            Color backgroundColor = Color.WHITE;

            // If cell is not selected and not focused 
            if (!(isSelected && hasFocus)) {
                // Apply colors
                setForeground(foregroundColor);
                setBackground(backgroundColor);
                setBorder(noFocusBorder);
            }

            // Return the component
            return this;
        }
    }

    private final class EnhancedTableCellEditor extends AbstractCellEditor implements TableCellEditor {

        /** default serial UID for Serializable interface */
        private static final long serialVersionUID = 1;

        // This method is called when a cell value is edited by the user.
        @Override
        public Component getTableCellEditorComponent(final JTable table, final Object value, final boolean isSelected,
                                                     final int row, final int column) {
            // If the cell is empty
            if (value == null) {
                return null; // Exit
            }

            // Retrieve clicked cell informations
            final int colIndex = table.convertColumnIndexToModel(column);

            if (colIndex == -1) {
                logger.warn("Error searching in the table model while trying to render cell "
                        + "at column {} table.getColumnCount() = {}", column, table.getColumnCount());
                return null;
            }

            final int modelColumn = obsTableSorter.columnModelIndex(colIndex);

            if (hasURL(modelColumn)) {
                final int modelRow = obsTableSorter.modelIndex(row);
                final String url = getURL(modelColumn, modelRow);

                if (url != null) {
                    if (logger.isDebugEnabled()) {
                        logger.debug("User clicked, will open '{}' in default browser.", url);
                    }

                    // Open web browser with the computed URL
                    BrowserLauncher.openURL(url);
                }
            }

            // Return null to "cancel" editing
            return null;
        }

        // This method is called when editing is completed.
        // It must return the new value to be stored in the cell.
        @Override
        public Object getCellEditorValue() {
            // Should not be called
            logger.error("TableCellColorsEditor.getCellEditorValue() should have not been called.");
            return null;
        }
    }

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
        // invoke Bootstrapper method to initialize logback now:
        Bootstrapper.getState();

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
