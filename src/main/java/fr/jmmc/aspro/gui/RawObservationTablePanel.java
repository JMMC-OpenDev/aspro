/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.aspro.model.ObservationManager;
import fr.jmmc.aspro.model.oi.ObservationSetting;
import fr.jmmc.aspro.model.oi.TargetRawObservation;
import fr.jmmc.aspro.model.rawobs.RawObservation;
import fr.jmmc.jmcs.Bootstrapper;
import fr.jmmc.jmcs.gui.component.BasicTableSorter;
import fr.jmmc.jmcs.gui.util.AutofitTableColumns;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import fr.jmmc.jmcs.service.BrowserLauncher;
import fr.jmmc.jmcs.util.NumberUtils;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.swing.AbstractCellEditor;
import javax.swing.BorderFactory;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;
import javax.swing.border.Border;
import javax.swing.event.ListSelectionEvent;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author bourgesl
 */
public final class RawObservationTablePanel extends javax.swing.JPanel {

    private static final long serialVersionUID = 1L;
    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(RawObservationTablePanel.class.getName());

    /* members */
    private List<RawObservation> observations = null;
    private final RawObservationTableModel obsModel;
    private final BasicTableSorter obsTableSorter;

    /** Creates new form RawObservationTablePanel */
    public RawObservationTablePanel() {
        this.obsModel = new RawObservationTableModel();

        initComponents();

        final EnhancedTableCellRenderer rdr = new EnhancedTableCellRenderer();
        final EnhancedTableCellEditor editor = new EnhancedTableCellEditor();

        // Configure table sorting
        obsTableSorter = new BasicTableSorter(obsModel, jTableObs.getTableHeader());
        jTableObs.setModel(obsTableSorter);

        // Fix row height:
        SwingUtils.adjustRowHeight(jTableObs);

        // Use the internal cell renderer:
        final TableColumnModel columnModel = jTableObs.getColumnModel();
        for (int i = 0, len = columnModel.getColumnCount(); i < len; i++) {
            final TableColumn tc = columnModel.getColumn(i);
            tc.setCellRenderer(rdr);
            tc.setCellEditor(editor);
        }

        this.jListIns.setModel(new DefaultComboBoxModel(AsproConstants.INS_OBS_LIST));
    }

    public void resetFilters() {
        this.jListIns.clearSelection();
    }
    
    public RawObservationTablePanel setData(final List<RawObservation> observations) {
        this.observations = observations;
        filterData();
        return this;
    }

    private void filterData() {
        List<RawObservation> filtered = null;

        if (observations != null) {
            // filter observations:
            final List<String> rawObsFilterInsNames = ObservationManager.getInstance().getMainObservation().getRawObsFilterInsNames();

            logger.info("filter instruments: {}", rawObsFilterInsNames);

            if (rawObsFilterInsNames == null) {
                // no filter:
                filtered = observations;
            } else {
                final int len = observations.size();
                filtered = new ArrayList<RawObservation>(len);

                for (int i = 0; i < len; i++) {
                    final RawObservation rawObs = observations.get(i);

                    if (!rawObsFilterInsNames.contains(rawObs.getInstrumentName())) {
                        logger.info("skip rawObs instrument: {}", rawObs.getInstrumentName());
                        continue;
                    }
                    // TODO: other filters (chain)
                    filtered.add(rawObs);
                }
            }
        }

        obsModel.setData(filtered);

        if (filtered != null) {
            AutofitTableColumns.autoResizeTable(jTableObs);
        }
    }

    private boolean hasURL(final int column) {
        return obsModel.hasURL(column);
    }

    private String getURL(final int column, final int row) {
        return obsModel.getURL(column, row);
    }

    /**
     * Called whenever the instrument selection changes.
     * @param e the event that characterizes the change.
     */
    private void processInstrumentsValueChanged(final ListSelectionEvent e) {
        // skip events when the user selection is adjusting :
        if (e.getValueIsAdjusting()) {
            return;
        }

        final List<String> selectedInstruments = getSelectedInstruments();

        logger.info("Selected instruments: {}", selectedInstruments);

        final ObservationManager om = ObservationManager.getInstance();

        // Set filters:
        om.getMainObservation().setRawObsFilterInsNames(selectedInstruments);

        // filter table:
        filterData();

        om.fireTargetObservationsChangedEvents();
    }

    /**
     * Return the currently selected instruments
     * @return instrument list
     */
    public List<String> getSelectedInstruments() {
        final ListSelectionModel lsm = jListIns.getSelectionModel();

        final int iMin = lsm.getMinSelectionIndex();
        final int iMax = lsm.getMaxSelectionIndex();

        if ((iMin < 0) || (iMax < 0)) {
            return null;
        }

        final ListModel model = jListIns.getModel();
        final List<String> selectedItems = new ArrayList<String>(iMax - iMin);
        for (int i = iMin; i <= iMax; i++) {
            if (lsm.isSelectedIndex(i)) {
                selectedItems.add((String) model.getElementAt(i));
            }
        }
        return selectedItems;
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
        jScrollPaneIns = new javax.swing.JScrollPane();
        jListIns = new javax.swing.JList<>();
        jPanelHeaderFiller = new javax.swing.JPanel();
        jScrollPaneTable = new javax.swing.JScrollPane();
        jTableObs = new javax.swing.JTable();

        setName("Form"); // NOI18N
        setLayout(new java.awt.BorderLayout());

        jPanelHeader.setName("jPanelHeader"); // NOI18N
        jPanelHeader.setLayout(new java.awt.GridBagLayout());

        jLabelFilterBy.setText("Filter by:");
        jLabelFilterBy.setName("jLabelFilterBy"); // NOI18N
        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.insets = new java.awt.Insets(2, 2, 2, 2);
        jPanelHeader.add(jLabelFilterBy, gridBagConstraints);

        jScrollPaneIns.setName("jScrollPaneIns"); // NOI18N

        jListIns.setModel(new javax.swing.AbstractListModel<String>() {
            String[] strings = { "_MATISSE_" };
            public int getSize() { return strings.length; }
            public String getElementAt(int i) { return strings[i]; }
        });
        jListIns.setToolTipText("Select 1 or more instruments to filter raw observations (use Ctrl + click)");
        jListIns.setLayoutOrientation(javax.swing.JList.HORIZONTAL_WRAP);
        jListIns.setName("jListIns"); // NOI18N
        jListIns.setPrototypeCellValue("_MATISSE_");
        jListIns.setVisibleRowCount(1);
        jListIns.addListSelectionListener(new javax.swing.event.ListSelectionListener() {
            public void valueChanged(javax.swing.event.ListSelectionEvent evt) {
                jListInsValueChanged(evt);
            }
        });
        jScrollPaneIns.setViewportView(jListIns);

        gridBagConstraints = new java.awt.GridBagConstraints();
        gridBagConstraints.fill = java.awt.GridBagConstraints.BOTH;
        jPanelHeader.add(jScrollPaneIns, gridBagConstraints);

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
        jScrollPaneTable.setPreferredSize(new java.awt.Dimension(200, 200));
        jScrollPaneTable.setViewportView(null);

        jTableObs.setModel(obsModel);
        jTableObs.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jTableObs.setMinimumSize(new java.awt.Dimension(50, 50));
        jTableObs.setName("jTableObs"); // NOI18N
        jScrollPaneTable.setViewportView(jTableObs);

        add(jScrollPaneTable, java.awt.BorderLayout.CENTER);
    }// </editor-fold>//GEN-END:initComponents

    private void jListInsValueChanged(javax.swing.event.ListSelectionEvent evt) {//GEN-FIRST:event_jListInsValueChanged
        processInstrumentsValueChanged(evt);
    }//GEN-LAST:event_jListInsValueChanged


    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JLabel jLabelFilterBy;
    private javax.swing.JList<String> jListIns;
    private javax.swing.JPanel jPanelHeader;
    private javax.swing.JPanel jPanelHeaderFiller;
    private javax.swing.JScrollPane jScrollPaneIns;
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

    final class EnhancedTableCellEditor extends AbstractCellEditor implements TableCellEditor {

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
    private static void showFrameData(final int frameCloseOperation, final String title, final List<RawObservation> observations) {
        final JFrame frame = new JFrame(title);
        frame.setDefaultCloseOperation(frameCloseOperation);

        frame.setMinimumSize(new Dimension(800, 800));

        frame.add(new RawObservationTablePanel().setData(observations));
        frame.pack();
        frame.setVisible(true);
    }

    public static void main(String[] args) {
        // invoke Bootstrapper method to initialize logback now:
        Bootstrapper.getState();

        final File file = new File("/home/bourgesl/ASPRO2/obsportal_targets.asprox");

        try {
            ObservationSetting observation = ObservationManager.getInstance().loadObservation(file, null);

            // Merge all raw obs:
            final List<RawObservation> obsList = new ArrayList<RawObservation>();
            for (TargetRawObservation targetRawObs : observation.getTargetObservations()) {
                obsList.addAll(targetRawObs.getObservations());
            }
            showFrameData(JFrame.EXIT_ON_CLOSE, "Observations", obsList);

        } catch (IOException ioe) {
            logger.error("IO exception occured:", ioe);
        }
    }
}
