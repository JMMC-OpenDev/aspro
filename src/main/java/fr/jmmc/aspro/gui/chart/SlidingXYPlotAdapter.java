/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import fr.jmmc.aspro.gui.util.TargetList;
import fr.jmmc.aspro.model.observability.StarObservabilityData;
import fr.jmmc.aspro.model.observability.TargetPositionDate;
import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.model.rawobs.RawObservation;
import fr.jmmc.jmcs.util.FormatterUtils;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.oiexplorer.core.gui.chart.BoundedSymbolAxis;
import fr.jmmc.oiexplorer.core.gui.chart.EnhancedXYBarRenderer;
import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Paint;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.annotations.XYAnnotation;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.labels.XYToolTipGenerator;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYBarRenderer;
import org.jfree.chart.ui.Layer;
import org.jfree.chart.ui.RectangleInsets;
import org.jfree.data.Range;
import org.jfree.data.gantt.TaskSeries;
import org.jfree.data.gantt.TaskSeriesCollection;
import org.jfree.data.gantt.XYTaskDataset;
import org.jfree.data.time.TimePeriod;
import org.jfree.data.xy.XYDataset;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is a custom XYPlot adapter to provide both a sliding dataset and annotations in sync.
 * @author bourgesl
 */
public final class SlidingXYPlotAdapter implements XYToolTipGenerator {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(SlidingXYPlotAdapter.class.getName());
    /** max items printed before using A3 format */
    public final static int MAX_PRINTABLE_ITEMS_A4 = 10;
    /** max items printed before using A2 format */
    public final static int MAX_PRINTABLE_ITEMS_A3 = MAX_PRINTABLE_ITEMS_A4 * 4;
    /** max items displayed before scrolling */
    public final static int MAX_VIEW_ITEMS = MAX_PRINTABLE_ITEMS_A4;
    /** symbol label insets: 0.5 on top/bottom, 2 on left/right */
    private final static RectangleInsets SYMBOL_LABEL_INSETS = new RectangleInsets(0.5d, 2d, 0.5d, 2d);
    /** cached highlight colors keyed by their initial color */
    private static final Map<Color, Paint> HIGHLIGHT_PAINTS = new HashMap<Color, Paint>(8);
    /** 15% transparent white color */
    private static final Color WHITE_75 = new Color(255, 255, 255, 224);
    /* members */
    /** jFreeChart instance */
    private final JFreeChart chart;
    /** xy plot instance */
    private final XYPlot xyPlot;
    /** JMMC annotation */
    private XYTextAnnotation aJMMC = null;
    /** plot renderer to define annotations */
    private final XYBarRenderer renderer;
    /** number of data items */
    private int size;
    /** data collection */
    private TaskSeriesCollection collection = null;
    /** data symbols (targets) */
    private List<String> symbols = null;
    /** data colors (targets) */
    private List<Color> colors = null;
    /** annotations */
    private Map<Integer, List<XYAnnotation>> annotations = null;
    /* tooltip information */
    /** target list for tooltip generation */
    private List<Target> targetList = null;
    /** (optional) target user informations */
    private TargetUserInformations targetUserInfos = null;
    /** data labels (legend) */
    private List<String> labels = null;
    /** StarObservabilityData list for tooltip generation */
    private List<StarObservabilityData> soTargetList = null;
    /** complete target to 'virtual' position mapping */
    private Map<String, Integer> fullTargetIndex = null;
    /** true to indicate to change grid line colors; false otherwise */
    private boolean hasBackground = false;
    /** 24h date formatter like in france */
    private final DateFormat timeFormatter = DateFormat.getTimeInstance(DateFormat.SHORT, Locale.FRANCE);
    /** double formatter for HA */
    private final NumberFormat df1 = new DecimalFormat("0.0");
    /** internal state */
    private final SlidingXYPlotState state = new SlidingXYPlotState();
    /* previous state */
    /** last used start position for the subset */
    private int lastStart = -1;
    /** last used end position for the subset */
    private int lastEnd = -1;
    /** tooltip buffer */
    private final StringBuilder sbToolTip = new StringBuilder(512);
    /** formatter buffer */
    private final StringBuffer sbf = new StringBuffer(512);

    /**
     * Constructor
     * @param chart chart instance
     * @param plot xy plot
     * @param maxElements max items in the chart view
     */
    public SlidingXYPlotAdapter(final JFreeChart chart, final XYPlot plot, final int maxElements) {
        this.chart = chart;
        this.xyPlot = plot;
        this.state.maxViewItems = maxElements;
        this.renderer = (XYBarRenderer) plot.getRenderer();
        this.renderer.setDefaultToolTipGenerator(this);
    }

    public TargetUserInformations getTargetUserInfos() {
        return targetUserInfos;
    }

    public void setTargetUserInfos(final TargetUserInformations targetUserInfos) {
        this.targetUserInfos = targetUserInfos;
    }

    /**
     * Define the new data
     * @param collection data collection
     * @param symbols data symbols
     * @param colors data colors
     * @param annotations map of annotations keyed by position
     * @param targetList target list for tooltip generation / selection handling
     * @param labels data labels (legend)
     * @param soTargetList StarObservabilityData list for tooltip generation
     * @param fullTargetIndex complete target to 'virtual' position mapping
     * @param hasBackground true to indicate to change grid line colors; false otherwise
     * @param aJMMC JMMC annotation instance to be added to renderer's annotations
     */
    public void setData(final TaskSeriesCollection collection, final List<String> symbols, final List<Color> colors,
                        final Map<Integer, List<XYAnnotation>> annotations,
                        final List<Target> targetList, final List<String> labels,
                        final List<StarObservabilityData> soTargetList,
                        final Map<String, Integer> fullTargetIndex,
                        final boolean hasBackground,
                        final XYTextAnnotation aJMMC) {
        this.size = symbols.size();
        this.collection = collection;
        this.symbols = symbols;
        this.colors = colors;
        this.annotations = annotations;
        this.targetList = targetList;
        this.labels = labels;
        this.soTargetList = soTargetList;
        this.fullTargetIndex = fullTargetIndex;
        this.hasBackground = hasBackground;
        this.aJMMC = aJMMC;
    }

    /**
     * Return the number of data items
     * @return number of data items
     */
    public int getSize() {
        return size;
    }

    /**
     * Return a state copy
     * @return state copy
     */
    public SlidingXYPlotState backupState() {
        return new SlidingXYPlotState(this.state);
    }

    /**
     * Copy the state from the given state
     * @param state state to copy
     */
    public void restoreState(final SlidingXYPlotState state) {
        final int oldSelectedPosition = this.state.selectedPosition;

        // restore state from given state:
        this.state.copy(state);

        // may update plot:
        updatePlot(false);

        // may update series paint:
        setSelectedPosition(this.state.selectedPosition, oldSelectedPosition);
    }

    /**
     * Return the max items in the chart view when the useSubset mode is enabled
     * @return max items in the chart view
     */
    public int getMaxViewItems() {
        return this.state.maxViewItems;
    }

    /**
     * Define the max items in the chart view when the useSubset mode is enabled
     * @param maxViewItems max items in the chart view
     */
    public void setMaxViewItems(final int maxViewItems) {
        this.state.maxViewItems = maxViewItems;
    }

    /**
     * Return the current position of the subset
     * @return current position of the subset
     */
    public int getPosition() {
        return this.state.position;
    }

    /**
     * Define the new position of the subset and refresh the plot
     *
     * @param pos new position of the subset
     */
    public void setPosition(final int pos) {
        this.state.position = pos;

        // may update plot:
        updatePlot(false);
    }

    /**
     * Return the selected position (highlight)
     * @return selected position (highlight)
     */
    public int getSelectedPosition() {
        return this.state.selectedPosition;
    }

    /**
     * Define the selected position (highlight)
     * @param selectedPosition selected position (highlight)
     */
    public void setSelectedPosition(final int selectedPosition) {
        // may update series paint:
        setSelectedPosition(selectedPosition, this.state.selectedPosition);
    }

    /**
     * Define the selected position (highlight) if different than given old value
     * @param selectedPosition selected position (highlight)
     * @param oldSelected old value
     */
    private void setSelectedPosition(final int selectedPosition, final int oldSelected) {
        if (oldSelected != selectedPosition) {
            this.state.selectedPosition = selectedPosition;

            updateAnnotationPaint(oldSelected);
            updateSeriesPaint(oldSelected);
            updateAnnotationPaint(this.state.selectedPosition);
            updateSeriesPaint(this.state.selectedPosition);

            // fire change event:
            if (this.renderer instanceof EnhancedXYBarRenderer) {
                ((EnhancedXYBarRenderer) this.renderer).setNotify(true);
            }
        }
    }

    /**
     * Return the position given the series index
     * @param series  the series index (zero-based).
     * @return position relative to the target list
     */
    public int getSeriePosition(final int series) {
        // note: series corresponds to the target
        if (this.lastStart != -1) {
            return this.lastStart + series;
        }
        return -1;
    }

    /**
     * Return true if the given position is visible (between lastStart and lastEnd) 
     * @param pos position 
     * @return true if the given position is visible 
     */
    public boolean isInViewRange(final int pos) {
        return (this.lastStart != -1) && (this.lastStart <= pos) && (pos <= this.lastEnd);
    }

    /**
     * Return the flag to enable/disable the subset mode
     * @return flag to enable/disable the subset mode
     */
    public boolean isUseSubset() {
        return this.state.useSubset;
    }

    /**
     * Define the flag to enable/disable the subset mode and force an update of the plot
     * @param useSubset new value
     */
    public void setUseSubset(final boolean useSubset) {
        if (logger.isDebugEnabled()) {
            logger.debug("useSubset: {}", useSubset);
        }

        this.state.useSubset = useSubset;

        // forced update plot:
        updatePlot(true);
    }

    /**
     * Update the plot
     * @param forceUpdate flag to force an update of the plot
     */
    private void updatePlot(final boolean forceUpdate) {
        if (this.state.useSubset) {
            updatePlot(this.state.maxViewItems, forceUpdate);
        } else {
            updatePlot(this.size, forceUpdate);
        }
    }

    /**
     * Update the plot
     * @param max number of items to display
     * @param forceUpdate flag to force an update of the plot
     */
    private void updatePlot(final int max, final boolean forceUpdate) {
        int start = this.state.position;
        if (start < 0) {
            start = 0;
        }

        int end = start + max;
        if (end > this.size) {
            end = this.size;
            start = end - max;
            if (start < 0) {
                start = 0;
            }
        }

        if (!forceUpdate && start == this.lastStart && end == this.lastEnd) {
            if (logger.isDebugEnabled()) {
                logger.debug("same positions : ignoring ({} to {})", start, end);
            }
            return;
        }

        // update memorized position to avoid redundant updates:
        this.lastStart = start;
        this.lastEnd = end;

        int newSize = end - start;

        if (logger.isDebugEnabled()) {
            logger.debug("updatePlot: pos = {} :: ({} to {})", this.state.position, start, end);
        }

        final double barWidth;
        final double rangeMin, rangeMax;
        boolean drawOutline = true;

        if (newSize > 0) {
            rangeMin = -(2d / 3d);
            rangeMax = newSize - (1d / 3d);
        } else {
            newSize = 0;
            rangeMin = 0d;
            rangeMax = 1d;
        }

        // adjust bar width:
        if (newSize > 50) {
            barWidth = 0.95d;
            // disable outline (gray) to better see colored bars:
            drawOutline = false;
        } else if (newSize > 15) {
            barWidth = 0.8d;
        } else if (newSize > 6) {
            barWidth = 0.6d;
        } else if (newSize > 1) {
            barWidth = 0.5d;
        } else {
            barWidth = 0.25d;
        }

        // disable chart & plot notifications:
        final boolean savedNotify = this.chart.isNotify();
        this.chart.setNotify(false);
        this.xyPlot.setNotify(false);
        if (this.renderer instanceof EnhancedXYBarRenderer) {
            ((EnhancedXYBarRenderer) this.renderer).setNotify(false);
        }

        try {
            // draw outline ?
            this.renderer.setDrawBarOutline(drawOutline);
            // reset colors :
            this.renderer.clearSeriesPaints(false);
            // side effect with chart theme :
            this.renderer.setAutoPopulateSeriesPaint(false);

            final TaskSeriesCollection subTaskSeriesCollection = new TaskSeriesCollection();

            final String[] subSymbols = new String[newSize];

            for (int i = start, n = 0; i < end; i++, n++) {

                subTaskSeriesCollection.add(this.collection.getSeries(i));

                subSymbols[n] = this.symbols.get(i);

                // color :
                this.renderer.setSeriesPaint(n, this.colors.get(i), false);
            }

            // update color for selected position:
            updateAnnotationPaint(this.state.selectedPosition);
            updateSeriesPaint(this.state.selectedPosition);

            final XYTaskDataset dataset = new XYTaskDataset(subTaskSeriesCollection);

            dataset.setSeriesWidth(barWidth);

            // update dataset :
            this.xyPlot.setDataset(dataset);

            // change the Domain axis (vertical):
            if (!(this.xyPlot.getDomainAxis() instanceof AutoFitBoundedSymbolAxis)) {
                final BoundedSymbolAxis symbolAxis = new AutoFitBoundedSymbolAxis(null, subSymbols);
                symbolAxis.setInverted(true);
                symbolAxis.setGridBandsVisible(false);
                this.xyPlot.setDomainAxis(symbolAxis);
            }

            // adjust symbols and range:
            final BoundedSymbolAxis symbolAxis = (AutoFitBoundedSymbolAxis) this.xyPlot.getDomainAxis();
            symbolAxis.setSymbols(subSymbols);
            symbolAxis.setBounds(new Range(rangeMin, rangeMax));
            symbolAxis.setRange(rangeMin, rangeMax);

            // remove Annotations :
            this.renderer.removeAnnotations();

            // annotations :
            if (this.annotations != null) {

                final double halfBarWidth = 0.5d * barWidth;

                final ObservabilityPlotContext renderContext = ObservabilityPlotContext.getInstance();

                // set text maximum width = bar width :
                renderContext.setMaxTextWidth(barWidth);

                // set diamond maximum size = 85% bar width :
                renderContext.setMaxDiamondWidth(barWidth * 0.85d);

                // set tip radius = 50% bar width (tick location over bar edges) :
                renderContext.setTipRadius(halfBarWidth);

                // set max tip height = margin between bars (half tick + text) :
                renderContext.setMaxTipHeight(0.5d * (1d - barWidth));

                // Redefine the x-position of annotations (corresponding to visible targets) :
                List<XYAnnotation> list;
                Integer pos;
                for (int i = start, n = 0; i < end; i++, n++) {
                    pos = NumberUtils.valueOf(i);

                    list = this.annotations.get(pos);

                    if (list != null) {

                        for (XYAnnotation annotation : list) {
                            if (annotation instanceof ExtendedXYTextAnnotation) { // TODO
                                // Applies to XYTickAnnotation / FitXYTextAnnotation :
                                final ExtendedXYTextAnnotation a = (ExtendedXYTextAnnotation) annotation;
                                a.setX(n);
                                this.renderer.addAnnotation(a);

                            } else if (annotation instanceof XYDiamondAnnotation) {
                                final XYDiamondAnnotation a = (XYDiamondAnnotation) annotation;
                                a.setX(n);

                                this.renderer.addAnnotation(a, Layer.FOREGROUND);
                            } else if (annotation instanceof EnhancedXYBoxAnnotation) {
                                final EnhancedXYBoxAnnotation a = (EnhancedXYBoxAnnotation) annotation;
                                a.setX0(n - halfBarWidth);
                                a.setX1(n + halfBarWidth);
                                // define the optional series index for mouse events (click on annotation):
                                a.setSeriesIndex(n);

                                // note: use layer according to the annotation layer:
                                this.renderer.addAnnotation(a, a.getLayer());
                            } else if (annotation instanceof XYTextAnnotation) {
                                // Applies to XYTickAnnotation also :
                                final XYTextAnnotation a = (XYTextAnnotation) annotation;
                                a.setX(n);
                                this.renderer.addAnnotation(a);
                            } else {
                                logger.info("unsupported annotation type: {}", annotation.getClass().getName());
                            }
                        }
                    }
                }
            }

            // add JMMC annotation (moving position):
            this.renderer.addAnnotation(this.aJMMC, Layer.BACKGROUND);

            // tick color :
            this.xyPlot.getRangeAxis().setTickMarkPaint(Color.BLACK);
            this.xyPlot.getDomainAxis().setTickMarkPaint(Color.BLACK);

            // update theme at end :
            org.jfree.chart.ChartUtils.applyCurrentTheme(this.chart);

            // hack symbol axis tick margin:
            this.xyPlot.getDomainAxis().setTickLabelInsets(SYMBOL_LABEL_INSETS); // margin set to 1

            if (hasBackground) {
                // Set grid line colors:
                this.xyPlot.setDomainGridlinePaint(Color.WHITE);
                this.xyPlot.setRangeGridlinePaint(Color.WHITE);
            }

        } finally {
            // restore chart & plot notifications:
            if (this.renderer instanceof EnhancedXYBarRenderer) {
                ((EnhancedXYBarRenderer) this.renderer).setNotify(true);
            }
            this.xyPlot.setNotify(savedNotify);
            this.chart.setNotify(savedNotify);
        }
    }

    /**
     * Update the series paint at given index (highlighted or not)
     * @param index index to use
     */
    private void updateSeriesPaint(final int index) {
        // ensure boundary checks in dataset:
        if (index >= 0 && index < this.size) {
            final int n = index - this.lastStart;
            // ensure boundary checks in view:
            if (n >= 0 && n < (this.lastEnd - this.lastStart)) {
                final Color initial = this.colors.get(index);

                final Paint fillPaint = (index == this.state.selectedPosition) ? getHighlightPaint(initial) : initial;

                // color :
                this.renderer.setSeriesPaint(n, fillPaint, false);
            }
        }
    }

    /**
     * Update the series paint at given index (highlighted or not)
     * @param index index to use
     */
    private void updateAnnotationPaint(final int index) {
        // ensure boundary checks in dataset:
        if (index >= 0 && index < this.size) {
            final List<XYAnnotation> list = this.annotations.get(NumberUtils.valueOf(index));

            if (list != null) {
                for (XYAnnotation annotation : list) {
                    if (annotation instanceof EnhancedXYBoxAnnotation) {
                        final EnhancedXYBoxAnnotation a = (EnhancedXYBoxAnnotation) annotation;
                        // set the highlighted flag if an highlight paint is defined:
                        if (a.getHighlightPaint() != null) {
                            a.setHighlighted(index == this.state.selectedPosition);
                        }
                    }
                }
            }
        }
    }

    /**
     * Generates the tooltip text for the specified item.
     *
     * @param dataset  the dataset (<code>null</code> not permitted).
     * @param series  the series index (zero-based).
     * @param item  the item index (zero-based).
     *
     * @return The tooltip text (possibly <code>null</code>).
     */
    @Override
    public String generateToolTip(final XYDataset dataset, final int series, final int item) {
        // note: series corresponds to the target, item to its observability ranges
        if (this.targetList != null) {
            final int index = getSeriePosition(series);

            if ((index >= 0) && (index < this.targetList.size())) {

                final Target target = this.targetList.get(index);
                logger.debug("target: {}", target);

                // use interval or star data:
                final TaskSeries taskSeries = this.collection.getSeries(index);
                final TimePeriod period = taskSeries.get(item).getDuration();
                final StarObservabilityData starObs = this.soTargetList.get(index);

                return generateToolTip(target, labels.get(index), null, starObs, period.getStart(), period.getEnd());
            }
        }
        return null;
    }

    /**
     * Generates the tooltip text for the given values.
     *
     * @param target target to get its tooltip text
     * @param legendLabel legend label
     * @param description constraint's description
     * @param starObs StarObservabilityData corresponding to the given target
     * @param startDate start date of the time interval
     * @param endDate end date of the time interval
     *
     * @return The tooltip text (possibly <code>null</code>).
     */
    public String generateToolTip(final Target target, final String legendLabel,
                                  final String description,
                                  final StarObservabilityData starObs,
                                  final Date startDate, final Date endDate) {

        final StringBuilder sb = this.sbToolTip;
        sb.setLength(0); // clear

        final StringBuffer sbf = this.sbf;

        sb.append("<html><b>").append(legendLabel).append(" Observability");
        if (description != null) {
            sb.append(" [").append(description).append(']');
        }
        sb.append("</b>");

        if (starObs != null) {
            // note: often the start or end TargetPositionDate can not be found
            // when the interval has been splitted due to night overlaps (RA)

            Date date = startDate;
            sb.append("<br><b>Start</b>: ");
            FormatterUtils.format(this.timeFormatter, sbf, date);
            sb.append(sbf);
            sbf.setLength(0);

            TargetPositionDate pos = starObs.getTargetPosition(date);
            if (pos != null) {
                sb.append(" - <b>HA</b>: ");
                FormatterUtils.format(this.df1, sbf, pos.getHa());
                sb.append(sbf);
                sbf.setLength(0);
                sb.append(" (az ").append(pos.getAzimuth());
                sb.append(", el ").append(pos.getElevation()).append(')');
            }

            date = endDate;
            sb.append("<br><b>End</b>: ");
            FormatterUtils.format(this.timeFormatter, sbf, date);
            sb.append(sbf);
            sbf.setLength(0);

            pos = starObs.getTargetPosition(date);
            if (pos != null) {
                sb.append(" - <b>HA</b>: ");
                FormatterUtils.format(this.df1, sbf, pos.getHa());
                sb.append(sbf);
                sbf.setLength(0);
                sb.append(" (az ").append(pos.getAzimuth());
                sb.append(", el ").append(pos.getElevation()).append(')');
            }

            date = starObs.getTransitDate();
            if (date != null) {
                sb.append("<br><b>Transit</b>: ");
                FormatterUtils.format(this.timeFormatter, sbf, date);
                sb.append(sbf);
                sbf.setLength(0);

                // if target not rise at transit: no az/el:
                pos = starObs.getTargetPosition(date);
                if (pos != null) {
                    sb.append(" (az ").append(pos.getAzimuth());
                    sb.append(", el ").append(pos.getElevation()).append(')');
                }
            }
            sb.append("<hr>");
        }

        TargetList.getTooltipPart(sb, false, target, this.targetUserInfos);

        sb.append("</html>");

        return sb.toString();
    }

    /**
     * Generates the tooltip text for the given values.
     *
     * @param obsCount number of observations in the observation group
     * @param obsFirst to get its tooltip text
     * @param obsLast to get its tooltip text
     *
     * @return The tooltip text (possibly <code>null</code>).
     */
    public String generateToolTip(final int obsCount, final RawObservation obsFirst, final RawObservation obsLast) {
        final StringBuffer sb = this.sbf;

        sb.append("<html>");
        obsFirst.toHtml(sb, obsCount);

        if ((obsLast != null) && (obsLast != obsFirst)) {
            obsFirst.timeToHtml(sb, 1); // show start
            obsFirst.weatherToHtml(sb);
            obsLast.timeToHtml(sb, 2); // show end
            obsLast.weatherToHtml(sb);
        } else {
            obsFirst.timeToHtml(sb, (1 | 2)); // show start / end
            obsFirst.weatherToHtml(sb);
        }
        sb.append("</html>");

        final String tooltip = sb.toString();
        sb.setLength(0);
        return tooltip;
    }

    /**
     * Return the position of the given target (first occurence matching by target name) in the complete target list
     * @param targetName target name to find its position (first occurence)
     * @return position or -1 if not found
     */
    public int findTargetPosition(final String targetName) {
        int pos = -1;

        if (targetName != null && this.targetList != null) {
            for (int i = 0, len = this.targetList.size(); i < len; i++) {
                final Target t = this.targetList.get(i);

                if (t.getName().equals(targetName)) {
                    pos = i;
                    break;
                }
            }
        }
        return pos;
    }

    /**
     * Return the virtual position of the given target (first occurence matching by target name) in the complete target list
     * @param targetName target name to find its position (first occurence)
     * @return position or -1 if not found
     */
    public int findTargetVirtualPosition(final String targetName) {
        int pos = -1;

        if (targetName != null && this.fullTargetIndex != null) {
            final Integer index = this.fullTargetIndex.get(targetName);
            if (index != null) {
                pos = index.intValue();
            }
        }
        return pos;
    }

    /**
     * Return the target name at the current position
     * @return target name
     */
    public String getCurrentTargetName() {
        return getTargetName(this.state.position);
    }

    /**
     * Return the target name at the given position (in the target list)
     * @param pos position
     * @return target name
     */
    public String getTargetName(final int pos) {
        String targetName = null;
        if (this.targetList != null && pos >= 0 && pos < this.targetList.size()) {
            targetName = this.targetList.get(pos).getName();
        }
        return targetName;
    }

    public static Paint getHighlightPaint(final Color initial) {
        Paint paint = HIGHLIGHT_PAINTS.get(initial);
        if (paint == null) {
            final Color other = (initial.getAlpha() == 255) ? Color.WHITE : WHITE_75;
            paint = new GradientPaint(0f, 0f, other, 0f, 1f, initial);
            HIGHLIGHT_PAINTS.put(initial, paint);
        }
        return paint;
    }
}
