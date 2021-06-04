/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import fr.jmmc.aspro.AsproConstants;
import fr.jmmc.oiexplorer.core.gui.chart.ChartUtils;
import fr.jmmc.oiexplorer.core.gui.chart.EnhancedXYBarRenderer;
import java.awt.Color;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.annotations.XYTextAnnotation;
import org.jfree.chart.axis.DateAxis;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.labels.StandardXYToolTipGenerator;
import org.jfree.chart.labels.XYToolTipGenerator;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.XYPlot;
import org.jfree.chart.renderer.xy.XYBarRenderer;
import org.jfree.chart.urls.StandardXYURLGenerator;
import org.jfree.data.xy.IntervalXYDataset;

/**
 * Several static methods related to the JFreeChart library
 * @author bourgesl
 */
public final class AsproChartUtils {

    /** flag to use the warning annotation or the normal JMMC annotation */
    private static boolean warningAnnotation = false;

    /**
     * Define the flag to use the warning annotation or the normal JMMC annotation
     * @param warningAnnotation flag to use the warning annotation or the normal JMMC annotation
     */
    public static void setWarningAnnotation(final boolean warningAnnotation) {
        AsproChartUtils.warningAnnotation = warningAnnotation;
    }

    /**
     * Forbidden constructor
     */
    private AsproChartUtils() {
        // no-op
    }

    /**
     * Create an empty XYBarChart for the observability chart
     * @return jFreeChart instance
     */
    public static JFreeChart createXYBarChart() {
        // no title :
        final JFreeChart chart = createXYBarChart("", null, false, null, null, PlotOrientation.HORIZONTAL, true, false, false);

        final XYPlot xyPlot = (XYPlot) chart.getPlot();

        // enlarge right margin to have last displayed hour (00:00)
        xyPlot.setInsets(ChartUtils.NORMAL_PLOT_INSETS);

        // disable cross hairs (and distance computation):
        xyPlot.setDomainCrosshairVisible(false);
        xyPlot.setDomainCrosshairLockedOnData(false);
        xyPlot.setRangeCrosshairVisible(false);
        xyPlot.setRangeCrosshairLockedOnData(false);

        xyPlot.getDomainAxis().setVisible(false);
        xyPlot.getRangeAxis().setVisible(false);

        // Adjust outline :
        xyPlot.setOutlineStroke(ChartUtils.DEFAULT_STROKE);

        final XYBarRenderer xyBarRenderer = (XYBarRenderer) xyPlot.getRenderer();
        xyBarRenderer.setUseYInterval(true);
        xyBarRenderer.setDrawBarOutline(true);

        return chart;
    }

    /**
     * Creates and returns a default instance of an XY bar chart
     * BUT it uses an alternate XYPlot implementation to have gridlines visible even with range markers
     * <P>
     * The chart object returned by this method uses an {@link XYPlot} instance
     * as the plot, with a {@link DateAxis} for the domain axis, a
     * {@link NumberAxis} as the range axis, and a {@link XYBarRenderer} as the
     * renderer.
     *
     * @param title  the chart title (<code>null</code> permitted).
     * @param xAxisLabel  a label for the X-axis (<code>null</code> permitted).
     * @param dateAxis  make the domain axis display dates?
     * @param yAxisLabel  a label for the Y-axis (<code>null</code> permitted).
     * @param dataset  the dataset for the chart (<code>null</code> permitted).
     * @param orientation  the orientation (horizontal or vertical)
     *                     (<code>null</code> NOT permitted).
     * @param legend  a flag specifying whether or not a legend is required.
     * @param tooltips  configure chart to generate tool tips?
     * @param urls  configure chart to generate URLs?
     *
     * @return An XY bar chart.
     */
    public static JFreeChart createXYBarChart(final String title,
            final String xAxisLabel,
            final boolean dateAxis,
            final String yAxisLabel,
            final IntervalXYDataset dataset,
            final PlotOrientation orientation,
            final boolean legend,
            final boolean tooltips,
            final boolean urls) {

        if (orientation == null) {
            throw new IllegalArgumentException("Null 'orientation' argument.");
        }
        final ValueAxis domainAxis;
        if (dateAxis) {
            domainAxis = new DateAxis(xAxisLabel);
        } else {
            final NumberAxis axis = new NumberAxis(xAxisLabel);
            axis.setAutoRangeIncludesZero(false);
            domainAxis = axis;
        }
        final ValueAxis valueAxis = new NumberAxis(yAxisLabel);

        final XYBarRenderer renderer = new EnhancedXYBarRenderer();
        if (tooltips) {
            XYToolTipGenerator tt;
            if (dateAxis) {
                tt = StandardXYToolTipGenerator.getTimeSeriesInstance();
            } else {
                tt = new StandardXYToolTipGenerator();
            }
            renderer.setDefaultToolTipGenerator(tt);
        }
        if (urls) {
            renderer.setURLGenerator(new StandardXYURLGenerator());
        }

        final XYPlot plot = new GridLineFixedXYPlot(dataset, domainAxis, valueAxis, renderer);
        plot.setOrientation(orientation);

        final JFreeChart chart = ChartUtils.createChart(title, plot, legend);

        return chart;
    }

    /**
     * Creates a new text annotation to be displayed at the given coordinates.
     * The coordinates are specified in data space.
     *
     * @param text  the text (<code>null</code> not permitted).
     * @param x  the x-coordinate (in data space).
     * @param y  the y-coordinate (in data space).
     * @return new annotation
     */
    public static FitXYTextAnnotation createFitXYTextAnnotation(final String text, final double x, final double y) {
        final FitXYTextAnnotation a = new FitXYTextAnnotation(text, x, y);
        // font is determined automatically (auto-fit)
        return a;
    }

    /**
     * Creates a new text annotation to be displayed at the given coordinates.  The
     * coordinates are specified in data space.
     *
     * HACK : use small text annotation font
     *
     * @param text  the text (<code>null</code> not permitted).
     * @param x  the x-coordinate (in data space).
     * @param y  the y-coordinate (in data space).
     * @param cosAngle  cosinus of the angle of the arrow's line (in radians).
     * @param sinAngle  sinus of the angle of the arrow's line (in radians).
     * @return new annotation
     */
    public static XYTickAnnotation createXYTickAnnotation(final String text, final double x, final double y,
            final double cosAngle, final double sinAngle) {

        final XYTickAnnotation a = new XYTickAnnotation(text, x, y, cosAngle, sinAngle);
        a.setFont(ChartUtils.SMALL_TEXT_ANNOTATION_FONT);
        // default color is BLACK

        return a;
    }

    /**
     * Create a new JMMC annotation
     * @return new JMMC annotation
     */
    public static XYTextAnnotation createJMMCAnnotation() {
        if (warningAnnotation) {
            final XYTextAnnotation annotation = ChartUtils.createJMMCAnnotation(AsproConstants.JMMC_WARNING_ANNOTATION);
            annotation.setPaint(Color.RED);
            return annotation;
        }
        return ChartUtils.createJMMCAnnotation(AsproConstants.JMMC_ANNOTATION);
    }
}
