/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.chart;

import fr.jmmc.oiexplorer.core.gui.chart.ChartUtils;
import java.awt.Font;
import java.awt.Graphics2D;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class gathers rendering options used when rendering the JFreeChart plot
 * @author bourgesl
 */
public final class ObservabilityPlotContext {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(ObservabilityPlotContext.class.getName());
    /** ObservabilityPlotContext singleton */
    private static final ObservabilityPlotContext instance = new ObservabilityPlotContext();
    /** default value for flag hideTextDontFit */
    public final static boolean DEFAULT_HIDE_TEXT_DONT_FIT = true;
    /** text pattern to perform auto fit time width */
    public final static String AUTO_FIT_TIME = "00:00";
    /** text pattern to perform auto fit tip height */
    public final static String AUTO_FIT_TIP = "00";
    /** text pattern to perform auto fit symbol text height */
    public final static String AUTO_FIT_SYMBOL = "HD 123";
    /** maximum font size for text annotations */
    private final static int MAX_SIZE_FONT = 12;
    /** minimum font size for text annotations (display) */
    private final static int DISPLAY_MIN_SIZE_FONT = 6;
    /** minimum font size for text annotations (print) */
    private final static int PRINT_MIN_SIZE_FONT = 2;
    /** minimum size for diamond mark (in Java2D units). */
    public final static double MIN_DIAMOND_SIZE = 3d;
    /** The default tip radius (in Java2D units). */
    public static final double DEFAULT_TIP_RADIUS = 20d;
    /** maximum half tip length (in Java2D units). */
    public static final double MAX_HALF_TICK_LENGTH = 2.5d;
    /** minimum half tip length (in Java2D units). */
    public static final double MIN_HALF_TICK_LENGTH = 0.5d;
    /* members */
    /** flag indicating to disable annotation rendering if its size do not fit i.e. too small (useful for GUI rendering) */
    private boolean hideAnnotationTooSmall;
    /** minimum font size for text annotations */
    private int minSizeFont = DISPLAY_MIN_SIZE_FONT;
    /* diamond marks */
    /** maximum width defined in data units to scale the diamond mark */
    private double maxDiamondWidth = 0d;
    /** flag indicating that autoFitDiamondSize is done */
    private boolean autoFitDiamondSizeDone = false;
    /** diamond scaling factor to scale the diamond mark */
    private double autoFitDiamondScale = 1d;
    /* fit text annotation */
    /** maximum width defined in data units to scale the text */
    private double maxTextWidth = 0d;
    /** flag indicating that autoFitTimeWidthFont is done */
    private boolean autoFitTimeWidthDone = false;
    /** best font for the complete plot to fit text width like '00:00' */
    private Font autoFitTimeFont = null;
    /* tip and text annotation */
    /** tip radius (center of the tick) defined in data units to position and scale the tick */
    private double tipRadius = 0d;
    /** maximum height defined in data units to scale the text + half tick */
    private double maxTipHeight = 0d;
    /** flag indicating that autoFitTip is done */
    private boolean autoFitTipDone = false;
    /** half tick length (scaled) */
    private double autoFitHalfTickLength = 0d;
    /** best font for the complete plot to fit text height like '00' */
    private Font autoFitTipFont = null;
    /** flag indicating that autoFitSymbolAxisFont is done */
    private boolean autoFitSymbolAxisFontDone = false;
    /** best font for the complete plot to fit symbol axis's text height like '123' */
    private Font autoFitSymbolAxisFont = null;

    /**
     * Return the ObservabilityPlotContext singleton
     * @return ObservabilityPlotContext singleton
     */
    public static ObservabilityPlotContext getInstance() {
        return instance;
    }

    /**
     * Public constructor
     */
    private ObservabilityPlotContext() {
        super();

        setHideAnnotationTooSmall(DEFAULT_HIDE_TEXT_DONT_FIT);

        reset();
    }

    /**
     * Reset any cached state
     */
    public void reset() {
        this.autoFitDiamondSizeDone = false;
        this.autoFitDiamondScale = 1d;

        this.autoFitTimeWidthDone = false;
        this.autoFitTimeFont = null;

        this.autoFitTipDone = false;
        this.autoFitHalfTickLength = 0d;
        this.autoFitTipFont = null;

        this.autoFitSymbolAxisFontDone = false;
        this.autoFitSymbolAxisFont = null;
    }

    /**
     * Return the flag indicating to disable annotation rendering if its size do not fit i.e. too small (useful for GUI rendering)
     * @return flag indicating to disable annotation rendering if its size do not fit i.e. too small (useful for GUI rendering)
     */
    public boolean isHideAnnotationTooSmall() {
        return hideAnnotationTooSmall;
    }

    /**
     * Define the flag indicating to disable annotation rendering if its size do not fit i.e. too small (useful for GUI rendering)
     * @param hideAnnotationTooSmall flag indicating to disable annotation rendering if its size do not fit i.e. too small (useful for GUI rendering)
     */
    public void setHideAnnotationTooSmall(final boolean hideAnnotationTooSmall) {
        this.hideAnnotationTooSmall = hideAnnotationTooSmall;
    }

    /**
     * Return the minimum font size for text annotations
     * @return minimum font size for text annotations
     */
    public int getMinSizeFont() {
        return minSizeFont;
    }

    /**
     * Define the minimum font size for text annotations
     * @param print flag to use printer small fonts instead of display
     */
    public void setMinSizeFont(final boolean print) {
        this.minSizeFont = (print) ? PRINT_MIN_SIZE_FONT : DISPLAY_MIN_SIZE_FONT;
    }

    // --- AutoFit XYDiamondAnnotation (size) ------------------------------------
    /**
     * Return the maximum width defined in data units to scale the diamond mark
     * @return maximum width defined in data units to scale the diamond mark
     */
    public double getMaxDiamondWidth() {
        return maxDiamondWidth;
    }

    /**
     * Define the maximum width defined in data units to scale the diamond mark
     * @param maxDiamondWidth maximum width defined in data units to scale the diamond mark
     */
    public void setMaxDiamondWidth(final double maxDiamondWidth) {
        this.maxDiamondWidth = maxDiamondWidth;
    }

    /**
     * Return the flag indicating that autoFitTimeWidthFont is done
     * @return flag indicating that autoFitTimeWidthFont is done
     */
    public boolean autoFitDiamondSizeDone() {
        return this.autoFitDiamondSizeDone;
    }

    /**
     * Return the diamond scaling factor to scale the diamond mark
     * @return scaling factor (0 means disable rendering)
     */
    public double autoFitDiamondScale() {
        return this.autoFitDiamondScale;
    }

    /**
     * Return the diamond scaling factor whose size best fits the given width
     * @param maxWidth maximum pixel width to fit
     * @param displaySize intial diamond size in pixels
     * @return scaling factor (0 means disable rendering)
     */
    public double autoFitDiamondSize(final double maxWidth, final double displaySize) {
        this.autoFitDiamondSizeDone = true;

        double scale = 1d;
        if (maxWidth < displaySize) {
            scale = maxWidth / displaySize;

            if (MIN_DIAMOND_SIZE > maxWidth) {
                if (isHideAnnotationTooSmall()) {
                    scale = 0d;
                } else {
                    scale = MIN_DIAMOND_SIZE / displaySize;
                }
            }
        }

        if (logger.isDebugEnabled()) {
            logger.debug("autoFitDiamondSize: {} <=> {}", scale, (scale * displaySize));
        }

        this.autoFitDiamondScale = scale;

        return scale;
    }

    // --- AutoFit FitXYTextAnnotation (width) -----------------------------------
    /**
     * Return the maximum width defined in data units to scale the text
     * @return maximum width defined in data units
     */
    public double getMaxTextWidth() {
        return maxTextWidth;
    }

    /**
     * Set the maximum width defined in data units to scale the text
     * @param maxWidth maximum width defined in data units
     */
    public void setMaxTextWidth(final double maxWidth) {
        this.maxTextWidth = maxWidth;
    }

    /**
     * Return the flag indicating that autoFitDiamondSize is done
     * @return flag indicating that autoFitDiamondSize is done
     */
    public boolean autoFitTimeWidthDone() {
        return this.autoFitTimeWidthDone;
    }

    /**
     * Return the biggest font whose size best fits the text '00:00'
     * @return font (null means disable rendering)
     */
    public Font autoFitTimeWidthFont() {
        return this.autoFitTimeFont;
    }

    /**
     * Return the biggest font whose size best fits the text '00:00' for the given width
     * @param g2d graphics object
     * @param maxWidth maximum pixel width to fit
     * @return font (null means disable rendering)
     */
    public Font autoFitTimeWidthFont(final Graphics2D g2d, final double maxWidth) {
        this.autoFitTimeWidthDone = true;

        final Font bestFont = ChartUtils.autoFitTextWidth(g2d, AUTO_FIT_TIME, maxWidth, getMinSizeFont(), MAX_SIZE_FONT, !isHideAnnotationTooSmall());

        if (logger.isDebugEnabled()) {
            logger.debug("autoFitTimeWidth: {}", bestFont);
        }

        this.autoFitTimeFont = bestFont;

        return bestFont;
    }

    // --- AutoFit XYTickAnnotation (height) -------------------------------------
    /**
     * Return the maximum height defined in data units to scale the text + half tick
     * @return maximum height defined in data units to scale the text + half tick
     */
    public double getMaxTipHeight() {
        return maxTipHeight;
    }

    /**
     * Define the maximum height defined in data units to scale the text + half tick
     * @param maxTipHeight maximum height defined in data units to scale the text + half tick
     */
    public void setMaxTipHeight(final double maxTipHeight) {
        this.maxTipHeight = maxTipHeight;
    }

    /**
     * Return the tip radius (center of the tick) defined in data units to position and scale the tick
     * @return tip radius (center of the tick) defined in data units to position and scale the tick
     */
    public double getTipRadius() {
        return tipRadius;
    }

    /**
     * Define the tip radius (center of the tick) defined in data units to position and scale the tick
     * @param tipRadius tip radius (center of the tick) defined in data units to position and scale the tick
     */
    public void setTipRadius(final double tipRadius) {
        this.tipRadius = tipRadius;
    }

    /**
     * Return the flag indicating that autoFitTip is done
     * @return flag indicating that autoFitTip is done
     */
    public boolean autoFitTipDone() {
        return this.autoFitTipDone;
    }

    /**
     * Return the best half tick length whose scale best fits the given radius
     * @return half tick length (0 means disable rendering)
     */
    public double autoFitTickLength() {
        return this.autoFitHalfTickLength;
    }

    /**
     * Return the best half tick length whose scale best fits the given length
     * @param length length in pixels
     * @return half tick length (0 means disable rendering)
     */
    public double autoFitTickLength(final double length) {
        this.autoFitTipDone = true;

        final double scale = length / DEFAULT_TIP_RADIUS;

        double halfTickLen = (scale > 1d) ? MAX_HALF_TICK_LENGTH : scale * MAX_HALF_TICK_LENGTH;

        if (MIN_HALF_TICK_LENGTH > halfTickLen) {
            if (isHideAnnotationTooSmall()) {
                halfTickLen = 0d;
            } else {
                halfTickLen = MIN_HALF_TICK_LENGTH;
            }
        }

        this.autoFitHalfTickLength = halfTickLen;

        if (logger.isDebugEnabled()) {
            logger.debug("autoFitTickLength: {} <=> {}", scale, halfTickLen);
        }

        return halfTickLen;
    }

    /**
     * Return the biggest font for the complete plot to fit text height like '00'
     * @return font (null means disable rendering)
     */
    public Font autoFitTipFont() {
        return this.autoFitTipFont;
    }

    /**
     * Return the biggest font for the complete plot to fit text height like '00'
     * @param g2d graphics object
     * @param maxHeight maximum pixel height to fit
     * @return font (null means disable rendering)
     */
    public Font autoFitTipFont(final Graphics2D g2d, final double maxHeight) {
        this.autoFitTipDone = true;

        final Font bestFont = ChartUtils.autoFitTextHeight(g2d, AUTO_FIT_TIP, maxHeight, getMinSizeFont(), MAX_SIZE_FONT, !isHideAnnotationTooSmall());

        if (logger.isDebugEnabled()) {
            logger.debug("autoFitTipFont: {}", bestFont);
        }

        this.autoFitTipFont = bestFont;

        return bestFont;
    }

    /**
     * Return the biggest font for the complete plot to fit text height like '00'
     * @param g2d graphics object
     * @param maxHeight maximum pixel height to fit
     * @return font (null means disable rendering)
     */
    public Font autoFitSymbolAxisFont(final Graphics2D g2d, final double maxHeight) {
        if (!this.autoFitSymbolAxisFontDone) {
            this.autoFitSymbolAxisFontDone = true;

            final Font bestFont = ChartUtils.autoFitTextHeight(g2d, AUTO_FIT_SYMBOL, maxHeight, getMinSizeFont(), MAX_SIZE_FONT, true);

            if (logger.isDebugEnabled()) {
                logger.debug("autoFitSymbolAxisFont: {}", bestFont);
            }

            this.autoFitSymbolAxisFont = bestFont;
        }

        return this.autoFitSymbolAxisFont;
    }
}
