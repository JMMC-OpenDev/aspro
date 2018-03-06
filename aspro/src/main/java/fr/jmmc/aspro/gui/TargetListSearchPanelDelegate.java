/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui;

import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.jmcs.gui.component.GenericListModel;
import fr.jmmc.jmcs.gui.component.SearchPanel;
import fr.jmmc.jmcs.gui.component.SearchPanel.SEARCH_DIRECTION;
import fr.jmmc.jmcs.gui.component.SearchPanelDelegate;
import fr.jmmc.jmcs.gui.util.SwingUtils;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.JList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Target list search engine.
 * @author Laurent BOURGES, Sylvain LAFRASSE.
 */
public final class TargetListSearchPanelDelegate implements SearchPanelDelegate {

    /** Logger */
    private static final Logger _logger = LoggerFactory.getLogger(TargetListSearchPanelDelegate.class.getName());
    /** undefined */
    private final static int UNDEFINED_INDEX = -1;
    // Members
    /** target list */
    private final JList jListTargets;
    /** current search value (regexp pattern) */
    private String _pattern;
    /** current found index */
    private int _lastFoundIndex;

    /**
     * Protected constructor
     * @param jListTargets target list
     */
    TargetListSearchPanelDelegate(final JList jListTargets) {
        this.jListTargets = jListTargets;
        reset();
    }

    /** Reset current search state */
    private void reset() {
        _pattern = null;
        _lastFoundIndex = UNDEFINED_INDEX;
    }

    public boolean search(final Pattern pattern, final SEARCH_DIRECTION givenDirection) {

        if (pattern == null) {
            return false;
        }

        // If the search token or view changed or a search reset was requested:
        SEARCH_DIRECTION currentDirection = givenDirection;

        // Get regexp from compiled Pattern:
        final String regexp = pattern.toString();

        if (!regexp.equals(_pattern) || (currentDirection == SEARCH_DIRECTION.UNDEFINED)) {
            reset(); // Reset search context
            currentDirection = SearchPanel.SEARCH_DIRECTION.NEXT; // Use NEXT direction by default
            _pattern = regexp; // Backup new search token
        }

        _logger.info("Searching pattern '{}' in '{}' direction.", regexp, currentDirection);

        // Get model:
        @SuppressWarnings("unchecked")
        final GenericListModel<Target> model = (GenericListModel<Target>) this.jListTargets.getModel();

        // Use tableSorter to process only currently visible rows and columns
        final int nbOfRows = model.getSize();

        int currentIndex = 0;

        // If the backward direction is requested
        final int directionalIncrement; // Either -1 to go backward r +1 to go forward
        if (currentDirection == SEARCH_DIRECTION.PREVIOUS) { // Going backward
            directionalIncrement = -1; // Will decrement indexes
        } else { // Going forward
            directionalIncrement = 1; // Will increment indexes
        }

        // Use previously found row/column if available
        if (_lastFoundIndex != UNDEFINED_INDEX) {
            _logger.debug("Current index = {}", _lastFoundIndex);
            currentIndex = _lastFoundIndex + directionalIncrement; // Skip current cell (i.e last one found) anyway !
        }

        final boolean isLogDebug = _logger.isDebugEnabled();

        // Traverse all targets
        Target foundValue = null;
        boolean searchDone = false;
        int foundIndex = UNDEFINED_INDEX;

        Target currentTarget;
        String currentValue;
        Matcher matcher;

        // Traverse all visible rows
        for (; currentIndex >= 0 && currentIndex < nbOfRows && !searchDone; currentIndex += directionalIncrement) {

            // Get current target:
            currentTarget = model.getElementAt(currentIndex);

            if (currentTarget != null) {
                // Get target name
                currentValue = currentTarget.getName();

                if (currentValue.length() != 0) {
                    if (isLogDebug) {
                        _logger.debug("Target name '{}' at index {}", currentValue, currentIndex);
                    }

                    // Do current value matches searched regexp ?
                    matcher = pattern.matcher(currentValue);

                    if (matcher.matches()) {
                        foundValue = currentTarget;
                        searchDone = true;
                        foundIndex = currentIndex;
                        break;
                    }
                }
            }
        }

        // If the searched value was not found
        if (foundValue == null) {
            return false;
        }

        // Select the found cell
        _logger.info("Found target [{}] at index {}", foundValue, foundIndex);

        final int selectedIndex = foundIndex;
        SwingUtils.invokeLaterEDT(new Runnable() {
            @Override
            public void run() {
                // as calibrators can be present multiple times, use setSelectedIndex:
                jListTargets.setSelectedIndex(selectedIndex);
                jListTargets.ensureIndexIsVisible(selectedIndex);

                // this.jListTargets.setSelectedValue(foundValue, true);
            }
        });

        // Memorize state for 'NEXT/PREVIOUS' purpose
        _lastFoundIndex = foundIndex;

        return true;
    }
}
