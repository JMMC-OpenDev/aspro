/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import fr.jmmc.aspro.model.WarningMessage.Level;
import java.util.ArrayList;
import java.util.List;

/**
 * This class is a container for warning messages produced while computation
 * @author bourgesl
 */
public final class WarningContainer {

    /** observation version (read only) */
    private final ObservationVersion version;
    /** warning messages */
    private List<WarningMessage> warningMessages = null;
    /** message level */
    private Level level = null;

    /**
     * Public constructor
     * @param version observation version
     */
    public WarningContainer(final ObservationVersion version) {
        this.version = version;
    }

    /**
     * @return observation version
     */
    public ObservationVersion getVersion() {
        return version;
    }

    /**
     * Add the given warning container to this warning container only if new message(s) do not already exist in this container
     * @param container messages to add
     */
    public void addWarnings(final WarningContainer container) {
        if (container.hasWarning()) {
            for (WarningMessage message : container.getWarnings()) {
                addMessage(message);
            }
        }
    }

    /**
     * Add the given warning message to the messages
     * @param msg message to add
     */
    public void addWarning(final String msg) {
        addMessage(msg, WarningMessage.Level.Warning);
    }

    /**
     * Add the given information message to the messages
     * @param msg message to add
     */
    public void addInformation(final String msg) {
        addMessage(msg, WarningMessage.Level.Information);
    }

    /**
     * Add the given trace message to the messages
     * @param msg message to add
     */
    public void addTrace(final String msg) {
        addMessage(msg, WarningMessage.Level.Trace);
    }

    /**
     * Add the given trace message to the messages
     * @param msg message to add
     * @param level message level
     */
    public void addMessage(final String msg, final Level level) {
        addMessage(new WarningMessage(msg, level));
    }

    /**
     * Add a separator to the messages
     */
    public void addSeparator() {
        addMessage(WarningMessage.SEPARATOR, true);
    }

    /**
     * Add the given message to the warning messages (if not already present)
     * @param message WarningMessage to add
     */
    public void addMessage(final WarningMessage message) {
        addMessage(message, false);
    }

    /**
     * Add the given message to the warning messages (if not already present)
     * @param message WarningMessage to add
     * @param skipPresent true to ignore contains check
     */
    private void addMessage(final WarningMessage message, final boolean skipPresent) {
        if (this.warningMessages == null) {
            this.warningMessages = new ArrayList<WarningMessage>(4);
        }
        if (skipPresent || !this.warningMessages.contains(message)) {
            this.warningMessages.add(message);
            this.level = ((this.level == Level.Warning) || (message.getLevel() == Level.Warning)) ? Level.Warning : Level.Information;
        }
    }

    /**
     * remove the given message
     * @param message WarningMessage to remove
     */
    public void removeMessage(final WarningMessage message) {
        if (this.warningMessages != null) {
            this.warningMessages.remove(message);
        }
    }

    /**
     * Return true if there are warning messages
     * @return true if there are warning messages
     */
    public boolean hasWarning() {
        return this.warningMessages != null && !this.warningMessages.isEmpty();
    }

    /**
     * Return the first warning message in this warning container that contains the given string
     * @param match matching string
     * @return first warning message in this warning container that contains the given string or null if not found
     */
    public String getMatchingWarning(final String match) {
        if (hasWarning()) {
            for (WarningMessage message : getWarnings()) {
                if (message.getMessage().contains(match)) {
                    return message.getMessage();
                }
            }
        }
        return null;
    }

    /**
     * Return the highest level of warning messages (Warning > Information)
     * @return highest level of warning messages (Warning > Information) or null if empty
     */
    public Level getLevel() {
        return this.level;
    }

    /**
     * Return the list of warning messages
     * @return warning messages or null
     */
    public List<WarningMessage> getWarnings() {
        return this.warningMessages;
    }
}
