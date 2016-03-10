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

    /** warning messages */
    private List<WarningMessage> warningMessages = null;
    /** message level */
    private Level level = null;

    /**
     * Public constructor
     */
    public WarningContainer() {
        super();
    }

    /**
     * Add the given warning container to this warning container only if the new message does not already exist in this container
     * @param container messages to add
     */
    public void addWarningMessages(final WarningContainer container) {
        if (container.hasWarningMessages()) {
            for (WarningMessage message : container.getWarningMessages()) {
                addMessage(message);
            }
        }
    }

    /**
     * Add the given message to the warning messages
     * @param msg message to add
     */
    public void addWarningMessage(final String msg) {
        addMessage(new WarningMessage(msg));
    }

    /**
     * Add the given message to the information messages
     * @param msg message to add
     */
    public void addInformationMessage(final String msg) {
        addMessage(new WarningMessage(msg, WarningMessage.Level.Information));
    }

    /**
     * Add the given message to the warning messages (if not already present)
     * @param message WarningMessage to add
     */
    private void addMessage(final WarningMessage message) {
        if (this.warningMessages == null) {
            this.warningMessages = new ArrayList<WarningMessage>(4);
        }
        if (!this.warningMessages.contains(message)) {
            this.warningMessages.add(message);
            this.level = (this.level == Level.Warning || message.getLevel() == Level.Warning) ? Level.Warning : Level.Information;
        }
    }

    /**
     * Return true if there are warning messages
     * @return true if there are warning messages
     */
    public boolean hasWarningMessages() {
        return this.warningMessages != null && !this.warningMessages.isEmpty();
    }

    /**
     * Return the first warning message in this warning container that contains the given string
     * @param match matching string
     * @return first warning message in this warning container that contains the given string or null if not found
     */
    public String getMatchingWarningMessage(final String match) {
        if (hasWarningMessages()) {
            for (WarningMessage message : getWarningMessages()) {
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
    public List<WarningMessage> getWarningMessages() {
        return this.warningMessages;
    }
}
