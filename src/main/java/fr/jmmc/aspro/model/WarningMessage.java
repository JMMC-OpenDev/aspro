/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

/**
 * This class represents a warning message (message + state)
 * @author bourgesl
 */
public final class WarningMessage {

    /** message level */
    public enum Level {

        /** information */
        Information,
        /** warning */
        Warning
    }
    /* members */
    /** message */
    private final String message;
    /** message level */
    private final Level level;

    /**
     * Protected Constructor
     * @param message message
     */
    WarningMessage(final String message) {
        this.message = message;
        this.level = Level.Warning;
    }

    /**
     * Protected Constructor
     * @param message message
     * @param level message level
     */
    WarningMessage(final String message, final Level level) {
        this.message = message;
        this.level = level;
    }

    /**
     * Return the message
     * @return message
     */
    public String getMessage() {
        return message;
    }

    /**
     * Return the message level 
     * @return message level 
     */
    public Level getLevel() {
        return level;
    }

    @Override
    public int hashCode() {
        int hash = 3;
        return hash;
    }

    @Override
    public boolean equals(final Object obj) {
        if (obj == null) {
            return false;
        }
        // Identity check:
        if (this == obj) {
            return true;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        final WarningMessage other = (WarningMessage) obj;
        if ((this.message == null) ? (other.getMessage() != null) : !this.message.equals(other.getMessage())) {
            return false;
        }
        if (this.level != other.getLevel()) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "WarningMessage{level=" + level + ", message='" + message + '}';
    }
}
