/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

/**
 * This class represents a warning message (message + state)
 * @author bourgesl
 */
public final class WarningMessage {

    final static WarningMessage SEPARATOR = new WarningMessage("<hr/>", Level.Html);

    /** special warning when oifits disabled */
    public final static WarningMessage WARN_OIFITS_DISABLED = new WarningMessage("OIFits data computation is disabled");

    /** message level */
    public enum Level {
        /** html */
        Html,
        /** trace */
        Trace,
        /** information */
        Information,
        /** warning */
        Warning
    }
    /* members */
    /** message level */
    private final Level level;
    /** logged flag */
    private boolean logged = false;
    /** message */
    private final String message;

    /**
     * Protected Constructor
     * @param message message
     */
    public WarningMessage(final String message) {
        this(message, Level.Warning);
    }

    /**
     * Protected Constructor
     * @param message message
     * @param level message level
     */
    public WarningMessage(final String message, final Level level) {
        this.message = message;
        this.level = level;
        if (level == Level.Html) {
            setLogged(true);
        }
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

    public boolean isLogged() {
        return logged;
    }

    public void setLogged(boolean logged) {
        this.logged = logged;
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
        return this.level == other.getLevel();
    }

    @Override
    public String toString() {
        return "WarningMessage{level=" + level + ", message='" + message + '}';
    }
}
