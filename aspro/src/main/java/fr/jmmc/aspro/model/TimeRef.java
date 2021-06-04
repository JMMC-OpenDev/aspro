/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model;

import java.util.ArrayList;
import java.util.List;

/** Supported Time Reference */
public enum TimeRef {

    /** Local Sidereal Time */
    LST("L.S.T."),
    /** UTC/GMT = Coordinated Universal Time (Greenwich Mean Time) */
    UTC("U.T.C."),
    /** Local Time */
    LOCAL("Local");

    /* members */
    private final String displayName;

    TimeRef(final String displayName) {
        this.displayName = displayName;
    }

    public String getDisplayName() {
        return displayName;
    }

    public static List<String> getDisplayNames() {
        final TimeRef[] refs = TimeRef.values();

        final List<String> names = new ArrayList<String>(refs.length);

        for (TimeRef ref : refs) {
            names.add(ref.displayName);
        }
        return names;
    }

    public static TimeRef findByDisplayName(final String displayName) {
        final TimeRef[] refs = TimeRef.values();
        
        for (TimeRef ref : refs) {
            if (ref.displayName.equals(displayName)) {
                return ref;
            }
        }
        throw new IllegalArgumentException("Unable to find matching TimeRef for the given display name='"+displayName+"' !");
    }
    
}
