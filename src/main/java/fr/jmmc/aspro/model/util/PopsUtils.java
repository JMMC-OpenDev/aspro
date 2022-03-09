/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.model.oi.Pop;
import fr.jmmc.jmcs.util.StringUtils;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TreeMap;

/**
 * Map<String, Pop> to String encoding & decoding functions
 */
public final class PopsUtils {

    private final static char KEY_SEP = ':';
    private final static String ENTRY_SEP = " ";

    private PopsUtils() {
        super();
    }

    public static String toString(final TreeMap<String, Pop> popsFixed) {
        if ((popsFixed == null) || popsFixed.isEmpty()) {
            return null;
        }
        // Encode map as String 'SS:n ...':
        final StringBuilder sb = new StringBuilder(32);
        for (Map.Entry<String, Pop> entry : popsFixed.entrySet()) {
            final String name = entry.getKey();
            final Pop pop = entry.getValue();
            if (pop != null) {
                sb.append(name);
                sb.append(KEY_SEP);
                sb.append(pop.getIndex());
                sb.append(ENTRY_SEP);
            }
        }
        sb.deleteCharAt(sb.length() - ENTRY_SEP.length());
        return sb.toString();
    }

    public static TreeMap<String, Pop> fromString(final String encoded, final List<Pop> listPoPs) {
        if (StringUtils.isEmpty(encoded)) {
            return null;
        }
        final TreeMap<String, Pop> popsFixed = new TreeMap<>();

        final StringTokenizer tokenizer = new StringTokenizer(encoded, ENTRY_SEP);

        for (; tokenizer.hasMoreTokens();) {
            final String entry = tokenizer.nextToken();
            final int pos = entry.indexOf(KEY_SEP);
            if (pos != -1) {
                final String name = entry.substring(0, pos);
                final String index = entry.substring(pos + 1, entry.length());

                Pop p = null;
                for (Pop pop : listPoPs) {
                    if (index.equals(pop.getIndexAsString())) {
                        p = pop;
                        break;
                    }
                }
                if (p != null) {
                    popsFixed.put(name, p);
                }
            }
        }
        return popsFixed;
    }

}
