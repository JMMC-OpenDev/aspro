/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.model.util;

import fr.jmmc.aspro.gui.TargetModelForm;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.lang.ref.WeakReference;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Simple Hard/Soft reference to reduce memory footprint
 * @param <K> reference object type
 */
public final class HeapReference<K> {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(TargetModelForm.class.getName());

    /* true to use soft reference (live until OOME) or false to use weak reference (short lived) */
    private final static boolean USE_SOFT = true;

    static {
        if (!USE_SOFT) {
            logger.warn("HeapReference.USE_SOFT = false : DO NOT USE IN PUBLIC RELEASES !");
        }
    }

    // members
    /** label for debugging */
    private final String name;
    /** optional hard reference to retain the weak reference 'strong' */
    private K hard = null;
    /** weak reference to the main object */
    private final Reference<K> weak;
    /** usage timestamp */
    private long timestamp;

    /**
     * Create an hard reference
     * @param name name used for debugging
     * @param value referenced heap object
     */
    public HeapReference(final String name, final K value) {
        this.name = name;
        this.hard = value;

        this.weak = (USE_SOFT) ? new SoftReference<>(value) : new WeakReference<>(value);
        updateTimeStamp();
        logger.debug("new HeapReference: {}", name);
    }

    public K get(final boolean updateTimestamp) {
        final K value = (isHardReference()) ? hard : this.weak.get();
        if (updateTimestamp && (value != null)) {
            updateTimeStamp();
        }
        return value;
    }

    public boolean isHardReference() {
        return (this.hard != null);
    }

    public void setHardReference(final boolean useHard) {
        if (isHardReference() != useHard) {
            if (useHard) {
                logger.debug("setHardReference(true): {}", name);
                this.hard = this.weak.get();
            } else {
                logger.debug("setHardReference(false): {}", name);
                this.hard = null;
            }
        }
    }

    private void updateTimeStamp() {
        this.timestamp = System.currentTimeMillis();
    }

    public long getTimestamp() {
        return timestamp;
    }

    public boolean isTimestampValid(final long maxDuration) {
        if (isHardReference()) {
            return true; // always valid
        }
        return (System.currentTimeMillis() - getTimestamp()) <= maxDuration;
    }

}
