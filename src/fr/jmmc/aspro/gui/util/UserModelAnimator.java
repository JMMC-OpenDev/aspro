/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.UserModel;
import fr.jmmc.aspro.service.UserModelData;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.swing.Timer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class provides user model animation service as a singleton
 * 
 * Note: this class must be called within Swing EDT to be thread safe
 * 
 * @author bourgesl
 */
public final class UserModelAnimator {

    /** Class logger */
    private static final Logger logger = LoggerFactory.getLogger(UserModelAnimator.class.getName());
    /** singleton instance */
    private static UserModelAnimator instance = new UserModelAnimator();
    /** default image refresh period = 2 seconds */
    private static final int REFRESH_PERIOD = 2 * 1000;
    /** debug mode for register/unregister actions */
    private static final boolean DEBUG = false;

    /**
     * Return the singleton instance
     * @return UserModelAnimator instance
     */
    public static UserModelAnimator getInstance() {
        return instance;
    }

    /* members */
    /** image refresh Swing timer */
    private final Timer timerImageRefresh;
    /** animation contexts keyed by user model key (= file + checksum) */
    private final Map<String, UserModelAnimationContext> contexts = new LinkedHashMap<String, UserModelAnimationContext>(4);
    /** animation contexts keyed by listener */
    private final Map<UserModelAnimatorListener, UserModelAnimationContext> contextByListener = new IdentityHashMap<UserModelAnimatorListener, UserModelAnimationContext>(4);

    /**
     * Private constructor (singleton)
     */
    private UserModelAnimator() {
        super();

        // Create the timeline refresh timer:
        this.timerImageRefresh = new Timer(REFRESH_PERIOD, new ActionListener() {
            /**
             * Invoked when the timer action occurs.
             */
            @Override
            public void actionPerformed(final ActionEvent ae) {
                notifyListeners();
            }
        });
    }

    /**
     * Notify listeners if any
     */
    private void notifyListeners() {
        for (final Iterator<UserModelAnimationContext> it = contexts.values().iterator(); it.hasNext();) {
            final UserModelAnimationContext context = it.next();

            final List<UserModelData> modelDataList = context.userModel.getModelDataList();

            if (modelDataList != null) {
                final int size = modelDataList.size();

                if (context.imageIndex < 0 || context.imageIndex >= size) {
                    context.imageIndex = 0;
                }

                // invoke listeners:
                for (UserModelAnimatorListener listener : context.actionListeners) {
                    listener.perform(context.userModel.getFile(), context.imageIndex);
                }

                // use next image:
                context.imageIndex++;
            }
        }
    }

    /**
     * Register the given animator listener on the given user model
     * @param userModel user model to listen to
     * @param listener animator listener to register or update
     */
    public void register(final UserModel userModel, final UserModelAnimatorListener listener) {
        UserModelAnimationContext oldContext = contextByListener.get(listener);

        if (oldContext != null) {
            if (userModel.getFile().equals(oldContext.userModel.getFile()) && userModel.getChecksum() == oldContext.userModel.getChecksum()) {
                // fast return = nothing to do.
                return;
            }
            // listener is present but different model:
            if (oldContext.actionListeners.remove(listener)) {
                if (oldContext.actionListeners.isEmpty()) {
                    // remove context:
                    contexts.remove(oldContext.userModelKey);
                }
            }
        }

        final String key = getUserModelKey(userModel);

        oldContext = contexts.get(key);
        if (oldContext != null) {
            // context is present:
            oldContext.actionListeners.add(listener);

            // update:
            contextByListener.put(listener, oldContext);
        } else {
            final UserModelAnimationContext context = new UserModelAnimationContext();
            context.userModelKey = key;
            context.userModel = userModel;
            context.actionListeners.add(listener);

            // update:
            contextByListener.put(listener, context);
            contexts.put(key, context);
        }

        // anyway start or stop timer:
        enableImageRefreshTimer(!contexts.isEmpty());

        if (DEBUG) {
            logger.info("register: contexts:\n{}", contexts);
        }
    }

    /**
     * Unregister the given animator listener on any user model
     * @param listener animator listener to remove
     */
    public void unregister(final UserModelAnimatorListener listener) {
        final UserModelAnimationContext oldContext = contextByListener.remove(listener);

        if (oldContext != null) {
            // listener is present:
            if (oldContext.actionListeners.remove(listener)) {
                if (oldContext.actionListeners.isEmpty()) {
                    // remove context:
                    contexts.remove(oldContext.userModelKey);
                }
            }
        }

        // anyway start or stop timer:
        enableImageRefreshTimer(!contexts.isEmpty());

        if (DEBUG) {
            logger.info("unregister: contexts:\n{}", contexts);
        }
    }

    /**
     * Start/Stop the internal image Refresh timer
     * @param enable true to enable it, false otherwise
     */
    private void enableImageRefreshTimer(final boolean enable) {
        if (enable) {
            if (!this.timerImageRefresh.isRunning()) {
                logger.debug("Starting timer: {}", this.timerImageRefresh);

                this.timerImageRefresh.start();
            }
        } else {
            if (this.timerImageRefresh.isRunning()) {
                logger.debug("Stopping timer: {}", this.timerImageRefresh);

                this.timerImageRefresh.stop();
            }
        }
    }

    /**
     * Compyte an user model key (key in context map)
     * @param userModel user model to use
     * @return user model key
     */
    private String getUserModelKey(final UserModel userModel) {
        return userModel.getFile() + '#' + userModel.getChecksum();
    }

    /**
     * Return true if the (internal) timer is running i.e. animator is running
     * @return true if the (internal) timer is running 
     */
    public boolean isRunning() {
        return this.timerImageRefresh.isRunning();
    }

    /**
     * Return the (internal) timer delay, in milliseconds,
     * @return timer delay, in milliseconds,
     */
    public int getDelay() {
        return this.timerImageRefresh.getDelay();
    }

    /**
     * Define the (internal) timer delay, in milliseconds,
     * @param delay timer delay, in milliseconds,
     */
    public void setDelay(final int delay) {
        this.timerImageRefresh.setDelay(delay);
    }

    /**
     * Simple user model context related to animation
     */
    private static final class UserModelAnimationContext {

        /** user model key (= file + checksum) */
        String userModelKey;
        /** user model file */
        UserModel userModel;
        /** action listeners */
        List<UserModelAnimatorListener> actionListeners = new ArrayList<UserModelAnimatorListener>(2); // only few listeners
        /** current image index */
        int imageIndex = 1;

        @Override
        public String toString() {
            return "UserModelAnimationContext[" + userModel.getFile() + "][index=" + imageIndex + "]" + actionListeners;
        }
    }

    /**
     * User model animator listener interface
     */
    public interface UserModelAnimatorListener {

        /**
         * Perform image refresh on the given user model and image index (always on [0; modelDataList.size - 1]
         * @param userModelFile user model file to use
         * @param imageIndex image index to display
         */
        public void perform(final String userModelFile, final int imageIndex);
    }
}
