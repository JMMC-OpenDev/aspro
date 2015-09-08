/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.service;

import fr.jmmc.aspro.model.HorizonShape;
import fr.jmmc.aspro.model.oi.AzEl;
import fr.jmmc.aspro.model.oi.HorizonProfile;
import fr.jmmc.aspro.model.oi.Station;
import java.awt.Polygon;
import java.awt.Rectangle;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class checks if an azimuth / elevation coordinate is over the telescope horizon
 * @author bourgesl
 */
public final class HorizonService {

    /** singleton instance */
    private static final HorizonService instance = new HorizonService();

    /* members */
    /** cached horizon shapes : low memory impact */
    private final Map<String, HorizonShape> cachedProfiles = new HashMap<String, HorizonShape>(128);

    /**
     * Return the singleton
     * @return singleton
     */
    public static HorizonService getInstance() {
        return instance;
    }

    /**
     * Private constructor
     */
    private HorizonService() {
        /* no-op */
    }

    /**
     * Give the profile corresponding to the given station
     * @param interferometerName interferometer name used to compute a cache key (interferometer name - station name)
     * @param station station to use
     * @return profile or null if there is no horizon profile for the given station
     */
    public HorizonShape getProfile(final String interferometerName, final Station station) {
        // get profile key = '<interferometer> - <station>' :
        final String key = station.getKey(interferometerName);

        HorizonShape profile = cachedProfiles.get(key);

        if (profile == null) {
            final HorizonProfile p = station.getHorizon();

            // As VLT horizons have only integer coordinates, let's use a simple polygon :
            final List<AzEl> points = p.getPoints();

            if (!points.isEmpty()) {
                final int npoints = points.size() + 1;
                final int[] xpoints = new int[npoints];
                final int[] ypoints = new int[npoints];

                // Note : az is given in [0 - 360] range :
                // 0 = meridian (north-south) positive toward east
                int i = 0;
                for (AzEl point : points) {
                    xpoints[i] = (int) point.getAzimuth();
                    ypoints[i] = (int) point.getElevation();
                    i++;
                }
                // close the polygon :
                final AzEl orig = points.get(0);
                xpoints[i] = (int) orig.getAzimuth();
                ypoints[i] = (int) orig.getElevation();

                final Polygon polygon = new Polygon(xpoints, ypoints, npoints) {
                    /** default serial UID for Serializable interface */
                    private static final long serialVersionUID = 1L;

                    /**
                     * Gets the bounding box of this <code>Polygon</code>.
                     * The bounding box is the smallest {@link Rectangle} whose
                     * sides are parallel to the x and y axes of the
                     * coordinate space, and can completely contain the <code>Polygon</code>.
                     * @return a <code>Rectangle</code> that defines the bounds of this
                     * <code>Polygon</code>.
                     * @since 1.1
                     */
                    @Override
                    public Rectangle getBounds() {
                        if (bounds != null) {
                            // avoid creating new Rectangle at each call !!
                            return bounds;
                        }
                        return super.getBounds();
                    }

                    /**
                     * {@inheritDoc}
                     * @since 1.2
                     */
                    @Override
                    public final boolean contains(final double x, final double y) {
                        if (!getBounds().contains(x, y)) {
                            return false;
                        }
                        int hits = 0;

                        // local vars for performance:
                        final int np = npoints;
                        final int[] xp = xpoints;
                        final int[] yp = ypoints;

                        int lastx = xp[np - 1];
                        int lasty = yp[np - 1];
                        int curx, cury;
                        int leftx;
                        double test1, test2;

                        // Walk the edges of the polygon
                        for (int i = 0; i < np; lastx = curx, lasty = cury, i++) {
                            curx = xp[i];
                            cury = yp[i];

                            if (cury == lasty) {
                                continue;
                            }

                            if (curx < lastx) {
                                if (x >= lastx) {
                                    continue;
                                }
                                leftx = curx;
                            } else {
                                if (x >= curx) {
                                    continue;
                                }
                                leftx = lastx;
                            }

                            if (cury < lasty) {
                                if (y < cury || y >= lasty) {
                                    continue;
                                }
                                if (x < leftx) {
                                    hits++;
                                    continue;
                                }
                                test1 = x - curx;
                                test2 = y - cury;
                            } else {
                                if (y < lasty || y >= cury) {
                                    continue;
                                }
                                if (x < leftx) {
                                    hits++;
                                    continue;
                                }
                                test1 = x - lastx;
                                test2 = y - lasty;
                            }

                            if (test1 < (test2 / (lasty - cury) * (lastx - curx))) {
                                hits++;
                            }
                        }

                        return ((hits & 1) != 0);
                    }
                };

                profile = new HorizonShape(key, polygon);

                cachedProfiles.put(key, profile);
            }
        }

        return profile;
    }

    /**
     * Checks if the az/el position in hidden by the given horizon profile
     * @param profile profile horizon to check
     * @param az azimuth (deg : 0 = north, positive toward east)
     * @param elev elevation (deg)
     * @return true if no obstruction
     */
    public boolean checkProfile(final HorizonShape profile, final double az, final double elev) {
    // adjust azimuth to be compatible with vlti profiles :

        // 0 = meridian (north-south) positive toward east :
        final double adjAz = (az < 180d) ? az + 180d : az - 180d;

        return profile.check(adjAz, elev);
    }
}
