/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.Target;
import fr.jmmc.aspro.model.oi.TargetGroup;
import fr.jmmc.aspro.model.oi.TargetGroupMembers;
import fr.jmmc.aspro.model.oi.TargetInformation;
import fr.jmmc.aspro.model.oi.TargetUserInformations;
import fr.jmmc.aspro.model.util.TargetUtils;
import fr.jmmc.jmcs.util.ColorEncoder;
import fr.jmmc.jmcs.util.NumberUtils;
import fr.jmmc.jmcs.util.StringUtils;
import fr.jmmc.oitools.image.FitsUnit;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.util.List;
import javax.swing.JList;
import javax.swing.ListModel;

/**
 * Custom JList to provide tooltips on Target instances
 */
@SuppressWarnings("unchecked")
public final class TargetList extends JList {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;

    private static final Target DEF_TARGET;

    static {
        DEF_TARGET = new Target();
        DEF_TARGET.updateNameAndIdentifier("HIP 123456");
    }

    /* members */
    /** tooltip buffer */
    private final StringBuilder sbToolTip = new StringBuilder(512);
    /** last item index at the mouse position */
    private int lastIndex;
    /** last tooltip at item index */
    private String lastTooltip;
    /** (optional) target user informations */
    private TargetUserInformations targetUserInfos = null;

    public TargetList() {
        super();
        // Useful to define the empty list width and height :
        setPrototypeCellValue(DEF_TARGET);
    }

    /** 
     * update model and reset last tooltip
     * @param model model to set
     */
    @Override
    public void setModel(final ListModel model) {
        super.setModel(model);

        // reset last tooltip:
        lastIndex = -1;
        lastTooltip = null;
    }

    public TargetUserInformations getTargetUserInfos() {
        return targetUserInfos;
    }

    public void setTargetUserInfos(final TargetUserInformations targetUserInfos) {
        this.targetUserInfos = targetUserInfos;
    }

    /** 
     * This method is called as the cursor moves within the component
     * @param me mouse event
     * @return tooltip text
     */
    @Override
    public String getToolTipText(final MouseEvent me) {
        final Point pt = me.getPoint();
        // Get item index :
        final int index = locationToIndex(pt);
        if (index != -1) {
            // check cell bounds:
            if (getCellBounds(index, index + 1).contains(pt)) {
                if (lastIndex == index) {
                    // use last tooltip:
                    return lastTooltip;
                } else {
                    String tooltip = null;
                    // Get target :
                    final Target target = (Target) getModel().getElementAt(index);
                    if (target != null) {
                        // Return the tool tip text:
                        tooltip = getTooltip(sbToolTip, target, targetUserInfos);
                    }
                    lastIndex = index;
                    lastTooltip = tooltip;
                    return tooltip;
                }
            }
        }
        return getToolTipText();
    }

    public static String getTooltip(final StringBuilder sb, final Target target,
                                    final TargetUserInformations targetUserInfos) {
        sb.setLength(0); // clear

        sb.append("<html>");

        getTooltipPart(sb, true, target, targetUserInfos);

        sb.append("</html>");

        return sb.toString();
    }

    public static void getTooltipPart(final StringBuilder sb, final boolean full, final Target target,
                                      final TargetUserInformations targetUserInfos) {

        target.toHtml(sb, full);

        // Target user info:
        if (targetUserInfos != null) {
            // Add group list:
            boolean startGroups = true;
            int n = 0;

            for (TargetGroup group : targetUserInfos.getDisplayGroups()) {
                TargetGroupMembers gm = targetUserInfos.getGroupMembers(group);

                if (gm != null && gm.hasTarget(target)) {
                    if (startGroups) {
                        startGroups = false;
                        sb.append("<hr><b>Groups</b>:");
                    }
                    sb.append("&nbsp;<span style=\"color:")
                            .append(ColorEncoder.encode(group.getOverDecodedColor()))
                            .append(";background:")
                            .append(ColorEncoder.encode(group.getDecodedColor()))
                            .append("\">&nbsp;")
                            .append(group.getName())
                            .append("&nbsp;</span>");
                    if (++n == 4) {
                        sb.append("<br>");
                        n = 0;
                    }
                }
            }

            final TargetInformation targetInfo = targetUserInfos.getTargetInformation(target);
            if (targetInfo != null) {
                boolean startDist = true;
                // Distances to calibrator stars:
                List<Target> targets = targetInfo.hasCalibrators() ? targetInfo.getCalibrators() : null;
                if (targets != null) {
                    if (startDist) {
                        startDist = false;
                        sb.append("<hr>");
                    } else {
                        sb.append("<br>");
                    }
                    addDistances(sb, target, targets, "Calibrators");
                }
                // Distances to ancillary stars:
                // AO
                targets = TargetUserInformations.getTargetsForGroup(targetUserInfos, targetInfo, TargetGroup.GROUP_AO);
                if (targets != null) {
                    if (startDist) {
                        startDist = false;
                        sb.append("<hr>");
                    } else {
                        sb.append("<br>");
                    }
                    addDistances(sb, target, targets, "AO stars");
                }
                // FT
                targets = TargetUserInformations.getTargetsForGroup(targetUserInfos, targetInfo, TargetGroup.GROUP_FT);
                if (targets != null) {
                    if (startDist) {
                        startDist = false;
                        sb.append("<hr>");
                    } else {
                        sb.append("<br>");
                    }
                    addDistances(sb, target, targets, "FT stars");
                }
                // GUIDE
                targets = TargetUserInformations.getTargetsForGroup(targetUserInfos, targetInfo, TargetGroup.GROUP_GUIDE);
                if (targets != null) {
                    if (startDist) {
                        startDist = false;
                        sb.append("<hr>");
                    } else {
                        sb.append("<br>");
                    }
                    addDistances(sb, target, targets, "Guide stars");
                }
            }

            // Add target notes:
            final String userDescription = targetUserInfos.getDescription(target);
            if (userDescription != null) {
                sb.append("<hr><b>Notes</b>:<br>").append(StringUtils.replaceCR(userDescription, "<br>"));
            }
        }
    }

    public static void addDistances(final StringBuilder sb, final Target refTarget, final List<Target> targets, final String prefix) {
        sb.append("<b>").append(prefix).append("</b>: ");

        int n = 0;
        for (Target t : targets) {
            final double dist = TargetUtils.computeDistanceInDegrees(refTarget, t);
            final FitsUnit axisUnit = FitsUnit.getAngleDegUnit(dist);

            if (n == 3) {
                sb.append("<br>");
                n = 0;
            }
            sb.append(t.getName()).append(" (");
            sb.append(NumberUtils.trimTo3Digits(FitsUnit.ANGLE_DEG.convert(dist, axisUnit)));
            sb.append(" ").append(axisUnit.getStandardRepresentation());
            sb.append("), ");
            n++;
        }
        sb.deleteCharAt(sb.length() - 2);
    }
}
