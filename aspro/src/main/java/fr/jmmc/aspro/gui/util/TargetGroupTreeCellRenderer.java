/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.gui.util;

import fr.jmmc.aspro.model.oi.TargetGroup;
import java.awt.Color;
import java.awt.Component;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;

/**
 * This custom renderer defines the target group label and color
 * @author bourgesl
 */
public final class TargetGroupTreeCellRenderer extends TargetTreeCellRenderer {

    /** default serial UID for Serializable interface */
    private static final long serialVersionUID = 1;

    /**
     * Public constructor
     * @param renderer target renderer
     */
    public TargetGroupTreeCellRenderer(final TargetRenderer renderer) {
        super(renderer);
    }

    /**
     * Configures the renderer based on the passed in components.
     * The value is set from messaging the tree with
     * <code>convertValueToText</code>, which ultimately invokes
     * <code>toString</code> on <code>value</code>.
     * The foreground color is set based on the selection and the icon
     * is set based on the <code>leaf</code> and <code>expanded</code>
     * parameters.
     */
    @Override
    public Component getTreeCellRendererComponent(
            final JTree tree,
            final Object value,
            final boolean sel,
            final boolean expanded,
            final boolean leaf,
            final int row,
            final boolean hasFocus) {

        super.getTreeCellRendererComponent(
                tree, value, sel,
                expanded, leaf, row,
                hasFocus);

        final DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;

        if (node.getUserObject() instanceof TargetGroup) {
            final TargetGroup group = (TargetGroup) node.getUserObject();

            final Color color = group.getDecodedColor();
            if (color != null) {
                setForeground(color);
            }
        }

        return this;
    }
}
