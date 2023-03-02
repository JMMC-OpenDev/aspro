
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlID;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.adapters.CollapsedStringAdapter;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes a group of targets.
 *             
 * 
 * <p>Java class for TargetGroup complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="TargetGroup"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="category" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="color" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="id" use="required" type="{http://www.w3.org/2001/XMLSchema}ID" /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "TargetGroup", propOrder = {
    "name",
    "category",
    "description",
    "color"
})
public class TargetGroup
    extends OIBase
{

    @XmlElement(required = true)
    protected String name;
    @XmlElement(required = true)
    protected String category;
    @XmlElement(required = true)
    protected String description;
    protected String color;
    @XmlAttribute(name = "id", required = true)
    @XmlJavaTypeAdapter(CollapsedStringAdapter.class)
    @XmlID
    @XmlSchemaType(name = "ID")
    protected String id;

    /**
     * Gets the value of the name property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the value of the name property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setName(String value) {
        this.name = value;
    }

    /**
     * Gets the value of the category property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCategory() {
        return category;
    }

    /**
     * Sets the value of the category property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCategory(String value) {
        this.category = value;
    }

    /**
     * Gets the value of the description property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDescription() {
        return description;
    }

    /**
     * Sets the value of the description property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDescription(String value) {
        this.description = value;
    }

    /**
     * Gets the value of the color property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getColor() {
        return color;
    }

    /**
     * Sets the value of the color property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setColor(String value) {
        this.color = value;
    }

    /**
     * Gets the value of the id property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getId() {
        return id;
    }

    /**
     * Sets the value of the id property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setId(String value) {
        this.id = value;
    }
    
//--simple--preserve
    public final static String CATEGORY_OB = "[OB]";
    public final static String CATEGORY_SEL = "[SELECTION]";
    public final static String CATEGORY_USER = "[USER]";

    public final static String GROUP_AO = "JMMC_AO";
    public final static String GROUP_FT = "JMMC_FT";
    public final static String GROUP_GUIDE = "JMMC_GUIDE";
    public final static String GROUP_SELECTED = "JMMC_SELECTED";

    /** empty TargetGroup instance */
    public static final TargetGroup EMPTY_GROUP = new TargetGroup();

    /**
     * Empty constructor for JAXB
     */
    public TargetGroup() {
        super();
    }

    /**
     * constructor for internal groups (JMMC)
     * @param id id
     * @param name name
     * @param category category
     * @param description description
     * @param color color
     */
    public TargetGroup(final String id, final String name, final String category, final String description, final String color) {
        super();
        setId(id);
        setName(name);
        setCategory(category);
        setDescription(description);
        setColor(color);
    }

    /**
     * constructor with all fields
     * @param name name
     * @param category category
     * @param description description
     * @param color color
     */
    public TargetGroup(final String name, final String category, final String description, final String color) {
        super();
        setName(name);
        setCategory(category);
        setDescription(description);
        setColor(color);
        updateNameAndIdentifier();
    }

    /**
     * This method returns the group identifier.
     * If it is missing, it generates a new identifier from the name field.
     * @return identifier
     */
    public final String getIdentifier() {
        // ensure id is never null:
        if (this.id == null) {
            setIdentifier();
        }
        return this.id;
    }

    public static String generateIdentifier(final String name) {
        return fr.jmmc.aspro.model.util.XmlIdUtils.convert(Target.formatName(name));
    }

    private final void setIdentifier() {
        setId(fr.jmmc.aspro.model.util.XmlIdUtils.convert(getName()));
    }

    /**
     * Format and update the (internal) name and identifier.
     * Warning: this modified identifier is not updated in relationships
     */
    private final void updateNameAndIdentifier() {
        this.setName(Target.formatName(getName()));
        // recompute identifier:
        setIdentifier();
    }

    public final boolean isCategoryInternal() {
        return isCategoryOB() || isCategorySEL();
    }

    public final boolean isCategoryOB() {
        return CATEGORY_OB.equals(getCategory());
    }

    public final boolean isCategorySEL() {
        return CATEGORY_SEL.equals(getCategory());
    }

    public final boolean isCategoryUSER() {
        return CATEGORY_USER.equals(getCategory());
    }

    /** decoded color */
    @javax.xml.bind.annotation.XmlTransient
    private java.awt.Color decColor = null;

    /** another color over the decoded color */
    @javax.xml.bind.annotation.XmlTransient
    private java.awt.Color overDecColor = null;

    /**
     * Set and reset the decoded color.
     */
    public final void updateColor(final String color) {
        setColor(color);
        decColor = null;
        overDecColor = null;
    }

    public final java.awt.Color getDecodedColor() {
        if (decColor != null) {
            return decColor;
        }
        decColor = java.awt.Color.MAGENTA; // default

        final String col = getColor();

        if (col != null) {
            try {
                decColor = java.awt.Color.decode(col);
            } catch (NumberFormatException nfe) {
                logger.warn("Unable to decode color [{}]", col, nfe);
            }
        }
        return decColor;
    }

    public final java.awt.Color getOverDecodedColor() {
        if (overDecColor != null) {
            return overDecColor;
        }

        final java.awt.Color color = getDecodedColor();

        overDecColor = (fr.jmmc.oiexplorer.core.util.ColorUtils.luminance(color.getRGB()) > 128) ? java.awt.Color.BLACK : java.awt.Color.WHITE;

        return overDecColor;
    }

    /**
     * Return the tooltip describing this group
     * @return tooltip text
     */
    public final String getTooltip() {
        String desc = getDescription();
        if (desc == null || desc.length() == 0) {
            return "Missing description for [" + getName() + ']';
        }
        return desc;
    }

    /**
     * This equals method uses the identifier equality
     * @param obj other object
     * @return true if the identifiers are equals
     */
    @Override
    public final boolean equals(final Object obj) {
        if (obj == null) {
            return false;
        }
        // identity comparison :
        if (this == obj) {
            return true;
        }
        // class check :
        if (getClass() != obj.getClass()) {
            return false;
        }
        final TargetGroup other = (TargetGroup) obj;

        return areEquals(this.getIdentifier(), other.getIdentifier());
    }

    /**
     * This hashcode implementation uses only the id field
     * @return hashcode
     */
    @Override
    public final int hashCode() {
        int hash = 3;
        hash = 71 * hash + (this.getIdentifier() != null ? this.getIdentifier().hashCode() : 0);
        return hash;
    }

    @Override
    public final String toString() {
        return "TargetGroup [" + ((this.getName() != null) ? this.getName() : "undefined") + "][id: " + getIdentifier() + ']';
    }

    /**
     * Return a deep "copy" of this instance
     * @return deep "copy" of this instance
     */
    @Override
    public final Object clone() {
        final TargetGroup copy = (TargetGroup) super.clone();

        // no specific field to deep copy
        return copy;
    }

    @Override
    protected boolean areEquals(final OIBase o) {
        if (!super.areEquals(o)) {
            return false;
        }
        final TargetGroup other = (TargetGroup) o;
        return (areEquals(this.id, other.getId())
                && areEquals(this.name, other.getName())
                && areEquals(this.description, other.getDescription())
                && areEquals(this.color, other.getColor()));
    }

    /**
     * Return the group of the given name in the given list of groups
     * @param name group name
     * @param groups list of groups
     * @return group or null if the group was not found
     */
    public static TargetGroup getGroup(final String name, final java.util.List<TargetGroup> groups) {
        if (name != null) {
            for (TargetGroup g : groups) {
                if (g.getName().equals(name)) {
                    return g;
                }
            }
        }
        return null;
    }

    /**
     * Return the group of the given identifier in the given list of groups
     * @param id group identifier
     * @param groups list of groups
     * @return group or null if the group was not found
     */
    public static TargetGroup getGroupById(final String id, final java.util.List<TargetGroup> groups) {
        if (id != null) {
            for (TargetGroup g : groups) {
                if (g.getIdentifier().equals(id)) {
                    return g;
                }
            }
        }
        return null;
    }

    /**
     * Return the Map<ID, TargetGroup> index
     * @param groups
     * @return Map<ID, TargetGroup> index
     */
    static java.util.Map<String, TargetGroup> createTargetGroupIndex(final java.util.List<TargetGroup> groups) {
        // create the Map<ID, Target> index :
        final java.util.Map<String, TargetGroup> mapIDGroups = new java.util.HashMap<String, TargetGroup>(groups.size());
                
        for (final java.util.ListIterator<TargetGroup> it = groups.listIterator(); it.hasNext();) {
            final TargetGroup group = it.next();
            final String id = group.getIdentifier();
                        
            if (mapIDGroups.containsKey(id)) {
                logger.info("Removing duplicated group reference [{}] = [{}]", id, group);
                it.remove();
            } else {
                mapIDGroups.put(id, group);
            }            
        }
        return mapIDGroups;
    }

    public static void filterGroups(final java.util.List<TargetGroup> groups, final String category) {
        if (category != null) {
            for (final java.util.Iterator<TargetGroup> it = groups.iterator(); it.hasNext();) {
                if (category.equals(it.next().getCategory())) {
                    it.remove();
                }
            }
        }
    }

//--simple--preserve

}
