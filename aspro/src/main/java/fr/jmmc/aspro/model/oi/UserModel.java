
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes an user model (FITS image or cube).
 *             
 * 
 * <p>Java class for UserModel complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="UserModel"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="file" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="checksum" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="scaleX" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="scaleY" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *         &lt;element name="rotation" type="{http://www.w3.org/2001/XMLSchema}double" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "UserModel", propOrder = {
    "name",
    "description",
    "file",
    "checksum",
    "scaleX",
    "scaleY",
    "rotation"
})
public class UserModel
    extends OIBase
{

    @XmlElement(required = true)
    protected String name;
    protected String description;
    @XmlElement(required = true)
    protected String file;
    protected long checksum;
    protected Double scaleX;
    protected Double scaleY;
    protected Double rotation;

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
     * Gets the value of the file property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFile() {
        return file;
    }

    /**
     * Sets the value of the file property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFile(String value) {
        this.file = value;
    }

    /**
     * Gets the value of the checksum property.
     * 
     */
    public long getChecksum() {
        return checksum;
    }

    /**
     * Sets the value of the checksum property.
     * 
     */
    public void setChecksum(long value) {
        this.checksum = value;
    }

    /**
     * Gets the value of the scaleX property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getScaleX() {
        return scaleX;
    }

    /**
     * Sets the value of the scaleX property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setScaleX(Double value) {
        this.scaleX = value;
    }

    /**
     * Gets the value of the scaleY property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getScaleY() {
        return scaleY;
    }

    /**
     * Sets the value of the scaleY property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setScaleY(Double value) {
        this.scaleY = value;
    }

    /**
     * Gets the value of the rotation property.
     * 
     * @return
     *     possible object is
     *     {@link Double }
     *     
     */
    public Double getRotation() {
        return rotation;
    }

    /**
     * Sets the value of the rotation property.
     * 
     * @param value
     *     allowed object is
     *     {@link Double }
     *     
     */
    public void setRotation(Double value) {
        this.rotation = value;
    }
    
//--simple--preserve
    /** flag indicating that the file reference is valid (readable) */
    @javax.xml.bind.annotation.XmlTransient
    private boolean fileValid = false;

    /**
     * Return the true if the file reference is valid (readable)
     * @return true if the file reference is valid (readable)
     */
    public final boolean isFileValid() {
        return this.fileValid;
    }

    /**
     * Return the true if the file reference is valid (readable)
     * @param valid true if the file reference is valid (readable)
     */
    public final void setFileValid(final boolean valid) {
        this.fileValid = valid;
    }

    /** Cached list of prepared model data corresponding to the file reference (read only) */
    @javax.xml.bind.annotation.XmlTransient
    private java.util.List<fr.jmmc.aspro.service.UserModelData> modelDataList = null;

    /**
     * Return true if the Cached model data is not null nor empty
     * @return true if the Cached model data is not null nor empty
     */
    public boolean isModelDataReady() {
        return !isEmpty(modelDataList);
    }

    /**
     * Return the Cached model data given its image index (read only)
     * @param index image index [0; n[
     * @return Cached model data (read only)
     */
    public fr.jmmc.aspro.service.UserModelData getModelData(final int index) {
        if (isModelDataReady() && index < modelDataList.size()) {
            return modelDataList.get(index);
        }
        return null;
    }

    /**
     * Return the Cached model data corresponding to the file reference (read only)
     * @return Cached model data corresponding to the file reference (read only)
     */
    public java.util.List<fr.jmmc.aspro.service.UserModelData> getModelDataList() {
        return modelDataList;
    }

    /**
     * Define the Cached model data corresponding to the file reference (read only)
     * @param modelData Cached model data corresponding to the file reference (read only)
     */
    public void setModelDataList(final java.util.List<fr.jmmc.aspro.service.UserModelData> modelData) {
        this.modelDataList = modelData;
    }

    /**
     * Return a shallow "copy" of this instance
     * @return shallow "copy" of this instance
     */
    @Override
    public final Object clone() {
        final UserModel copy = (UserModel) super.clone();

        // Note: modelData instances are shallow copies
        if (copy.getModelDataList() != null) {
            copy.setModelDataList(OIBase.copyList(copy.getModelDataList()));
        }

        return copy;
    }

    @Override
    protected boolean areEquals(final OIBase o) {
        if (!super.areEquals(o)) {
            return false;
        }
        final UserModel other = (UserModel) o;
        return (areEquals(this.name, other.getName())
                && areEquals(this.description, other.getDescription())
                && areEquals(this.file, other.getFile())
                && areEquals(this.checksum, other.getChecksum())
                && areEquals(this.scaleX, other.getScaleX())
                && areEquals(this.scaleY, other.getScaleY())
                && areEquals(this.rotation, other.getRotation()));
    }

//--simple--preserve

}
