
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes an user model (FITS image or cube).
 *       
 * 
 * <p>Java class for UserModel complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="UserModel">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="name" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="description" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="file" type="{http://www.w3.org/2001/XMLSchema}string"/>
 *         &lt;element name="checksum" type="{http://www.w3.org/2001/XMLSchema}long"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "UserModel", propOrder = {
    "name",
    "description",
    "file",
    "checksum"
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
    return modelDataList != null && !modelDataList.isEmpty();
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
  
//--simple--preserve

}
