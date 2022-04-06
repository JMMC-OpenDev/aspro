
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *                 This type describes an interferometer file
 *             
 * 
 * <p>Java class for InterferometerFile complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="InterferometerFile"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="file" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *         &lt;element name="checksum" type="{http://www.w3.org/2001/XMLSchema}long"/&gt;
 *         &lt;element name="enabled" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "InterferometerFile", propOrder = {
    "file",
    "checksum",
    "enabled"
})
public class InterferometerFile
    extends OIBase
{

    @XmlElement(required = true)
    protected String file;
    protected long checksum;
    protected Boolean enabled;

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
     * Gets the value of the enabled property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isEnabled() {
        return enabled;
    }

    /**
     * Sets the value of the enabled property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setEnabled(Boolean value) {
        this.enabled = value;
    }
    
//--simple--preserve
    /** user config flag (read only) */
    @javax.xml.bind.annotation.XmlTransient
    private boolean userConfig = false;

    /**
     * @return user config flag (read only)
     */
    public boolean isUserConfig() {
        return userConfig;
    }

    /**
     * Define the user config flag (read only)
     * @param userConfig user config flag (read only)
     */
    public void setUserConfig(boolean userConfig) {
        this.userConfig = userConfig;
    }

    /**
     * Use this method instead of isEnabled()
     * @return true if enabled flag is null or true
     */
    public boolean isReallyEnabled() {
        return (enabled == null) || enabled.booleanValue();
    }

    /** file checksum (read only) */
    @javax.xml.bind.annotation.XmlTransient
    private long checksumFile = 0L;

    /**
     * @return file checksum
     */
    public long getChecksumFile() {
        return checksumFile;
    }

    /**
     * Define the file checksum
     * @param checksumFile file checksum
     */
    public void setChecksumFile(long checksumFile) {
        this.checksumFile = checksumFile;
    }

    /** interferometer name from loaded configuration (read only) */
    @javax.xml.bind.annotation.XmlTransient
    private String interferometerName = null;

    /**
     * @return interferometer name from loaded configuration (read only)
     */
    public String getInterferometerName() {
        return interferometerName;
    }

    /**
     * Define the interferometer name from loaded configuration (read only)
     * @param interferometerName interferometer name
     */
    public void setInterferometerName(String interferometerName) {
        this.interferometerName = interferometerName;
    }

    /**
     * Return true if the interferometer name is valid (configuration loaded)
     * @return true if the interferometer name is valid (configuration loaded)
     */
    public boolean isValid() {
        return getInterferometerName() != null;
    }

    @Override
    public final String toString() {
        return "InterferometerFile [" + file + "]{"
                + "checksum: " + checksumFile
                + " enabled: " + isReallyEnabled()
                + " interferometerName: " + interferometerName
                + "}";
    }
//--simple--preserve

}
