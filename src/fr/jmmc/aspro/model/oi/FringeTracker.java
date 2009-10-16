
package fr.jmmc.aspro.model.oi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;
import fr.jmmc.aspro.model.OIBase;


/**
 * 
 *         This type describes a Fringe Tracker system :
 * 
 * ! Fringe Tracker
 *   define logical oipt%FTUsed /global ! FT in Use
 *   define real oipt%FTVisibility /global
 *   define real oipt%FTLimit /global
 *   define real oipt%FTMaxInt /global
 *   define integer oipt%FTMode /global
 *   define character oipt%FTBand*1 /global ! FT observing band
 * 
 * Fringe Tracker
 *  let oipt%FTExist YES
 *  let oipt%FTBand "H" ! same for FTs
 *  let oipt%FTUsed NO
 *  let oipt%FTMode 0
 *  let oipt%FTVisibility 0.9
 *  let oipt%FTLimit 12
 *  let oipt%FTMaxInt 60.0
 *       
 * 
 * <p>Java class for FringeTracker complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="FringeTracker">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "FringeTracker")
public class FringeTracker
    extends OIBase
{


}
