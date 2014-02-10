/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.jaxb;

import com.sun.xml.bind.marshaller.NamespacePrefixMapper;

/**
 * This class customizes the xml schema prefixes
 * @see NamespacePrefixMapper
 *
 * The main problem consists in extending the correct JAXB implementation :
 * - JAXB 2.x
 * import com.sun.xml.bind.marshaller.NamespacePrefixMapper;
 *
 * - JDK 6
 * import com.sun.xml.internal.bind.marshaller.NamespacePrefixMapper;
 * 
 * @author bourgesl
 */
public final class AsproCustomPrefixMapper extends NamespacePrefixMapper {

  /** singleton instance */
  private final static AsproCustomPrefixMapper instance = new AsproCustomPrefixMapper();
  /** ASPRO namespace uris */
  public final static String SCHEMA_ASPRO = "http://www.jmmc.fr/aspro-oi/0.1";
  /** TARGET MODEL namespace uris */
  public final static String SCHEMA_TARGET_MODEL = "http://www.jmmc.fr/jmcs/models/0.1";

  /* members */
  /** predeclared prefixes / namespace uris */
  private final String[] preDeclaredNamespaceUris2 = new String[]{"xsi", "http://www.w3.org/2001/XMLSchema-instance"};

  /**
   * Return the singleton instance
   * @return singleton instance
   */
  public final static NamespacePrefixMapper getInstance() {
    return instance;
  }

  /**
   * Public constructor to be used like :
   *     try {
   *    m.setProperty("com.sun.xml.bind.namespacePrefixMapper", AsproCustomPrefixMapper.getInstance());
   *  } catch (PropertyException pe) {
   *    // if the JAXB provider doesn't recognize the prefix mapper,
   *    // it will throw this exception. Since being unable to specify
   *    // a human friendly prefix is not really a fatal problem,
   *    // you can just continue marshalling without failing
   *
   *    logger.warn("jaxb property failure", pe);
   *  }
   */
  private AsproCustomPrefixMapper() {
    super();
  }

  /**
   * Similar to {@link #getPreDeclaredNamespaceUris()} but allows the
   * (prefix,nsUri) pairs to be returned.
   *
   * <p>
   * With {@link #getPreDeclaredNamespaceUris()}, applications who wish to control
   * the prefixes as well as the namespaces needed to implement both
   * {@link #getPreDeclaredNamespaceUris()} and {@link #getPreferredPrefix(String, String, boolean)}.
   *
   * <p>
   * This version eliminates the needs by returning an array of pairs.
   *
   * @return
   *      always return a non-null (but possibly empty) array. The array stores
   *      data like (prefix1,nsUri1,prefix2,nsUri2,...) Use an empty string to represent
   *      the empty namespace URI and the default prefix. Null is not allowed as a value
   *      in the array.
   *
   * @since
   *      JAXB RI 2.0 beta
   */
  @Override
  public String[] getPreDeclaredNamespaceUris2() {
    return preDeclaredNamespaceUris2;
  }

  /**
   * Returns a preferred prefix for the given namespace URI.
   *
   * This method is intended to be overrided by a derived class.
   *
   *
   * <p>
   * As noted in the return value portion of the javadoc, there
   * are several cases where the preference cannot be honored.
   * Specifically, as of JAXB RI 2.0 and onward:
   *
   * <ol>
   * <li>
   * If the prefix returned is already in use as one of the in-scope
   * namespace bindings. This is partly necessary for correctness
   * (so that we don't unexpectedly change the meaning of QNames
   * bound to {@link String}), partly to simplify the marshaller.
   * <li>
   * If the prefix returned is "" yet the current JAXBContext
   * includes classes that use the empty namespace URI. This allows
   * the JAXB RI to reserve the "" prefix for the empty namespace URI,
   * which is the only possible prefix for the URI.
   * This restriction is also to simplify the marshaller.
   * </ol>
   *
   * @param namespaceUri
   *      The namespace URI for which the prefix needs to be found.
   *      Never be null. "" is used to denote the default namespace.
   * @param suggestion
   *      When the content tree has a suggestion for the prefix
   *      to the given namespaceUri, that suggestion is passed as a
   *      parameter. Typicall this value comes from the QName.getPrefix
   *      to show the preference of the content tree. This parameter
   *      may be null, and this parameter may represent an already
   *      occupied prefix.
   * @param requirePrefix
   *      If this method is expected to return non-empty prefix.
   *      When this flag is true, it means that the given namespace URI
   *      cannot be set as the default namespace.
   *
   * @return
   *      null if there's no prefered prefix for the namespace URI.
   *      In this case, the system will generate a prefix for you.
   *
   *      Otherwise the system will try to use the returned prefix,
   *      but generally there's no guarantee if the prefix will be
   *      actually used or not.
   *
   *      return "" to map this namespace URI to the default namespace.
   *      Again, there's no guarantee that this preference will be
   *      honored.
   *
   *      If this method returns "" when requirePrefix=true, the return
   *      value will be ignored and the system will generate one.
   *
   * @since JAXB 1.0.1
   */
  public String getPreferredPrefix(final String namespaceUri, final String suggestion, final boolean requirePrefix) {

    if (SCHEMA_ASPRO.equals(namespaceUri)) {
      return "a";
    }
    if (SCHEMA_TARGET_MODEL.equals(namespaceUri)) {
      return "tm";
    }

    return suggestion;
  }
}
