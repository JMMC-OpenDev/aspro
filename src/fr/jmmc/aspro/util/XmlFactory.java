/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.util;

import fr.jmmc.jmcs.util.FileUtils;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

/**
 * Utility class for XSL transformations
 *
 * @author bourgesl
 */
public final class XmlFactory {

  /** Class logger */
  private static final Logger logger = LoggerFactory.getLogger(XmlFactory.class.getName());
  /** encoding used for XML and XSL documents */
  public static final String ENCODING = "UTF-8";
  /** default buffer size for XSLT result document */
  public static final int DEFAULT_BUFFER_SIZE = 16384;
  /** inner XSLT factory */
  private static TransformerFactory transformerFactory = null;
  /** cache for Xsl templates */
  private static final Map<String, Templates> cachedTemplates = new HashMap<String, Templates>(32);

  /**
   * Forbidden constructor
   */
  private XmlFactory() {
    /* no-op */
  }

  /**
   * Returns a TransformerFactory (JAXP)
   *
   * @return TransformerFactory (JAXP)
   *
   * @throws IllegalStateException if TransformerFactory initialization failed
   */
  protected static final TransformerFactory getTransformerFactory()
          throws IllegalStateException {

    if (transformerFactory == null) {
      try {
        transformerFactory = TransformerFactory.newInstance();
      } catch (TransformerFactoryConfigurationError tfce) {
        throw new IllegalStateException("XmlFactory.getTransformerFactory : failure on TransformerFactory initialisation : ", tfce);
      }
    }

    return transformerFactory;
  }

  /**
   * Returns a new xslt template (precompiled xslt script) for the given xslt source
   *
   * @param source stream source for xslt script
   *
   * @return new xslt template
   *
   * @throws IllegalStateException if TransformerFactory initialization failed or template creation failed
   */
  protected static final Templates newTemplate(final StreamSource source)
          throws IllegalStateException {

    try {
      return getTransformerFactory().newTemplates(source);
    } catch (TransformerConfigurationException tce) {
      throw new IllegalStateException("XmlFactory.newTransformer : failure on creating new template : " + source, tce);
    }
  }

  /**
   * Returns a transformer for the given xslt template (precompiled xslt script)
   *
   * @param tmp xslt template (precompiled xslt script)
   *
   * @return transformer for the given xslt template
   *
   * @throws IllegalStateException if transformer creation failed
   */
  protected static final Transformer newTransformer(final Templates tmp)
          throws IllegalStateException {

    try {
      return getOutTransformer(tmp.newTransformer());
    } catch (TransformerConfigurationException tce) {
      throw new IllegalStateException("XmlFactory.newTransformer : failure on creating new Transformer for template : " + tmp, tce);
    }
  }

  /**
   * Returns a transformer for the given xslt source
   *
   * @param source stream source for xslt script
   *
   * @return transformer for the given xslt source
   *
   * @throws IllegalStateException if TransformerFactory initialization failed or transformer creation failed
   */
  protected static final Transformer newTransformer(final StreamSource source)
          throws IllegalStateException {

    try {
      return getOutTransformer(getTransformerFactory().newTransformer(source));
    } catch (TransformerConfigurationException tce) {
      throw new IllegalStateException("XmlFactory.newTransformer : failure on creating new Transformer for source : " + source, tce);
    }
  }

  /**
   * Sets the encoding and indetation parameters for the given transformer
   *
   * @param tf transformer
   *
   * @return tf transformer
   */
  private static final Transformer getOutTransformer(final Transformer tf) {
    tf.setOutputProperty(OutputKeys.ENCODING, ENCODING);
    tf.setOutputProperty(OutputKeys.INDENT, "yes");

    return tf;
  }

  /**
   * Process xslt on xml document (using xslt cache)
   *
   * @param xmlSource XML content to transform
   * @param xslFilePath XSL file to use (XSLT)
   *
   * @return result document as string
   *
   * @throws IllegalStateException if TransformerFactory initialization failed or template creation failed or transformer creation failed
   * @throws IllegalArgumentException if transformation failure or the xsl file path is empty or IO failure
   */
  public static String transform(final String xmlSource, final String xslFilePath)
          throws IllegalStateException, IllegalArgumentException {

    return transform(xmlSource, xslFilePath, true);
  }

  /**
   * Process xslt on xml document
   *
   * @param xmlSource XML content to transform
   * @param xslFilePath XSL file to use (XSLT)
   * @param doCacheXsl true indicates that XSLT can be keep in permanent cache for reuse (avoid a lot of wasted time
   *        (compiling xslt) for many transformations)
   *
   * @return result document as string
   *
   * @throws IllegalStateException if TransformerFactory initialization failed or template creation failed or transformer creation failed
   * @throws IllegalArgumentException if transformation failure or the xsl file path is empty or IO failure
   */
  public static String transform(final String xmlSource, final String xslFilePath, final boolean doCacheXsl)
          throws IllegalStateException, IllegalArgumentException {

    final StringWriter out = new StringWriter(DEFAULT_BUFFER_SIZE);

    transform(xmlSource, xslFilePath, doCacheXsl, out);

    return out.toString();
  }

  /**
   * Process xslt on xml document
   *
   * @param xmlSource XML content to transform
   * @param xslFilePath XSL file to use (XSLT)
   * @param doCacheXsl true indicates that XSLT can be keep in permanent cache for reuse (avoid a lot of wasted time
   *        (compiling xslt) for many transformations)
   * @param out buffer (should be cleared before method invocation)
   *
   * @throws IllegalStateException if TransformerFactory initialization failed or template creation failed or transformer creation failed
   * @throws IllegalArgumentException if transformation failure or the xsl file path is empty or I/O exception occurs while reading XSLT
   */
  private static void transform(final String xmlSource, final String xslFilePath,
          final boolean doCacheXsl, final Writer out)
          throws IllegalStateException, IllegalArgumentException {

    logger.debug("XmlFactory.transform : enter : xslFilePath : {}", xslFilePath);

    if ((xmlSource != null) && (xslFilePath != null)) {
      final Transformer tf;

      if (doCacheXsl) {
        tf = loadXsl(xslFilePath);
      } else {
        tf = newTransformer(resolveXSLTPath(xslFilePath));
      }

      logger.debug("XmlFactory.transform : XML Source : {}", xmlSource);

      asString(tf, new StreamSource(new StringReader(xmlSource)), out);
    }

    logger.debug("XmlFactory.transform : exit : {}", out);
  }

  /**
   * Load an xslt using template cache
   *
   * @param xslFilePath XSL file to use (XSLT)
   *
   * @return transformer or null if file does not exist
   *
   * @throws IllegalStateException if TransformerFactory initialization failed or template creation failed or transformer creation failed
   * @throws IllegalArgumentException if the xsl file path is empty or I/O exception occurs while reading XSLT
   */
  private static final Transformer loadXsl(final String xslFilePath)
          throws IllegalStateException, IllegalArgumentException {

    if ((xslFilePath == null) || (xslFilePath.length() == 0)) {
      throw new IllegalArgumentException("XmlFactory.resolvePath : unable to load XSLT : empty file path !");
    }

    Transformer tf = null;
    Templates tmp = cachedTemplates.get(xslFilePath);

    if (tmp == null) {
      tmp = newTemplate(resolveXSLTPath(xslFilePath));

      cachedTemplates.put(xslFilePath, tmp);

      if (logger.isDebugEnabled()) {
        logger.debug("XmlFactory.loadXsl : template: {}", Integer.toHexString(tmp.hashCode()));
      }
    }

    if (logger.isDebugEnabled()) {
      logger.debug("XmlFactory.loadXsl : template in cache: {}", Integer.toHexString(tmp.hashCode()));
    }

    tf = newTransformer(tmp);

    logger.debug("XmlFactory.loadXsl : xslt : {}", tf);

    return tf;
  }

  /**
   * Resolve the XSL file path using the class loader
   *
   * @param xslFilePath XSL file to use (XSLT)
   *
   * @return StreamSource instance
   *
   * @throws IllegalStateException if the file is not found
   * @throws IllegalArgumentException if the xsl file path is empty or I/O exception occurs while reading XSLT
   */
  private static StreamSource resolveXSLTPath(final String xslFilePath)
          throws IllegalStateException, IllegalArgumentException {

    if ((xslFilePath == null) || (xslFilePath.length() == 0)) {
      throw new IllegalArgumentException("XmlFactory.resolveXSLTPath : unable to load XSLT : empty file path !");
    }

    final URL url = FileUtils.getResource(xslFilePath);

    logger.debug("XmlFactory.resolveXSLTPath : url : {}", url);

    try {
      return new StreamSource(new BufferedInputStream(url.openStream()));
    } catch (IOException ioe) {
      throw new IllegalArgumentException("XmlFactory.resolveXSLTPath : unable to load the XSLT file : " + xslFilePath, ioe);
    }
  }

  /**
   * Converts source xml document into the out writer with given transformer
   *
   * @param transformer XSL transformer to use
   * @param source xml document
   * @param out buffer (should be cleared before method invocation)
   *
   * @throws IllegalArgumentException if transformation failure
   */
  private static void asString(final Transformer transformer, final Source source, final Writer out)
          throws IllegalArgumentException {
    try {
      transformer.transform(source, new StreamResult(out));
    } catch (TransformerException te) {
      throw new IllegalArgumentException("XmlFactory.asString : transformer failure :", te);
    }
  }
}
//~ End of file --------------------------------------------------------------------------------------------------------

