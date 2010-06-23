/*******************************************************************************
 * JMMC project
 *
 * "@(#) $Id: XmlFactory.java,v 1.1 2010-06-23 12:49:06 bourgesl Exp $"
 *
 * History
 * -------
 * $Log: not supported by cvs2svn $
 *
 */
package fr.jmmc.aspro.util;

import java.io.File;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;

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
 * @author bourgesl
 */
public final class XmlFactory {
  /* constants */

  /** Class Name */
  private static final String className_ = "fr.jmmc.aspro.util.XmlFactory";
  /** Class logger */
  private static final java.util.logging.Logger logger = java.util.logging.Logger.getLogger(
          className_);
  /** encoding used for XML and XSL documents */
  public static final String ENCODING = "UTF-8";
  /** default buffer size for XSLT result document */
  public static final int DEFAULT_BUFFER_SIZE = 4096;
  /** use the class loader (true) or the file system (false) to resolve XSL file paths */
  private static final boolean USE_CLASSLOADER = true;
  /** inner XSLT factory */
  private static TransformerFactory transformerFactory = null;
  /** cache for Xsl templates */
  private static Map<String, Templates> cachedTemplates = new HashMap<String, Templates>(32);

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
   */
  protected static final TransformerFactory getTransformerFactory() {
    if (transformerFactory == null) {
      try {
        transformerFactory = TransformerFactory.newInstance();
      } catch (final TransformerFactoryConfigurationError tfce) {
        logger.log(Level.SEVERE, "XmlFactory.getTransformerFactory : failure on TransformerFactory initialisation : ", tfce);
      }
    }

    return transformerFactory;
  }

  /**
   * Returns a transformer for the given xslt source
   *
   * @param source stream source for xslt script
   *
   * @return transformer for the given xslt source
   */
  protected static final Transformer newTransformer(final StreamSource source) {
    try {
      return getOutTransformer(getTransformerFactory().newTransformer(source));
    } catch (final TransformerConfigurationException tce) {
      logger.log(Level.SEVERE, "XmlFactory.newTransformer : failure on creating new Transformer for source : " + source, tce);
    }

    return null;
  }

  /**
   * Returns a new xslt template (precompiled xslt script) for the given xslt source
   *
   * @param source stream source for xslt script
   *
   * @return new xslt template
   */
  protected static final Templates newTemplate(final StreamSource source) {
    try {
      return getTransformerFactory().newTemplates(source);
    } catch (final TransformerConfigurationException tce) {
      logger.log(Level.SEVERE, "XmlFactory.newTransformer : failure on creating new template : " + source, tce);
    }

    return null;
  }

  /**
   * Returns a transformer for the given xslt template (precompiled xslt script)
   *
   * @param tmp xslt template (precompiled xslt script)
   *
   * @return transformer for the given xslt template
   */
  protected static final Transformer newTransformer(final Templates tmp) {
    try {
      return getOutTransformer(tmp.newTransformer());
    } catch (final TransformerConfigurationException tce) {
      logger.log(Level.SEVERE, "XmlFactory.newTransformer : failure on creating new Transformer for template : " + tmp, tce);
    }

    return null;
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
   * Process xslt on xml document
   *
   * @param xmlSource XML content to transform
   * @param xslFilePath XSL file to use (XSLT)
   *
   * @return result document
   */
  public static String transform(final String xmlSource, final String xslFilePath) {
    return transform(xmlSource, xslFilePath, true);
  }

  /**
   * Process xslt on xml document XSL code can use parameter 'lastModified'
   *
   * @param xmlSource XML content to transform
   * @param xslFilePath XSL file to use (XSLT)
   * @param doCacheXsl true indicates that XSLT can be keep in permanent cache for reuse (avoid a lot of wasted time
   *        (compiling xslt) for many transformations)
   *
   * @return result document
   */
  public static String transform(final String xmlSource, final String xslFilePath, final boolean doCacheXsl) {
    final StringWriter out = new StringWriter(DEFAULT_BUFFER_SIZE);

    transform(xmlSource, xslFilePath, doCacheXsl, out);

    return out.toString();
  }

  /**
   * Process xslt on xml document XSL code can use parameter 'lastModified'
   *
   * @param xmlSource XML content to transform
   * @param xslFilePath XSL file to use (XSLT)
   * @param doCacheXsl true indicates that XSLT can be keep in permanent cache for reuse (avoid a lot of wasted time
   *        (compiling xslt) for many transformations)
   * @param out buffer (should be cleared before method invocation)
   */
  private static void transform(final String xmlSource, final String xslFilePath,
                                final boolean doCacheXsl, final Writer out) {
    if (logger.isLoggable(Level.FINE)) {
      logger.fine("XmlFactory.transform : enter : xslFilePath : " + xslFilePath);
    }

    if ((xmlSource != null) && (xslFilePath != null)) {
      Transformer tf = null;

      if (doCacheXsl) {
        tf = loadXsl(xslFilePath);
      } else {
        final StreamSource source = resolvePath(xslFilePath);
        if (source != null) {
          tf = newTransformer(source);
        }
      }

      if (tf != null) {
        if (logger.isLoggable(Level.FINE)) {
          logger.fine("XmlFactory.transform : XML Source : " + xmlSource);
        }

        asString(tf, new StreamSource(new StringReader(xmlSource)), out);
      }
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("XmlFactory.transform : exit : " + out);
    }
  }

  /**
   * Loads xsl template with cache
   *
   * @param xslFilePath XSL file to use (XSLT)
   *
   * @return transformer or null if file does not exist or xslt not valid
   */
  private static final Transformer loadXsl(final String xslFilePath) {
    if ((xslFilePath == null) || (xslFilePath.length() == 0)) {
      logger.log(Level.SEVERE, "XmlFactory.loadXsl : unable to load template : empty file name !");

      return null;
    }

    Transformer tf = null;
    Templates tmp = cachedTemplates.get(xslFilePath);

    if (tmp == null) {

      try {
        final StreamSource source = resolvePath(xslFilePath);
        if (source != null) {
          tmp = newTemplate(source);
          cachedTemplates.put(xslFilePath, tmp);

          if (logger.isLoggable(Level.FINE)) {
            logger.fine("XmlFactory.loadXsl : template : " + Integer.toHexString(tmp.hashCode()));
          }
        }
      } catch (final Exception e) {
        logger.log(Level.SEVERE, "XmlFactory.loadXsl : unable to create template for XSL : " + xslFilePath, e);
      }
    }

    if (tmp != null) {
      if (logger.isLoggable(Level.FINE)) {
        logger.fine("XmlFactory.loadXsl : template in cache : " + Integer.toHexString(tmp.hashCode()));
      }

      tf = newTransformer(tmp);
    }

    if (logger.isLoggable(Level.FINE)) {
      logger.fine("XmlFactory.loadXsl : xslt : " + tf);
    }

    return tf;
  }

  /**
   * Resolve the XSL file path (using class loader)
   *
   * @param xslFilePath XSL file to use (XSLT)
   *
   * @return StreamSource instance or null
   */
  private static StreamSource resolvePath(final String xslFilePath) {
    StreamSource source = null;
    if ((xslFilePath == null) || (xslFilePath.length() == 0)) {
      logger.log(Level.SEVERE, "XmlFactory.resolvePath : unable to load XSLT : empty path !");

      return null;
    }
    if (USE_CLASSLOADER) {
      try {
        final URL url = FileUtils.getResource(xslFilePath);

        if (logger.isLoggable(Level.FINE)) {
          logger.fine("XmlFactory.resolvePath : url : " + url);
        }

        source = new StreamSource(url.openStream());
      } catch (Exception e) {
        logger.log(Level.SEVERE, "XmlFactory.resolvePath : unable to load XSLT : " + xslFilePath, e);
      }
    } else {
      final File file = new File(xslFilePath);

      if (!file.exists()) {
        logger.log(Level.SEVERE, "XmlFactory.resolvePath : unable to load XSLT : no file found for : " + xslFilePath);

        return null;
      }

      if (logger.isLoggable(Level.FINE)) {
        logger.fine("XmlFactory.resolvePath : file : " + file);
      }
      source = new StreamSource(file);
    }

    return source;
  }

  /**
   * Converts source xml document into the out writer with given transformer
   *
   * @param transformer XSL transformer to use
   * @param source xml document
   * @param out buffer (should be cleared before method invocation)
   *
   * @throws RuntimeException if TransformerException
   */
  private static void asString(final Transformer transformer, final Source source, final Writer out) {
    try {
      transformer.transform(source, new StreamResult(out));
    } catch (final TransformerException te) {
      throw new RuntimeException("XmlFactory.asString : transformer failure :", te);
    }
  }
}
//~ End of file --------------------------------------------------------------------------------------------------------

