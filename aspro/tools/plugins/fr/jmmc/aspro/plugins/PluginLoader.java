/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.plugins;

import fr.jmmc.jmcs.util.IntrospectionUtils;
import java.io.File;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.security.Policy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class loads dynamically plugins
 * @author bourgesl
 */
public class PluginLoader {

  /** Class logger */
  protected static final Logger logger = LoggerFactory.getLogger(PluginLoader.class.getName());
  /** singleton */
  private final static PluginLoader pluginLoader = new PluginLoader();

  /**
   * Return the singleton
   */
  public static PluginLoader getInstance() {
    return pluginLoader;
  }

  /* members */
  /**
   * Private constructor
   */
  private PluginLoader() {
    super();
  }

  public Class<?> getClass(final File jarFile, final String className) throws Exception {
    String filePath = jarFile.getAbsolutePath();

//    filePath = "jar:file://" + filePath + "!/";

    final URL url = new URL("file", null, filePath);

    logger.warn("loading Jar url : {}", url);

    // use PluginLoader classloader (JNLP secure ClassLoader) as parent class loader:
    final URLClassLoader clazzLoader = new URLClassLoader(new URL[]{url}, PluginLoader.class.getClassLoader()) /*
             {

             protected PermissionCollection getPermissions(CodeSource codesource) {
             logger.warn("getPermissions : {}", codesource);
        
             PermissionCollection pcol = super.getPermissions(codesource);
             pcol.add(new AllPermission());

             logger.warn("getPermissions : {}", pcol);
             return (pcol);
             }
             }
             */;

    logger.warn("clazzLoader : {}", clazzLoader);

    final Class<?> clazz = clazzLoader.loadClass(className);

    logger.warn("clazz : {}", clazz);

    return clazz;

  }

  public void testCall() {
    if (false) {
      Policy.setPolicy(new PluginPolicy());
      System.setSecurityManager(new SecurityManager());
    }

    final File vegaPluginJar = new File(System.getProperty("user.home") + "/vegaPlugin/CalculSNR.jar");
    final String vegaPluginClass = "calculsnr.Main";

    try {
      Class<?> pluginClass = getClass(vegaPluginJar, vegaPluginClass);

      for (Method m : pluginClass.getDeclaredMethods()) {
        logger.info("method '{}'", m.getName());

        for (Class<?> param : m.getParameterTypes()) {
          logger.info("\tparam '{}'", param);
        }

        logger.info("\treturn '{}'", m.getReturnType());
      }

      final Method mainMethod = IntrospectionUtils.getMethod(pluginClass, "main", new Class<?>[]{String[].class});
      if (mainMethod != null) {
        Object pluginInstance = pluginClass.newInstance();
        mainMethod.invoke(pluginInstance, (String) null);
      }

    } catch (Exception e) {
      logger.error("Plugin fatal error:", e);
    }
  }
}
