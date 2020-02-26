/*******************************************************************************
 * JMMC project ( http://www.jmmc.fr ) - Copyright (C) CNRS.
 ******************************************************************************/
package fr.jmmc.aspro.plugins;

import java.security.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Custom policy for the security-test.
 * 
 * @author David Roden &lt;droden@gmail.com&gt;
 * @version $Id$
 */
public final class PluginPolicy extends Policy {

  /** Class logger */
  protected static final Logger logger = LoggerFactory.getLogger(PluginPolicy.class.getName());

  /**
   * Returns {@link AllPermission} for any code sources that do not end in
   * “/rogue.jar” and an empty set of permissions for code sources that do end
   * in “/rogue.jar”, denying access to all local resources to the rogue
   * plugin.
   * 
   * @param codeSource
   *            The code source to get the permissiosn for
   * @return The permissions for the given code source
   */
  @Override
  public PermissionCollection getPermissions(final CodeSource codeSource) {
    logger.warn("getPermissions : {}", codeSource);

    Permissions p = new Permissions();
    if (!codeSource.getLocation().toString().endsWith("/CalculSNR.jar")) {
      p.add(new AllPermission());
    }
    logger.warn("getPermissions : {} = {}", codeSource, p);
    return p;
  }

  /**
   * Return a PermissionCollection object containing the set of
   * permissions granted to the specified ProtectionDomain.
   *
   * <p> Applications are discouraged from calling this method
   * since this operation may not be supported by all policy implementations.
   * Applications should rely on the <code>implies</code> method
   * to perform policy checks.
   *
   * <p> The default implementation of this method first retrieves
   * the permissions returned via <code>getPermissions(CodeSource)</code>
   * (the CodeSource is taken from the specified ProtectionDomain),
   * as well as the permissions located inside the specified ProtectionDomain.
   * All of these permissions are then combined and returned in a new
   * PermissionCollection object.  If <code>getPermissions(CodeSource)</code>
   * returns Policy.UNSUPPORTED_EMPTY_COLLECTION, then this method
   * returns the permissions contained inside the specified ProtectionDomain
   * in a new PermissionCollection object.
   *
   * <p> This method can be overridden if the policy implementation
   * supports returning a set of permissions granted to a ProtectionDomain.
   *
   * @param domain the ProtectionDomain to which the returned
   *          PermissionCollection has been granted.
   *
   * @return a set of permissions granted to the specified ProtectionDomain.
   *          If this operation is supported, the returned
   *          set of permissions must be a new mutable instance
   *          and it must support heterogeneous Permission types.
   *          If this operation is not supported,
   *          Policy.UNSUPPORTED_EMPTY_COLLECTION is returned.
   *
   * @since 1.4
   */
  public PermissionCollection getPermissions(ProtectionDomain domain) {
    logger.warn("getPermissions : {} @ {}", domain.getClassLoader(), domain.getCodeSource());
    Permissions p = new Permissions();

    if (!domain.getCodeSource().getLocation().toString().endsWith("/CalculSNR.jar")) {
      p.add(new AllPermission());
    }

    logger.warn("getPermissions = {}", p);
    return p;
  }

  @Override
  public boolean implies(ProtectionDomain domain, Permission permission) {
    logger.warn("implies : {} for {}", domain.getCodeSource(), permission);

    return (!domain.getCodeSource().getLocation().toString().endsWith("/CalculSNR.jar"));

//    return true; 
// return super.implies(domain, permission);
  }

  /**
   * Does nothing.
   */
  @Override
  public void refresh() {
  }
}