package matula.util.system;

import java.io.IOException;
import java.net.IDN;
import java.net.InetAddress;
import java.net.MalformedURLException;
import java.net.UnknownHostException;

/**
 * <p>The foreign predicates for the module system/domain.
 * </p>
 * Warranty & Liability
 * To the extent permitted by applicable law and unless explicitly
 * otherwise agreed upon, XLOG Technologies GmbH makes no warranties
 * regarding the provided information. XLOG Technologies GmbH assumes
 * no liability that any problems might be solved with the information
 * provided by XLOG Technologies GmbH.
 * <p/>
 * Rights & License
 * All industrial property rights regarding the information - copyright
 * and patent rights in particular - are the sole property of XLOG
 * Technologies GmbH. If the company was not the originator of some
 * excerpts, XLOG Technologies GmbH has at least obtained the right to
 * reproduce, change and translate the information.
 * <p/>
 * Reproduction is restricted to the whole unaltered document. Reproduction
 * of the information is only allowed for non-commercial uses. Selling,
 * giving away or letting of the execution of the library is prohibited.
 * The library can be distributed as part of your applications and libraries
 * for execution provided this comment remains unchanged.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ForeignDomain {
    private static final char CHAR_AT = '@';

    /**
     * <p>Determine the user of a domain.</p>
     *
     * @param dom The domain.
     * @return The user.
     */
    public static String sysDomainUser(String dom) {
        int k = dom.lastIndexOf(CHAR_AT);
        if (k == -1)
            return "";
        return dom.substring(0, k);
    }

    /**
     * <p>Determine the host of a domain.</p>
     *
     * @param dom The domain.
     * @return The host.
     */
    public static String sysDomainHost(String dom) {
        int k = dom.lastIndexOf(CHAR_AT);
        if (k == -1)
            return dom;
        return dom.substring(k + 1);
    }

    /**
     * <p>Create a domain from user and host.</p>
     *
     * @param user The user.
     * @param host The host.
     * @return The domain.
     * @throws MalformedURLException Domain assembling problem.
     */
    public static String sysDomainMake(String user, String host)
            throws MalformedURLException {
        if ("".equals(user)) {
            return host;
        } else {
            if (!ForeignDomain.isHost(host))
                throw new MalformedURLException("illegal host");
            return user + "@" + host;
        }
    }

    /**
     * <p>Check whether the host is well formed.</p>
     *
     * @param host The host.
     * @return True if the host is well formed, otherwise false.
     */
    private static boolean isHost(String host) {
        return (host.indexOf(CHAR_AT) == -1);
    }

    /************************************************************/
    /* Domain Lookup                                            */
    /************************************************************/

    /**
     * <p>Determine a forward lookup.</p>
     *
     * @param host The host.
     * @return The forward host, or null.
     * @throws MalformedURLException Domain assembling problem.
     */
    public static String sysForwardLookup(String host)
            throws MalformedURLException {
        InetAddress ia;
        try {
            ia = InetAddress.getByName(host);
        } catch (UnknownHostException x) {
            ia = null;
        }
        if (ia == null)
            return null;
        return ia.getHostAddress();
    }

    /**
     * <p>Determine a reverse lookup.</p>
     *
     * @param host The host.
     * @return The reverse host, or null.
     * @throws MalformedURLException Domain assembling problem.
     */
    public static String sysReverseLookup(String host)
            throws MalformedURLException {
        InetAddress ia;
        try {
            ia = InetAddress.getByName(host);
        } catch (UnknownHostException x) {
            ia = null;
        }
        if (ia == null)
            return null;
        return ia.getCanonicalHostName();
    }

    /************************************************************/
    /* Ping Host                                                */
    /************************************************************/

    /**
     * <p>Ping a host.</p>
     *
     * @param host The host.
     * @return True if host is reachable, otherwise false.
     */
    public static boolean sysPingHost(String host)
            throws IOException {
        InetAddress ia;
        try {
            ia = InetAddress.getByName(host);
        } catch (UnknownHostException x) {
            ia = null;
        }
        if (ia == null)
            return false;
        return ia.isReachable(1000);
    }

    /************************************************************/
    /* Puny Code                                                */
    /************************************************************/

    /**
     * <p>Determine the punny encode of a spec.</p>
     *
     * @param spec The spec.
     * @return The encoded spec.
     */
    public static String sysSpecPuny(String spec) {
        try {
            String scheme = ForeignUri.sysSpecScheme(spec);
            String authority = ForeignUri.sysSpecAuthority(spec);
            String path = ForeignUri.sysSpecPath(spec);
            if (ForeignUri.SCHEME_JAR.equals(scheme)) {
                int k = path.lastIndexOf("!/");
                if (k != -1) {
                    spec = ForeignUri.sysSpecMake("", authority, path.substring(0, k));
                    spec = ForeignDomain.sysSpecPuny(spec);
                    spec = ForeignUri.sysSpecMake(ForeignUri.SCHEME_JAR, "", spec + path.substring(k));
                } else {
                    spec = ForeignUri.sysSpecMake("", authority, path);
                    spec = ForeignDomain.sysSpecPuny(spec);
                    spec = ForeignUri.sysSpecMake(ForeignUri.SCHEME_JAR, "", spec);
                }
            } else {
                spec = ForeignUri.sysSpecMake(scheme, sysDomainPuny(authority), path);
            }
            return spec;
        } catch (MalformedURLException x) {
            throw new RuntimeException(ForeignUri.SHOULDNT_HAPPEN, x);
        }
    }

    /**
     * <p>Determine the punny encode of a spec.</p>
     *
     * @param spec The spec.
     * @return The encoded spec.
     * @throws MalformedURLException Domain assembling problem.
     */
    public static String sysSpecUnpuny(String spec)
            throws MalformedURLException {
        String scheme = ForeignUri.sysSpecScheme(spec);
        String authority = ForeignUri.sysSpecAuthority(spec);
        String path = ForeignUri.sysSpecPath(spec);
        if (ForeignUri.SCHEME_JAR.equals(scheme)) {
            int k = path.lastIndexOf("!/");
            if (k != -1) {
                spec = ForeignUri.sysSpecMake("", authority, path.substring(0, k));
                spec = ForeignDomain.sysSpecUnpuny(spec);
                spec = ForeignUri.sysSpecMake(ForeignUri.SCHEME_JAR, "", spec + path.substring(k));
            } else {
                spec = ForeignUri.sysSpecMake("", authority, path);
                spec = ForeignDomain.sysSpecUnpuny(spec);
                spec = ForeignUri.sysSpecMake(ForeignUri.SCHEME_JAR, "", spec);
            }
        } else {
            spec = ForeignUri.sysSpecMake(scheme, sysDomainUnpuny(authority), path);
        }
        return spec;
    }

    /**
     * <p>Determine the punny encode of a domain.</p>
     *
     * @param dom The domain.
     * @return The encoded domain.
     */
    private static String sysDomainPuny(String dom) {
        try {
            String host = sysDomainHost(dom);
            if ("".equals(host))
                return dom;
            host = IDN.toASCII(host);
            String user = sysDomainUser(dom);
            return sysDomainMake(user, host);
        } catch (MalformedURLException x) {
            throw new RuntimeException(ForeignUri.SHOULDNT_HAPPEN, x);
        }
    }

    /**
     * <p>Determine the punny decode of a domain.</p>
     *
     * @param dom The domain.
     * @return The decoded domain.
     * @throws MalformedURLException Domain assembling problem.
     */
    private static String sysDomainUnpuny(String dom)
            throws MalformedURLException {
        String host = sysDomainHost(dom);
        if ("".equals(host))
            return dom;
        host = IDN.toUnicode(host);
        String user = sysDomainUser(dom);
        return sysDomainMake(user, host);
    }

    /**
     * <p>Some tests.</p>
     *
     * @param args The arguments, unused.
     * @throws IOException Domain assembling problem.
     */
    /*
    public static void main(String[] args)
            throws IOException {
        String dom = "www.jekejeke.ch";
        System.out.println("dom=" + dom);
        dom = sysForwardLookup(dom);
        System.out.println("forward(dom)=" + dom);

        System.out.println();

        dom = "92.42.190.4";
        System.out.println("dom=" + dom);
        dom = sysReverseLookup(dom);
        System.out.println("reverse(dom)=" + dom);

        System.out.println();

        String adr = "http://zürich.ch/robots.txt";
        System.out.println("adr=" + adr);
        adr = sysSpecPuny(adr);
        System.out.println("puny(adr)=" + adr);

        System.out.println();

        adr = "http://xn--zrich-kva.ch/robots.txt";
        System.out.println("adr=" + adr);
        adr = sysSpecUnpuny(adr);
        System.out.println("unpuny(adr)=" + adr);
    }
    */

}