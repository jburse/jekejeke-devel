package matula.util.system;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.InetAddress;
import java.net.MalformedURLException;
import java.net.UnknownHostException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ForeignDomain {
    private static final char CHAR_AT = '@';
    private static final char CHAR_COLON = ':';

    private static Method toascii;
    private static Method tounicode;

    static {
        try {
            Class<?> clazz = Class.forName("java.net.IDN");
            toascii = clazz.getDeclaredMethod("toASCII", String.class);
            tounicode = clazz.getDeclaredMethod("toUnicode", String.class);
        } catch (ClassNotFoundException e) {
            toascii = null;
            tounicode = null;
        } catch (NoSuchMethodException e) {
            toascii = null;
            tounicode = null;
        }
    }

    /************************************************************/
    /* Authority Make                                           */
    /************************************************************/

    /**
     * <p>Determine the user of an authority.</p>
     *
     * @param au The authority.
     * @return The user.
     */
    public static String sysAuthorityUser(String au) {
        int k = au.lastIndexOf(CHAR_AT);
        if (k == -1)
            return "";
        return au.substring(0, k);
    }

    /**
     * <p>Determine the host of an authority.</p>
     *
     * @param au The authority.
     * @return The host.
     */
    public static String sysAuthorityHost(String au) {
        int k = au.lastIndexOf(CHAR_AT);
        String hp;
        if (k == -1) {
            hp = au;
        } else {
            hp = au.substring(k + 1);
        }
        k = hp.indexOf(CHAR_COLON);
        if (k == -1) {
            return hp;
        } else {
            return hp.substring(0, k);
        }
    }

    /**
     * <p>Determine the port of an authority.</p>
     *
     * @param au The authority.
     * @return The port.
     */
    public static int sysAuthorityPort(String au) {
        int k = au.lastIndexOf(CHAR_AT);
        String hp;
        if (k == -1) {
            hp = au;
        } else {
            hp = au.substring(k + 1);
        }
        k = hp.indexOf(CHAR_COLON);
        if (k == -1) {
            return -1;
        } else {
            return Integer.parseInt(hp.substring(k + 1));
        }
    }

    /**
     * <p>Create a domain from user, host and port.</p>
     *
     * @param user The user.
     * @param host The host.
     * @param port The port.
     * @return The authority.
     * @throws MalformedURLException Authority assembling problem.
     */
    public static String sysAuthorityMake(String user, String host, int port)
            throws MalformedURLException {
        if (!ForeignDomain.isHost(host))
            throw new MalformedURLException("illegal host");
        String hp;
        if (port != -1) {
            hp = host + ":" + port;
        } else {
            hp = host;
        }
        if ("".equals(user)) {
            return hp;
        } else {
            return user + "@" + hp;
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
     */
    public static String sysForwardLookup(String host) {
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
     */
    public static String sysReverseLookup(String host) {
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
     * <p>Determine the punny encode of an uri.</p>
     *
     * @param adr The uri.
     * @return The encoded uri.
     */
    public static String sysUriPuny(String adr) {
        String spec = ForeignUri.sysUriSpec(adr);
        String newspec = ForeignDomain.sysSpecPuny(spec);
        if (!spec.equals(newspec)) {
            String query = ForeignUri.sysUriQuery(adr);
            String hash = ForeignUri.sysUriHash(adr);
            adr = ForeignUri.sysUriMake(newspec, query, hash);
        }
        return adr;
    }

    /**
     * <p>Determine the punny decode of an uri.</p>
     *
     * @param adr The uri.
     * @return The decoded uri.
     * @throws MalformedURLException Domain assembling problem.
     */
    public static String sysUriUnpuny(String adr)
            throws MalformedURLException {
        String spec = ForeignUri.sysUriSpec(adr);
        String newspec = ForeignDomain.sysSpecUnpuny(spec);
        if (!spec.equals(newspec)) {
            String query = ForeignUri.sysUriQuery(adr);
            String hash = ForeignUri.sysUriHash(adr);
            adr = ForeignUri.sysUriMake(newspec, query, hash);
        }
        return adr;

    }

    /**
     * <p>Determine the punny encode of a spec.</p>
     *
     * @param spec The spec.
     * @return The encoded spec.
     */
    public static String sysSpecPuny(String spec) {
        try {
            String scheme = ForeignUri.sysSpecScheme(spec);
            if (ForeignUri.SCHEME_JAR.equals(scheme)) {
                String authority = ForeignUri.sysSpecAuthority(spec);
                String path = ForeignUri.sysSpecPath(spec);
                int k = path.lastIndexOf(ForeignUri.JAR_SEP);
                if (k != -1) {
                    String subspec = ForeignUri.sysSpecMake(ForeignFile.STRING_EMPTY,
                            authority, path.substring(0, k));
                    String newsubspec = ForeignDomain.sysUriPuny(subspec);
                    if (!subspec.equals(newsubspec)) {
                        spec = ForeignUri.sysSpecMake(ForeignUri.SCHEME_JAR,
                                ForeignFile.STRING_EMPTY, newsubspec + path.substring(k));
                    }
                } else {
                    String subspec = ForeignUri.sysSpecMake(ForeignFile.STRING_EMPTY,
                            authority, path);
                    String newsubspec = ForeignDomain.sysUriPuny(subspec);
                    if (!subspec.equals(newsubspec)) {
                        spec = ForeignUri.sysSpecMake(ForeignUri.SCHEME_JAR,
                                ForeignFile.STRING_EMPTY, newsubspec);
                    }
                }
            } else if (ForeignUri.SCHEME_MAILTO.equals(scheme)) {
                String path = ForeignUri.sysSpecPath(spec);
                String newpath = sysDomainPuny(path);
                if (!path.equals(newpath)) {
                    spec = ForeignUri.sysSpecMake(scheme, ForeignFile.STRING_EMPTY, newpath);
                }
            } else {
                String authority = ForeignUri.sysSpecAuthority(spec);
                String newauthority = sysDomainPuny(authority);
                if (!authority.equals(newauthority)) {
                    String path = ForeignUri.sysSpecPath(spec);
                    spec = ForeignUri.sysSpecMake(scheme, newauthority, path);
                }
            }
        } catch (MalformedURLException x) {
            throw new RuntimeException(ForeignUri.SHOULDNT_HAPPEN, x);
        }
        return spec;
    }

    /**
     * <p>Determine the punny encode of a spec.</p>
     *
     * @param spec The spec.
     * @return The encoded spec.
     * @throws MalformedURLException Domain assembling problem.
     */
    private static String sysSpecUnpuny(String spec)
            throws MalformedURLException {
        String scheme = ForeignUri.sysSpecScheme(spec);
        if (ForeignUri.SCHEME_JAR.equals(scheme)) {
            String authority = ForeignUri.sysSpecAuthority(spec);
            String path = ForeignUri.sysSpecPath(spec);
            int k = path.lastIndexOf(ForeignUri.JAR_SEP);
            if (k != -1) {
                String subspec = ForeignUri.sysSpecMake(ForeignFile.STRING_EMPTY,
                        authority, path.substring(0, k));
                String newsubspec = ForeignDomain.sysUriUnpuny(subspec);
                if (!subspec.equals(newsubspec)) {
                    spec = ForeignUri.sysSpecMake(ForeignUri.SCHEME_JAR,
                            ForeignFile.STRING_EMPTY, newsubspec + path.substring(k));
                }
            } else {
                String subspec = ForeignUri.sysSpecMake(ForeignFile.STRING_EMPTY,
                        authority, path);
                String newsubspec = ForeignDomain.sysUriUnpuny(subspec);
                if (!subspec.equals(newsubspec)) {
                    spec = ForeignUri.sysSpecMake(ForeignUri.SCHEME_JAR,
                            ForeignFile.STRING_EMPTY, newsubspec);
                }
            }
        } else if (ForeignUri.SCHEME_MAILTO.equals(scheme)) {
            String path = ForeignUri.sysSpecPath(spec);
            String newpath = sysDomainUnpuny(path);
            if (!path.equals(newpath)) {
                spec = ForeignUri.sysSpecMake(scheme, ForeignFile.STRING_EMPTY, newpath);
            }
        } else {
            String authority = ForeignUri.sysSpecAuthority(spec);
            String newauthority = sysDomainUnpuny(authority);
            if (!authority.equals(newauthority)) {
                String path = ForeignUri.sysSpecPath(spec);
                spec = ForeignUri.sysSpecMake(scheme, newauthority, path);
            }
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
        if (toascii == null)
            return dom;
        try {
            String host = sysAuthorityHost(dom);
            String newhost = (String) toascii.invoke(null, host);
            if (!host.equals(newhost)) {
                String user = sysAuthorityUser(dom);
                int port = sysAuthorityPort(dom);
                dom = sysAuthorityMake(user, newhost, port);
            }
        } catch (MalformedURLException x) {
            throw new RuntimeException(ForeignUri.SHOULDNT_HAPPEN, x);
        } catch (IllegalAccessException x) {
            throw new RuntimeException(ForeignUri.SHOULDNT_HAPPEN, x);
        } catch (InvocationTargetException x) {
            throw new RuntimeException(ForeignUri.SHOULDNT_HAPPEN, x);
        }
        return dom;
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
        if (tounicode == null)
            return dom;
        try {
            String host = sysAuthorityHost(dom);
            String newhost = (String) tounicode.invoke(null, host);
            if (!host.equals(newhost)) {
                String user = sysAuthorityUser(dom);
                int port = sysAuthorityPort(dom);
                dom = sysAuthorityMake(user, newhost, port);
            }
        } catch (IllegalAccessException x) {
            throw new RuntimeException(ForeignUri.SHOULDNT_HAPPEN, x);
        } catch (InvocationTargetException x) {
            throw new RuntimeException(ForeignUri.SHOULDNT_HAPPEN, x);
        }
        return dom;
    }

    /************************************************************/
    /* SHA-1 Hash                                               */
    /************************************************************/

    /**
     * <p>Compute the SHA-1 Hash of a payload.</p>
     *
     * @param data The payload.
     * @return The SHA-1 Hash.
     */
    public static byte[] sysSHA1Hash(byte[] data) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-1");
            return md.digest(data);
        } catch (NoSuchAlgorithmException e) {
            return null;
        }
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
        String adr = "mailto:foo@zürich.ch";
        System.out.println("adr=" + adr);
        adr = sysSpecPuny(adr);
        System.out.println("puny(adr)=" + adr);

        System.out.println();

        adr = "mailto:foo@xn--zrich-kva.ch";
        System.out.println("adr=" + adr);
        adr = sysSpecUnpuny(adr);
        System.out.println("unpuny(adr)=" + adr);

        System.out.println();

        adr = "jar:http://zürich.ch/archive.jar!/entry.txt";
        System.out.println("adr=" + adr);
        adr = sysSpecPuny(adr);
        System.out.println("puny(adr)=" + adr);

        System.out.println();

        adr = "jar:http://xn--zrich-kva.ch/archive.jar!/entry.txt";
        System.out.println("adr=" + adr);
        adr = sysSpecUnpuny(adr);
        System.out.println("unpuny(adr)=" + adr);
    }
    */

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