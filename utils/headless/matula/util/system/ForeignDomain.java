package matula.util.system;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.net.IDN;
import java.net.MalformedURLException;
import java.net.SocketTimeoutException;

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
    /* Canonical Domain                                         */
    /************************************************************/

    /**
     * <p>Determine a canonical domain.</p>
     *
     * @param dom The domain.
     * @return The canonical domain.
     * @throws MalformedURLException Domain assembling problem.
     */
    public static String sysCanonicalDomain(String dom)
            throws IOException {
        String user = sysDomainUser(dom);
        String host = sysDomainHost(dom);
        String spec = ForeignUri.sysSpecMake(ForeignUri.SCHEME_HTTP, host, "/robots.txt");
        String adr = ForeignUri.sysUriMake(spec, "", "");

        for (;;) {
            String res;
            try {
                res = ForeignCache.DEFAULT_HEAD.getRedirect(adr);
            } catch (IOException x) {
                if (x instanceof InterruptedIOException &&
                        !(x instanceof SocketTimeoutException)) {
                    throw x;
                } else {
                    res = null;
                }
            }
            if (res==null)
                break;
            adr=res;
        }

        spec = ForeignUri.sysUriSpec(adr);
        host = ForeignUri.sysSpecAuthority(spec);
        return sysDomainMake(user, host);
    }

    /************************************************************/
    /* Domain Encoding/Decoding                                 */
    /************************************************************/

    /**
     * <p>Encode an domain.</p>
     * <p>The host will be ASCII encoded.</p>
     * <p>The encoding will be punny code.</p>
     *
     * @param dom The domain.
     * @return The encoded domain.
     */
    public static String sysDomainEncode(String dom) {
        try {
            String user = sysDomainUser(dom);
            String host = sysDomainHost(dom);
            host = IDN.toASCII(host);
            return sysDomainMake(user, host);
        } catch (MalformedURLException x) {
            throw new RuntimeException(ForeignUri.SHOULDNT_HAPPEN, x);
        }
    }

    /**
     * <p>Decode a domain.</p>
     * <p>The host will be decoded.</p>
     * <p>The decodiong will be punny code.</p>
     *
     * @param dom The domain.
     * @return The decoded domain.
     * @throws MalformedURLException Domain assembling problem.
     */
    public static String sysDomainDecode(String dom)
            throws MalformedURLException {
        String user = sysDomainUser(dom);
        String host = sysDomainHost(dom);
        host = IDN.toUnicode(host);
        return sysDomainMake(user, host);
    }

    /**
     * <p>Some test.</p>
     *
     * @param args The arguments, unused.
     * @throws IOException Domain assembling problem.
     */
    public static void main(String[] args)
            throws IOException {
        String dom = "foo@λ.com";
        System.out.println("dom=" + dom);
        dom = sysDomainEncode(dom);
        System.out.println("encode(dom)=" + dom);
        dom = sysDomainDecode(dom);
        System.out.println("decode(encode(dom))=" + dom);

        System.out.println();

        dom = "swi-prolog.org";
        System.out.println("dom=" + dom);
        dom = sysCanonicalDomain(dom);
        System.out.println("canonical(dom)=" + dom);

        System.out.println();

        dom = "64.71.35.59";
        System.out.println("dom=" + dom);
        dom = sysCanonicalDomain(dom);
        System.out.println("canonical(dom)=" + dom);
    }

}