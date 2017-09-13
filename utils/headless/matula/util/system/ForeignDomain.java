package matula.util.system;

import java.net.IDN;

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

    /**
     * <p>Determine the user of a domain.</p>
     *
     * @param domain The domain.
     * @return The user.
     */
    public static String sysDomainUser(String domain) {
        int k = domain.indexOf('@');
        if (k == -1)
            return "";
        return domain.substring(0, k);
    }

    /**
     * <p>Determine the host of a domain.</p>
     *
     * @param domain The domain.
     * @return The host.
     */
    public static String sysDomainHost(String domain) {
        int k = domain.indexOf('@');
        if (k == -1)
            return IDN.toUnicode(domain);
        return IDN.toUnicode(domain.substring(k + 1));
    }

    /**
     * <p>Create a domain from user and host.</p>
     *
     * @param user The user.
     * @param host The host.
     * @return The domain.
     */
    public static String sysDomainMake(String user, String host) {
        if ("".equals(user)) {
            return IDN.toASCII(host);
        } else {
            return user + "@" + IDN.toASCII(host);
        }
    }

    /**
     * <p>Some test.</p>
     *
     * @param args The arguments, unused.
     */
    /*
    public static void main(String[] args) {
        String user="foo";
        String host="λ.com";
        System.out.println("user="+user+", host="+host);

        String domain=sysDomainMake(user,host);
        System.out.println("domain="+domain);

        user=sysDomainUser(domain);
        host=sysDomainHost(domain);
        System.out.println("user="+user+", host="+host);
    }
    */

}