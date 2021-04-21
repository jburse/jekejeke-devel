package matula.util.misc;

import matula.util.system.ForeignXml;

import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.DESKeySpec;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.security.GeneralSecurityException;
import java.security.spec.KeySpec;
import java.text.ParseException;

/**
 * <p>This class provides symmetric cipher</p>
 * <p/>
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
class Symmetric {
    public static final String DES_ALGORITHM = "DES";
    public static final String AES_ALGORITHM = "AES";

    public static final String FORMAT_RAW = "RAW";

    private String secretkey;

    /**
     * Set the key.
     *
     * @param s The key.
     */
    public final void setKey(String s) {
        secretkey = s;
    }

    /**
     * <p>Retrieve the key.</p>
     *
     * @return The key.
     */
    public final String getKey() {
        return secretkey;
    }

    /**
     * <p>Decode a symmetric key.</p>
     * <p>Syntax is defined in encodeKey().</p>
     *
     * @param key The encoded key.
     * @return The symmetric key.
     * @throws GeneralSecurityException Shit happens.
     * @throws ParseException           Shit happens.
     */
    protected static SecretKey decodeKey(String key)
            throws GeneralSecurityException, ParseException {
        int k = key.indexOf(':');
        String algorithm = key.substring(0, k);
        int k2 = key.indexOf(':', k + 1);
        int k3 = key.indexOf(':', k2 + 1);
        String format = key.substring(k + 1, k2);
        String data = key.substring(k2 + 1, (k3 != -1 ? k3 : key.length()));
        if (format.equals(FORMAT_RAW)) {
            if (algorithm.equals(AES_ALGORITHM)) {
                return new SecretKeySpec(ForeignXml.sysBase64Decode(data), AES_ALGORITHM);
            } else if (algorithm.equals(DES_ALGORITHM)) {
                KeySpec spec = new DESKeySpec(ForeignXml.sysBase64Decode(data));
                SecretKeyFactory fact = SecretKeyFactory.getInstance(algorithm);
                return fact.generateSecret(spec);
            } else {
                throw new IllegalArgumentException("unsupported algorithm " + algorithm);
            }
        } else {
            throw new IllegalArgumentException("unknown format " + format);
        }
    }

    /**
     * <p>Decode an iv parameter.</p>
     * <p>Syntax is defined in encodeKey().</p>
     * *
     *
     * @param key The encoded key.
     * @return The iv parameter.
     * @throws ParseException Shit happens.
     */
    protected static IvParameterSpec decodeIv(String key)
            throws ParseException {
        int k = key.indexOf(':');
        int k2 = key.indexOf(':', k + 1);
        int k3 = key.indexOf(':', k2 + 1);
        if (k3 != -1) {
            String parameter = key.substring(k3 + 1);
            return new IvParameterSpec(ForeignXml.sysBase64Decode(parameter));
        } else {
            return null;
        }
    }

}