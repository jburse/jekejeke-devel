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
 * <p>Base class for decoder and encoder.</p>
 * @author Copyright 2007-2011, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.8.9 (a fast and small prolog interpreter)
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