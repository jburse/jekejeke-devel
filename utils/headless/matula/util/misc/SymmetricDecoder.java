package matula.util.misc;

import matula.util.system.ForeignXml;

import javax.crypto.Cipher;
import javax.crypto.CipherInputStream;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import java.io.IOException;
import java.io.InputStream;
import java.security.GeneralSecurityException;
import java.text.ParseException;

/**
 * <p>This class provides utilities to handle ascii printable keys.
 * The algorithm is symmetric keys. The method makeKey() will generate a
 * new key. The method encryptText() will encrypt a text by the key. The
 * method decryptText() will decrypt a text by the key.</p>
 * <p>Mode of operation is curtesy of the algorithm.</p>
 *
 * @author Copyright 2007-2011, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.8.9 (a fast and small prolog interpreter)
 */
public final class SymmetricDecoder extends Symmetric {

    /**
     * <p>Decrypt the given text by the public key. Decription is done by decoding the
     * bsae64 cypher, decrypting this into the payload, and then interpreting this
     * payload as UTF-8 text.</p>
     * <p>This way the text can be savely used in an URL request.</p>
     * <p>Mode of operation is curtesy of the algorithm.</p>
     *
     * @param t The encrypted text.
     * @return The original text.
     * @throws IOException              Shit happens.
     * @throws GeneralSecurityException Shit happens.
     * @throws ParseException           Shit happens.
     */
    public String decryptText(String t)
            throws IOException, GeneralSecurityException, ParseException {
        SecretKey key = decodeKey(getKey());
        IvParameterSpec iv = decodeIv(getKey());
        Cipher cipher;
        if (iv != null) {
            cipher = Cipher.getInstance(key.getAlgorithm() + "/CBC/PKCS5Padding");
            cipher.init(Cipher.DECRYPT_MODE, key, iv);
        } else {
            cipher = Cipher.getInstance(key.getAlgorithm());
            cipher.init(Cipher.DECRYPT_MODE, key);
        }

        byte[] plain = ForeignXml.sysBase64Decode(t);
        byte[] res = cipher.doFinal(plain);

        return new String(res, "UTF-8");
    }

    /**
     * <p>Create a decrypting input stream on top of an input stream.</p>
     *
     * @param in The given input stream.
     * @return The now decrypting input stream.
     * @throws GeneralSecurityException Shit happens.
     * @throws ParseException           Shit happens.
     */
    public InputStream decryptStream(InputStream in)
            throws GeneralSecurityException, ParseException {
        SecretKey key = decodeKey(getKey());
        Cipher cipher = Cipher.getInstance(key.getAlgorithm());
        cipher.init(Cipher.DECRYPT_MODE, key);
        return new CipherInputStream(in, cipher);
    }

}
