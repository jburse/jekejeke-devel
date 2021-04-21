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
 * <p>This class provides decoding via symmetric cipher</p>
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
