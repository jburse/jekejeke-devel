package matula.util.misc;

import matula.util.system.ForeignXml;

import javax.crypto.Cipher;
import javax.crypto.CipherOutputStream;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.IvParameterSpec;
import java.io.IOException;
import java.io.OutputStream;
import java.security.GeneralSecurityException;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.text.ParseException;

/**
 * <p>This class provides utilities to handle ascii printable keys.
 * The algorithm is symmetric keys. The method makeKey() will generate a
 * new key. The method encryptText() will encrypt a text by the key. The
 * method decryptText() will decrypt a text by the key.</p>
 * <p>Mode of operation is curtesy of the algorithm.</p>
 *
 * @author Copyright 2007-2019, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.7 (a fast and small prolog interpreter)
 */
public final class SymmetricEncoder extends Symmetric {
    public static final int DES_BITS = 56;
    public static final int AES_BITS = 128;

    /**
     * <p>Encrypt the given text by the private key. Encription is done by building
     * the UTF-8 binary representation, encrypting this payload, and then base64
     * encoding the result.</p>
     * <p>This way the text can be savely used in an URL request.</p>
     * <p>Mode of operation is curtesy of the algorithm.</p>
     *
     * @param t The original text.
     * @return The encrypted text.
     * @throws IOException              Shit happens.
     * @throws GeneralSecurityException Shit happens.
     * @throws ParseException           Shit happens.
     */
    public String encryptText(String t)
            throws IOException, GeneralSecurityException, ParseException {
        SecretKey key = decodeKey(getKey());
        IvParameterSpec iv = decodeIv(getKey());
        Cipher cipher;
        if (iv != null) {
            cipher = Cipher.getInstance(key.getAlgorithm() + "/CBC/PKCS5Padding");
            cipher.init(Cipher.ENCRYPT_MODE, key, iv);
        } else {
            cipher = Cipher.getInstance(key.getAlgorithm());
            cipher.init(Cipher.ENCRYPT_MODE, key);
        }

        byte[] plain = t.getBytes("UTF-8");
        byte[] res = cipher.doFinal(plain);

        return ForeignXml.sysBase64Encode(res);
    }

    /**
     * <p>Create an encrypting output stream on top of an output stream.</p>
     *
     * @param out The given output stream.
     * @return The now encrypting output stream.
     * @throws IOException              Shit happens.
     * @throws GeneralSecurityException Shit happens.
     * @throws ParseException           Shit happens.
     */
    public OutputStream encryptStream(OutputStream out)
            throws IOException, GeneralSecurityException, ParseException {
        SecretKey key = decodeKey(getKey());
        Cipher cipher = Cipher.getInstance(key.getAlgorithm());
        cipher.init(Cipher.ENCRYPT_MODE, key);
        return new CipherOutputStream(out, cipher);
    }

    /**
     * <p>Encode a key. The syntax is as follows:</p>
     * <pre>
     *   algorithm ":" format ":" key_base64
     * </pre>
     *
     * @param key The key.
     * @return The encoded key.
     */
    private static String encodeKey(Key key) {
        return key.getAlgorithm() + ":" +
                key.getFormat() + ":" +
                ForeignXml.sysBase64Encode(key.getEncoded());
    }

    /**
     * <p>Encode a key. The syntax is as follows:</p>
     * <pre>
     *   algorithm ":" format ":" key_base64 ":" iv_base64
     * </pre>
     *
     * @param key The key.
     * @return The encoded key.
     */
    private static String encodeKey(Key key, IvParameterSpec iv) {
        return key.getAlgorithm() + ":" +
                key.getFormat() + ":" +
                ForeignXml.sysBase64Encode(key.getEncoded()) + ":" +
                ForeignXml.sysBase64Encode(iv.getIV());
    }

    /**
     * <p>Make a key.</p>
     * <p>The key can be retrieved by the corresponding accessor.</p>
     *
     * @throws NoSuchAlgorithmException Shit happens.
     */
    public void makeKey() throws NoSuchAlgorithmException {
        KeyGenerator keyGen = KeyGenerator.getInstance(DES_ALGORITHM);
        keyGen.init(DES_BITS);
        setKey(encodeKey(keyGen.generateKey()));
    }

    /**
     * <p>Make a key.</p>
     * <p>The key can be retrieved by the corresponding accessor.</p>
     *
     * @throws NoSuchAlgorithmException Shit happens.
     */
    public void makeKey(String algorithm) throws NoSuchAlgorithmException {
        KeyGenerator keyGen = KeyGenerator.getInstance(algorithm);
        byte[] rand;
        if (algorithm.equals(AES_ALGORITHM)) {
            keyGen.init(AES_BITS);
            rand = new byte[16];
            new SecureRandom().nextBytes(rand);
        } else if (algorithm.equals(DES_ALGORITHM)) {
            keyGen.init(DES_BITS);
            rand = null;
        } else {
            throw new IllegalArgumentException("unsupported algorithm " + algorithm);
        }
        if (rand != null) {
            setKey(encodeKey(keyGen.generateKey(), new IvParameterSpec(rand)));
        } else {
            setKey(encodeKey(keyGen.generateKey()));
        }
    }

    /**
     * <p>Some testingt.</p>
     *
     * @param args Not used.
     */
    public static void main(String[] args) throws GeneralSecurityException, ParseException, IOException {
        SymmetricEncoder se = new SymmetricEncoder();
        SymmetricDecoder sd = new SymmetricDecoder();
        String text = "0123456701234567012345670123456701234567012345670123456701234567";

        se.makeKey();
        String key = se.getKey();
        System.out.println("key=" + key);
        System.out.println("decode(key)=" + decodeKey(key));
        String cipher = se.encryptText(text);
        System.out.println("encrypt(hello)=" + cipher);
        sd.setKey(key);
        System.out.println("decrypt(encrypt(hello))=" + sd.decryptText(cipher));
        System.out.println();

        se.makeKey(AES_ALGORITHM);
        key = se.getKey();
        System.out.println("key=" + key);
        System.out.println("decode(key)=" + decodeKey(key));
        cipher = se.encryptText(text);
        System.out.println("decrypt(encrypt(hello))=" + cipher);
        sd.setKey(key);
        System.out.println("decrypt(encrypt(hello))=" + sd.decryptText(cipher));
        System.out.println();
    }

}
