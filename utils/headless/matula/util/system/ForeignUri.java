package matula.util.system;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.nio.charset.CharacterCodingException;

/**
 * <p>The foreign predicates for the module system/uri. We also encode
 * *\" in URIs to avoid this error by Tomcat: Invalid character found
 * in the request target. The valid characters are defined in RFC
 * 7230 and RFC 3986
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
public final class ForeignUri {
    private static final char CHAR_HASH = '#';
    private static final char CHAR_ASK = '?';
    private static final char CHAR_AMP = '&';
    private static final char CHAR_EQ = '=';

    public final static String NEEDS_COMP = "#%=&\\";
    public final static String NEEDS_SPEC = "?#%\\";
    public final static String NEEDS_HASH = "%\\";

    public final static String SCHEME_FILE = "file";
    public final static String SCHEME_HTTP = "http";
    public final static String SCHEME_JAR = "jar";
    public final static String SCHEME_MAILTO = "mailto";

    public final static String JAR_SEP = "!/";

    public final static String ENCODING_UTF8 = "UTF-8";
    public final static String SHOULDNT_HAPPEN = "shouldnt happen";

    public final static int SCHEME_DRIVE = 2;

    /*******************************************************************/
    /* Query Constructor/Destructor                                    */
    /*******************************************************************/

    /**
     * <p>Determine the first parameter name.</p>
     * <p>The first parameter name will be decoded.</p>
     *
     * @param query The query.
     * @return The first parameter name.
     */
    public static String sysQueryName(String query) {
        if (ForeignFile.STRING_EMPTY.equals(query))
            return null;
        int k = query.indexOf(CHAR_AMP);
        String pair;
        if (k != -1) {
            pair = query.substring(0, k);
        } else {
            pair = query;
        }
        k = pair.indexOf(CHAR_EQ);
        if (k != -1)
            pair = pair.substring(0, k);
        return decode(pair, ENCODING_UTF8);
    }

    /**
     * <p>Determine the first parameter value.</p>
     * <p>The first parameter value will be decoded.</p>
     *
     * @param query The query.
     * @return The first parameter value.
     */
    public static String sysQueryValue(String query) {
        if (ForeignFile.STRING_EMPTY.equals(query))
            return null;
        int k = query.indexOf(CHAR_AMP);
        String pair;
        if (k != -1) {
            pair = query.substring(0, k);
        } else {
            pair = query;
        }
        k = pair.indexOf(CHAR_EQ);
        String value;
        if (k != -1) {
            value = pair.substring(k + 1);
        } else {
            value = ForeignFile.STRING_EMPTY;
        }
        return decode(value, ENCODING_UTF8);
    }

    /**
     * <p>Determine the query rest.</p>
     *
     * @param query The query.
     * @return The query rest.
     */
    public static String sysQueryRest(String query) {
        if (ForeignFile.STRING_EMPTY.equals(query))
            return null;
        int k = query.indexOf(CHAR_AMP);
        if (k != -1) {
            query = query.substring(k + 1);
        } else {
            query = ForeignFile.STRING_EMPTY;
        }
        return query;
    }

    /**
     * <p>Create a query from name, value and rest.</p>
     * <p>The name and value will be minimal encoded.</p>
     *
     * @param name  The first parameter name.
     * @param value The first parameter value.
     * @param rest  The query rest.
     * @return The query.
     */
    public static String sysQueryMake(String name, String value,
                                      String rest) {
        name = encode(name, false, NEEDS_COMP, ENCODING_UTF8);
        if (!ForeignFile.STRING_EMPTY.equals(value))
            name += CHAR_EQ + encode(value, false, NEEDS_COMP, ENCODING_UTF8);
        if (!ForeignFile.STRING_EMPTY.equals(rest))
            name += CHAR_AMP + rest;
        return name;
    }

    /*******************************************************************/
    /* Spec Constructor/Destructor                                     */
    /*******************************************************************/

    /**
     * <p>Retrieve the scheme of a spec.</p>
     * <p>Special casing for drive letters.</p>
     *
     * @param spec The spec.
     * @return The scheme.
     */
    public static String sysSpecScheme(String spec) {
        int k = getSchemeLength(spec);
        if (k == SCHEME_DRIVE)
            return ForeignFile.STRING_EMPTY;
        return (k != 0 ? spec.substring(0, k - 1) : ForeignFile.STRING_EMPTY);
    }

    /**
     * <p>Retrieve the authority of a spec.</p>
     * <p>Special casing for drive letters.</p>
     *
     * @param spec The spec.
     * @return The authority.
     */
    public static String sysSpecAuthority(String spec) {
        int k = getSchemeLength(spec);
        if (k == SCHEME_DRIVE)
            return ForeignFile.STRING_EMPTY;
        if (!spec.startsWith("//", k))
            return ForeignFile.STRING_EMPTY;
        int j = spec.indexOf("/", k + 2);
        if (j == -1)
            return spec.substring(k + 2);
        return spec.substring(k + 2, j);
    }

    /**
     * <p>Retrieve the path of a spec.</p>
     * <p>Special casing for drive letters.</p>
     *
     * @param spec The spec.
     * @return The path.
     */
    public static String sysSpecPath(String spec) {
        int k = getSchemeLength(spec);
        if (k == SCHEME_DRIVE)
            return spec;
        if (!spec.startsWith("//", k))
            return spec.substring(k);
        int j = spec.indexOf("/", k + 2);
        if (j == -1)
            return ForeignFile.STRING_EMPTY;
        return spec.substring(j);
    }

    /**
     * <p>Retrieve the length of the scheme part of a spec.</p>
     * <p>The used syntax is as follows:</p>
     * <pre>
     *       letter     :== "a" - "z" | "A" - "Z".
     *       digit      :== "0" - "9".
     *       special    :== "-" | "+" | ".".
     *       schema     :== letter { letter | digit | special } ":"
     * <pre>
     *
     * @param spec The spec.
     * @return The scheme.
     */
    static int getSchemeLength(String spec) {
        int k = 0;
        if (!(k < spec.length()) || !ForeignUri.isLetter(spec.charAt(k)))
            return 0;
        k++;
        while (k < spec.length() && (ForeignUri.isLetter(spec.charAt(k))
                || ForeignUri.isDigitOrSpecial(spec.charAt(k)))) {
            k++;
        }
        if (!(k < spec.length()) || spec.charAt(k) != ':')
            return 0;
        k++;
        return k;
    }

    /**
     * <p>Check whether the given character is an ASCII letter.</p>
     *
     * @param ch The character.
     * @return True if it is an ASCII letter, otherwise false.
     */
    private static boolean isLetter(char ch) {
        return (('a' <= ch && ch <= 'z') ||
                ('A' <= ch && ch <= 'Z'));
    }

    /**
     * <p>Check whether the given character is a digit or a schema special.</p>
     *
     * @param ch The character.
     * @return True if it is a digit or special, otherwise false.
     */
    private static boolean isDigitOrSpecial(char ch) {
        return (('0' <= ch && ch <= '9') ||
                (ch == '-') || (ch == '+') || (ch == '.'));
    }

    /**
     * <p>Create a spec from scheme, authority and path.</p>
     *
     * @param scheme    The scheme.
     * @param authority The authority.
     * @param path      The path.
     * @return The spec.
     * @throws MalformedURLException URL assembling problem.
     */
    public static String sysSpecMake(String scheme,
                                     String authority,
                                     String path)
            throws MalformedURLException {
        if (!ForeignFile.STRING_EMPTY.equals(authority)) {
            if (!ForeignUri.isAuthority(authority))
                throw new MalformedURLException("illegal authority");
            if (!ForeignFile.STRING_EMPTY.equals(path) &&
                    ForeignFile.sysPathIsRelative(path))
                throw new MalformedURLException("path relative");
            path = "//" + authority + path;
        }
        if (!ForeignFile.STRING_EMPTY.equals(scheme)) {
            if (!ForeignUri.isScheme(scheme))
                throw new MalformedURLException("illegal scheme");
            path = scheme + ":" + path;
        }
        return path;
    }

    /**
     * <p>Check whether the authority is well formed.</p>
     *
     * @param authority The authority.
     * @return True if the authority is well formed, otherwise false.
     */
    private static boolean isAuthority(String authority) {
        return (authority.indexOf(ForeignFile.CHAR_SLASH) == -1);
    }

    /**
     * <p>Check whether the scheme is well formed.</p>
     *
     * @param scheme The scheme.
     * @return True if the scheme is well formed, otherwise false.
     */
    private static boolean isScheme(String scheme) {
        int k = 0;
        if (!(k < scheme.length()) || !ForeignUri.isLetter(scheme.charAt(k)))
            return false;
        k++;
        while (k < scheme.length() && (ForeignUri.isLetter(scheme.charAt(k))
                || ForeignUri.isDigitOrSpecial(scheme.charAt(k)))) {
            k++;
        }
        if (!(k < scheme.length()))
            return true;
        return false;
    }

    /*******************************************************************/
    /* URI Constructor/Destructor                                      */
    /*******************************************************************/

    /**
     * <p>Determine the hash.</p>
     * <p>The hash will be decoded.</p>
     *
     * @param adr The uri.
     * @return The decoded hash.
     */
    public static String sysUriHash(String adr) {
        int k = adr.indexOf(CHAR_HASH);
        String hash;
        if (k != -1) {
            hash = adr.substring(k + 1);
        } else {
            hash = ForeignFile.STRING_EMPTY;
        }
        return decode(hash, ENCODING_UTF8);
    }

    /**
     * <p>Determine the query.</p>
     *
     * @param adr The uri.
     * @return The query.
     */
    public static String sysUriQuery(String adr) {
        int k = adr.indexOf(CHAR_HASH);
        if (k != -1)
            adr = adr.substring(0, k);
        k = adr.indexOf(CHAR_ASK);
        String query;
        if (k != -1) {
            query = adr.substring(k + 1);
        } else {
            query = ForeignFile.STRING_EMPTY;
        }
        return query;
    }

    /**
     * <p>Determine the spec of an uri.</p>
     * <p>The spec will be decoded.</p>
     *
     * @param adr The uri.
     * @return The decoded spec.
     */
    public static String sysUriSpec(String adr) {
        int k = adr.indexOf(CHAR_HASH);
        if (k != -1)
            adr = adr.substring(0, k);
        k = adr.indexOf(CHAR_ASK);
        if (k != -1)
            adr = adr.substring(0, k);
        return decode(adr, ENCODING_UTF8);
    }

    /**
     * <p>Create an URI from spec, query and hash.</p>
     * <p>The spec and the hash will be minimal encoded.</p>
     *
     * @param spec  The spec.
     * @param query The query.
     * @param hash  The hash.
     * @return The uri.
     */
    public static String sysUriMake(String spec, String query,
                                    String hash) {
        spec = encode(spec, false, NEEDS_SPEC, ENCODING_UTF8);
        if (!ForeignFile.STRING_EMPTY.equals(query))
            spec += CHAR_ASK + query;
        if (!ForeignFile.STRING_EMPTY.equals(hash))
            spec += CHAR_HASH + encode(hash, false, NEEDS_HASH, ENCODING_UTF8);
        return spec;
    }

    /*******************************************************************/
    /* URI Following                                                   */
    /*******************************************************************/

    /**
     * <p>Check whether an uri is relative.</p>
     *
     * @param a The uri.
     * @return True if it is relative.
     */
    public static boolean sysUriIsRelative(String a) {
        String spec = ForeignUri.sysUriSpec(a);
        String scheme = ForeignUri.sysSpecScheme(spec);
        String authority = ForeignUri.sysSpecAuthority(spec);

        if (ForeignFile.STRING_EMPTY.equals(scheme) &&
                ForeignFile.STRING_EMPTY.equals(authority)) {
            String path = ForeignUri.sysSpecPath(spec);
            return ForeignFile.sysPathIsRelative(path);
        } else {
            return false;
        }
    }

    /**
     * <p>Determine the absolute URI.</p>
     *
     * @param a The base URI.
     * @param b The relative or absolute URI.
     * @return The absolute URI.
     */
    public static String sysUriAbsolute(String a, String b) {
        try {
            String spec2 = ForeignUri.sysUriSpec(b);
            String scheme2 = ForeignUri.sysSpecScheme(spec2);
            String authority2 = ForeignUri.sysSpecAuthority(spec2);

            if (ForeignFile.STRING_EMPTY.equals(scheme2) &&
                    ForeignFile.STRING_EMPTY.equals(authority2)) {
                String spec1 = ForeignUri.sysUriSpec(a);
                String scheme1 = ForeignUri.sysSpecScheme(spec1);
                String authority1 = ForeignUri.sysSpecAuthority(spec1);

                String path1 = ForeignUri.sysSpecPath(spec1);
                String path2 = ForeignUri.sysSpecPath(spec2);
                path1 = ForeignFile.sysPathAbsolute(path1, path2);
                spec1 = ForeignUri.sysSpecMake(scheme1, authority1, path1);
                String query = ForeignUri.sysUriQuery(b);
                String hash = ForeignUri.sysUriHash(b);
                return ForeignUri.sysUriMake(spec1, query, hash);
            } else {
                return b;
            }
        } catch (MalformedURLException x) {
            throw new RuntimeException("shouldn't happen", x);
        }
    }

    /**
     * <p>Determine the relative URI.</p>
     *
     * @param a The base URI.
     * @param b The absolute URI.
     * @return The relative or absolute URI.
     */
    public static String sysUriRelative(String a, String b)
            throws MalformedURLException {
        String spec1 = ForeignUri.sysUriSpec(a);
        String scheme1 = ForeignUri.sysSpecScheme(spec1);
        String authority1 = ForeignUri.sysSpecAuthority(spec1);

        String spec2 = ForeignUri.sysUriSpec(b);
        String scheme2 = ForeignUri.sysSpecScheme(spec2);
        String authority2 = ForeignUri.sysSpecAuthority(spec2);

        if (scheme1.equals(scheme2) && authority1.equals(authority2)) {
            String path1 = ForeignUri.sysSpecPath(spec1);
            String path2 = ForeignUri.sysSpecPath(spec2);
            path1 = ForeignFile.sysPathRelative(path1, path2);
            spec1 = ForeignUri.sysSpecMake(ForeignFile.STRING_EMPTY,
                    ForeignFile.STRING_EMPTY, path1);
            String query = ForeignUri.sysUriQuery(b);
            String hash = ForeignUri.sysUriHash(b);
            return ForeignUri.sysUriMake(spec1, query, hash);
        } else {
            return b;
        }
    }

    /************************************************************/
    /* Canonical URI                                           */
    /************************************************************/
    /**
     * <p>Determine a canonical URI and schemefy.</p>
     *
     * @param adr The uri.
     * @return The canonical and schemefied URI.
     * @throws MalformedURLException    Spec assembling problem.
     * @throws CharacterCodingException File canonization problem.
     */
    public static String sysCanonicalUri(String adr)
            throws IOException {
        String spec = ForeignUri.sysUriSpec(adr);
        spec = ForeignUri.sysCanonicalSpec(spec);
        String scheme = ForeignUri.sysSpecScheme(spec);
        if (SCHEME_FILE.equals(scheme)) {
            /* remove the query for file */
            String hash = ForeignUri.sysUriHash(adr);
            adr = ForeignUri.sysUriMake(spec, ForeignFile.STRING_EMPTY, hash);
        } else {
            String query = ForeignUri.sysUriQuery(adr);
            query = ForeignUri.decodeEncodeQuery(query);
            String hash = ForeignUri.sysUriHash(adr);
            adr = ForeignUri.sysUriMake(spec, query, hash);
            adr = ForeignDomain.sysUriUnpuny(adr);
            adr = derefUri(adr);
        }
        return adr;
    }

    /**
     * <p>Deref an uri.</p>
     *
     * @param adr The uri.
     * @return The derefed uri.
     * @throws IOException Shit happen.
     */
    private static String derefUri(String adr)
            throws IOException {
        for (; ; ) {
            String res;
            try {
                res = OpenCheck.DEFAULT_CHECK.checkRedirect(adr);
            } catch (IOException x) {
                if (OpenCheck.isInterrupt(x)) {
                    throw x;
                } else {
                    res = null;
                }
            }
            if (res == null)
                break;
            adr = res;
        }
        return adr;
    }

    /**
     * <p>Determine a canonical spec and schemefy.</p>
     *
     * @param spec The spec.
     * @return The canonical and schemefied spec.
     * @throws MalformedURLException    Spec assembling problem.
     * @throws CharacterCodingException File canonization problem.
     */
    private static String sysCanonicalSpec(String spec)
            throws IOException {
        String scheme = ForeignUri.sysSpecScheme(spec);
        String authority = ForeignUri.sysSpecAuthority(spec);
        String path = ForeignUri.sysSpecPath(spec);
        if (SCHEME_JAR.equals(scheme)) {
            int k = path.lastIndexOf(ForeignUri.JAR_SEP);
            if (k != -1) {
                spec = sysSpecMake(ForeignFile.STRING_EMPTY, authority, path.substring(0, k));
                spec = ForeignUri.sysCanonicalUri(spec);
                spec = ForeignUri.sysSpecMake(SCHEME_JAR, ForeignFile.STRING_EMPTY,
                        spec + path.substring(k));
            } else {
                spec = sysSpecMake(ForeignFile.STRING_EMPTY, authority, path);
                spec = ForeignUri.sysCanonicalUri(spec);
                spec = ForeignUri.sysSpecMake(SCHEME_JAR, ForeignFile.STRING_EMPTY, spec);
            }
        } else if (SCHEME_FILE.equals(scheme)) {
            /* remove the authority for file */
            spec = ForeignUri.sysSpecMake(SCHEME_FILE, ForeignFile.STRING_EMPTY,
                    ForeignFile.sysCanonicalPath(path));
        } else if (ForeignFile.STRING_EMPTY.equals(scheme) &&
                ForeignFile.STRING_EMPTY.equals(authority) &&
                !ForeignFile.sysPathIsRelative(path)) {
            /* remove the authority for file */
            spec = ForeignUri.sysSpecMake(SCHEME_FILE, ForeignFile.STRING_EMPTY,
                    ForeignFile.sysCanonicalPath(path));
        } else if (ForeignFile.STRING_EMPTY.equals(scheme) &&
                !ForeignFile.STRING_EMPTY.equals(authority)) {
            spec = ForeignUri.sysSpecMake(SCHEME_HTTP, authority, path);
        } else {
            /* */
        }
        return spec;
    }

    /*******************************************************************/
    /* URI Encoding/Decoding                                           */
    /*******************************************************************/

    /**
     * <p>Encode an uri.</p>
     * <p>The spec, the query and the hash will be ASCII encoded.</p>
     * <p>The encoding will be percent encoding.</p>
     *
     * @param adr The uri.
     * @return The encoded uri.
     */
    public static String sysUriEncode(String adr) {
        return encode(adr, true, null, ENCODING_UTF8);
    }

    /**
     * <p>Decode an url.</p>
     * <p>The spec, the query and the hash will be decoded and minimal encoded.</p>
     * <p>The minimal encoding will be percent encoding.</p>
     *
     * @param adr The uri.
     * @return The decoded uri.
     */
    public static String sysUriDecode(String adr) {
        String spec = ForeignUri.sysUriSpec(adr);
        String query = ForeignUri.sysUriQuery(adr);
        String hash = ForeignUri.sysUriHash(adr);

        query = ForeignUri.decodeEncodeQuery(query);
        return ForeignUri.sysUriMake(spec, query, hash);
    }

    /**
     * <p>Decode and minimal encode the query.</p>
     * <p>The minimal encoding will be percent encoding.</p>
     *
     * @param s The query.
     * @return The decoded query.
     */
    private static String decodeEncodeQuery(String s) {
        if (ForeignFile.STRING_EMPTY.equals(s))
            return ForeignFile.STRING_EMPTY;
        StringBuilder buf = new StringBuilder();
        int k1 = 0;
        int k = s.indexOf(CHAR_AMP, k1);
        while (k != -1) {
            buf.append(decodeEncodePair(s.substring(k1, k)));
            buf.appendCodePoint(CHAR_AMP);
            k1 = k + 1;
            k = s.indexOf(CHAR_AMP, k1);
        }
        buf.append(decodeEncodePair(s.substring(k1)));
        return buf.toString();
    }

    /**
     * <p>Decode and minimal encode a pair.</p>
     * <p>The minimal encoding will be percent encoding.</p>
     *
     * @param s The pair.
     * @return The decoded pair.
     */
    private static String decodeEncodePair(String s) {
        int k = s.indexOf(CHAR_EQ);
        String value;
        if (k != -1) {
            value = s.substring(k + 1);
            s = s.substring(0, k);
        } else {
            value = ForeignFile.STRING_EMPTY;
        }
        s = encode(decode(s, ENCODING_UTF8), false, NEEDS_COMP, ENCODING_UTF8);
        if (!ForeignFile.STRING_EMPTY.equals(value))
            s += "=" + encode(decode(value, ENCODING_UTF8), false, NEEDS_COMP, ENCODING_UTF8);
        return s;
    }

    /*******************************************************************/
    /* Encoding/Decoding Helper                                        */
    /*******************************************************************/

    /**
     * <p>URL encode a string.</p>
     *
     * @param s     The string.
     * @param above The above flag.
     * @param needs The needs set.
     * @param cset  The character set.
     * @return The URL encoded string.
     */
    public static String encode(String s, boolean above,
                                String needs, String cset) {
        try {
            StringBuilder buf = null;
            StringBuilder enc = null;
            int n = s.length();
            int pos = 0;
            while (pos < n) {
                int ch = s.codePointAt(pos);
                if ((above && (ch >= 0x80 || ch == '+' || ch <= 0x20)) ||
                        (needs != null && needs.indexOf(ch) != -1)) {
                    if (buf == null)
                        buf = new StringBuilder(s.substring(0, pos));
                    if (enc == null)
                        enc = new StringBuilder();
                    enc.appendCodePoint(ch);
                } else {
                    if (enc != null) {
                        hexDigits(buf, enc, cset);
                        enc = null;
                    }
                    if (buf != null)
                        buf.appendCodePoint(ch);
                }
                pos += Character.charCount(ch);
            }
            if (enc != null)
                hexDigits(buf, enc, cset);
            if (buf != null)
                return buf.toString();
            return s;
        } catch (UnsupportedEncodingException x) {
            throw new RuntimeException(SHOULDNT_HAPPEN, x);
        }
    }

    /**
     * <p>Convert characters to hex digit pairs.</p>
     *
     * @param buf  The destination buffer.
     * @param enc  The unencoded characters.
     * @param cset The character set.
     */
    private static void hexDigits(StringBuilder buf, StringBuilder enc,
                                  String cset)
            throws UnsupportedEncodingException {
        byte[] bytes = enc.toString().getBytes(cset);
        for (int i = 0; i < bytes.length; i++) {
            int val = bytes[i] & 0xFF;
            buf.appendCodePoint('%');
            buf.appendCodePoint(hexDigit(val / 16));
            buf.appendCodePoint(hexDigit(val % 16));
        }
    }

    /**
     * <p>Convert a value to a hex digit.</p>
     *
     * @param value The value.
     * @return The hex digit.
     */
    private static int hexDigit(int value) {
        if (value <= 9) {
            return '0' + value;
        } else {
            return 'A' + value - 10;
        }
    }

    /**
     * <p>URL decode a string.</p>
     *
     * @param s    The string.
     * @param cset The character set.
     * @return The URL decoded string.
     */
    public static String decode(String s, String cset) {
        try {
            StringBuilder buf = null;
            ByteArrayOutputStream bs = null;
            int n = s.length();
            int pos = 0;
            while (pos < n) {
                int ch = s.codePointAt(pos);
                if (ch == '%') {
                    int k = pos;
                    pos += Character.charCount(ch);
                    int i = 0;
                    int val;
                    int octet = 0;
                    while (pos < n && i < 2 &&
                            (val = digitHex(ch = s.codePointAt(pos))) != -1) {
                        octet = octet * 16 + val;
                        pos += Character.charCount(ch);
                        i++;
                    }
                    if (i == 2) {
                        if (buf == null)
                            buf = new StringBuilder(s.substring(0, k));
                        if (bs == null)
                            bs = new ByteArrayOutputStream();
                        bs.write(octet);
                    } else {
                        if (bs != null) {
                            buf.append(bs.toString(cset));
                            bs = null;
                        }
                        if (buf != null)
                            buf.append(s.substring(k, pos));
                    }
                } else {
                    if (bs != null) {
                        buf.append(bs.toString(cset));
                        bs = null;
                    }
                    if (buf != null)
                        buf.appendCodePoint(ch);
                    pos += Character.charCount(ch);
                }
            }
            if (bs != null)
                buf.append(bs.toString(cset));
            if (buf != null)
                return buf.toString();
            return s;
        } catch (UnsupportedEncodingException x) {
            throw new RuntimeException(SHOULDNT_HAPPEN, x);
        }
    }

    /**
     * <p>Convert a hex digit to a value.</p>
     *
     * @param digit The hex digit.
     * @return The value or -1.
     */
    private static int digitHex(int digit) {
        if ('0' <= digit && digit <= '9') {
            return digit - '0';
        } else if ('a' <= digit && digit <= 'f') {
            return digit - 'a' + 10;
        } else if ('A' <= digit && digit <= 'F') {
            return digit - 'A' + 10;
        } else {
            return -1;
        }
    }

    /**
     * <p>Some test.</p>
     *
     * @param args The arguments, unused.
     * @throws MalformedURLException    Spec assembling problem.
     * @throws CharacterCodingException File canonization problem.
     */
    /*
    public static void main(String[] args) throws UnsupportedEncodingException {
        String str = "a\uD834\uDD1Eb";
        String str2 = sysUriEncode(str);
        System.out.println("encode(" + str + ")=" + str2);

        str2 = URLEncoder.encode(str, "utf-8");
        System.out.println("encode(" + str + ")=" + str2);
    }
    */

    /**
     * <p>Some test.</p>
     *
     * @param args The arguments, unused.
     * @throws MalformedURLException    Spec assembling problem.
     * @throws CharacterCodingException File canonization problem.
     */
    /*
    public static void main(String[] args)
            throws IOException {
        String spec = "jar://abc!/foo";
        System.out.println("spec=" + spec);
        spec = ForeignUri.sysCanonicalSpec(spec);
        System.out.println("canonical(spec)=" + spec);

        System.out.println();

        spec = "file://abc/def/ghi";
        System.out.println("spec=" + spec);
        spec = ForeignUri.sysCanonicalSpec(spec);
        System.out.println("canonical(spec)=" + spec);

        System.out.println();

        spec = "/def/ghi";
        System.out.println("spec=" + spec);
        spec = ForeignUri.sysCanonicalSpec(spec);
        System.out.println("canonical(spec)=" + spec);

        System.out.println();

        spec = "//abc/def/ghi";
        System.out.println("spec=" + spec);
        spec = ForeignUri.sysCanonicalSpec(spec);
        System.out.println("canonical(spec)=" + spec);

        System.out.println();

        spec = "mailto:foo@bar.com";
        System.out.println("spec=" + spec);
        spec = ForeignUri.sysCanonicalSpec(spec);
        System.out.println("canonical(spec)=" + spec);

        System.out.println();

        String uri = "jar://abc!/foo#hash";
        System.out.println("uri=" + uri);
        uri = ForeignUri.sysCanonicalUri(uri);
        System.out.println("canonical(uri)=" + uri);

        System.out.println();

        uri = "file://abc/def/ghi?query";
        System.out.println("uri=" + uri);
        uri = ForeignUri.sysCanonicalUri(uri);
        System.out.println("canonical(uri)=" + uri);

        System.out.println();

        uri = "/def/ghi?query#hash";
        System.out.println("uri=" + uri);
        uri = ForeignUri.sysCanonicalUri(uri);
        System.out.println("canonical(uri)=" + uri);

        System.out.println();

        uri = "//abc/def/ghi?query#hash";
        System.out.println("uri=" + uri);
        uri = ForeignUri.sysCanonicalUri(uri);
        System.out.println("canonical(uri)=" + uri);

        System.out.println();

        uri = "mailto:foo@bar.com?query#hash";
        System.out.println("uri=" + uri);
        uri = ForeignUri.sysCanonicalUri(uri);
        System.out.println("canonical(uri)=" + uri);

        System.out.println();

        String spec = "http://nürnberg.de/robots.txt";
        System.out.println("spec=" + spec);
        spec = ForeignUri.sysCanonicalSpec(spec);
        System.out.println("canonical(spec)=" + spec);
    }
    */

}

