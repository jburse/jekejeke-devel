package matula.util.system;

import java.io.File;
import java.io.IOException;
import java.nio.charset.CharacterCodingException;

/**
 * The foreign predicates for the module system/file.
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ForeignFile {
    static final char CHAR_SLASH = '/';

    /************************************************************/
    /* Name Assembly                                            */
    /************************************************************/

    /**
     * <p>Retrieve the base name.</p>
     *
     * @param n name.
     * @return The base name.
     */
    public static String sysNameBase(String n) {
        int k = n.lastIndexOf('.');
        if (k == -1)
            return n;
        return n.substring(0, k);
    }

    /**
     * <p>Retrieve the extension.</p>
     *
     * @param n The name.
     * @return The extension.
     */
    public static String sysNameExt(String n) {
        int k = n.lastIndexOf('.');
        if (k == -1)
            return "";
        return n.substring(k + 1);
    }

    /**
     * <p>Create a name by base name and extension.</p>
     *
     * @param b The base name.
     * @param e The extension.
     * @return The name.
     */
    public static String sysNameMake(String b, String e) {
        if ("".equals(e))
            return b;
        return b + "." + e;
    }

    /************************************************************/
    /* Path Assembly                                            */
    /************************************************************/

    /**
     * <p>Retrieve the path directory.</p>
     * <p>Should also handle //.</p>
     *
     * @param p The path.
     * @return The directory.
     */
    public static String sysPathDirectory(String p) {
        int k = p.lastIndexOf(CHAR_SLASH);
        if (k == -1)
            return "";
        if (k == 0)
            return "/";
        return p.substring(0, k);
    }

    /**
     * <p>Retrieve the path name.</p>
     *
     * @param p The path.
     * @return The name.
     */
    public static String sysPathName(String p) {
        int k = p.lastIndexOf(CHAR_SLASH);
        if (k == -1)
            return p;
        return p.substring(k + 1);
    }

    /**
     * <p>Create a path by directory and name.</p>
     * <p>Should also handle //.</p>
     *
     * @param d The directory.
     * @param n The name.
     * @return The path.
     */
    public static String sysPathMake(String d, String n) {
        if ("".equals(d))
            return n;
        if ("/".equals(d))
            return "/" + n;
        return d + "/" + n;
    }

    /************************************************************/
    /* Path Following                                           */
    /************************************************************/

    /**
     * <p>Check whether a path is relative.</p>
     * <p>Special casing for drive letters.</p>
     *
     * @param path The path.
     * @return True if it is relative.
     */
    public static boolean sysPathIsRelative(String path) {
        int k = ForeignUri.getSchemeLength(path);
        if (k == ForeignUri.SCHEME_DRIVE) {
            if (path.startsWith("/", ForeignUri.SCHEME_DRIVE))
                return false;
            return true;
        }
        if (path.startsWith("/"))
            return false;
        return true;
    }

    /**
     * <p>Determine the absolute path.</p>
     * <p>Should also handle // with /.</p>
     *
     * @param a The base path.
     * @param b The relative or absolute path.
     * @return The absolute path.
     */
    public static String sysPathAbsolute(String a, String b) {
        if ("".equals(b))
            return a;
        if (!ForeignFile.sysPathIsRelative(b))
            return b;
        int k = a.lastIndexOf(CHAR_SLASH);
        int j = 0;
        while (b.startsWith("../", j)) {
            k = a.lastIndexOf(CHAR_SLASH, k - 1);
            j += 3;
        }
        return a.substring(0, k) + "/" + b.substring(j);
    }

    /**
     * <p>Determine the relative path.</p>
     * <p>Should also handle // with /.</p>
     *
     * @param a The base path.
     * @param b The absolute path.
     * @return The relative or absolute path.
     */
    public static String sysPathRelative(String a, String b) {
        if (a.equals(b))
            return "";
        int j = ForeignFile.commonPrefix(a, b);
        if (j == 0)
            return b;
        return ForeignFile.backNavigation(a.substring(j)) + b.substring(j);
    }

    /**
     * <p>Determine the common prefix of two paths.</p>
     *
     * @param a The first path.
     * @param b The second path.
     * @return The length of the common prefix.
     */
    private static int commonPrefix(String a, String b) {
        int j = 0;
        int k1 = a.indexOf(CHAR_SLASH, j);
        int k2 = b.indexOf(CHAR_SLASH, j);
        while (k1 != -1 && k2 != -1) {
            if (k1 != k2)
                return j;
            if (!a.substring(j, k1).equals(b.substring(j, k2)))
                return j;
            j = k1 + 1;
            k1 = a.indexOf(CHAR_SLASH, j);
            k2 = b.indexOf(CHAR_SLASH, j);
        }
        return j;
    }

    /**
     * <p>Turn a path into a back navigation.</p>
     *
     * @param a The path.
     * @return The back navigation.
     */
    private static String backNavigation(String a) {
        StringBuilder buf = null;
        int j = 0;
        int k = a.indexOf(CHAR_SLASH, j);
        while (k != -1) {
            if (buf == null)
                buf = new StringBuilder();
            buf.append("../");
            j = k + 1;
            k = a.indexOf(CHAR_SLASH, j);
        }
        if (buf == null)
            return "";
        return buf.toString();
    }

    /************************************************************/
    /* Canonical Path                                           */
    /************************************************************/

    /**
     * <p>Determine canonical path and slashify.</p>
     *
     * @param path The path.
     * @return The canonized and slashified path.
     * @throws CharacterCodingException File canonization problem.
     */
    public static String sysCanonicalPath(String path)
            throws CharacterCodingException {
        try {
            if (File.separatorChar != CHAR_SLASH)
                path = path.replace(CHAR_SLASH, File.separatorChar);
            File file = new File(path).getCanonicalFile();
            path = file.toString();
            if (File.separatorChar != CHAR_SLASH)
                path = path.replace(File.separatorChar, CHAR_SLASH);
            if (!path.startsWith("/"))
                path = "/" + path;
            if (!path.endsWith("/") && file.isDirectory())
                path = path + "/";
            return path;
        } catch (IOException x) {
            throw new CharacterCodingException();
        }
    }

}
