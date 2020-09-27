package matula.util.config;

import matula.util.data.ListArray;
import matula.util.system.ForeignFile;
import matula.util.system.ForeignUri;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * More foreign predicates for the module system/file.
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
public final class ForeignArchive {
    private final static char CHAR_PACK = '-';

    /*****************************************************************/
    /* OS Directores                                                 */
    /*****************************************************************/

    /**
     * <p>List the entries of a directory.</p>
     *
     * @param list The target list.
     * @param f    The file.
     * @return The target list.
     */
    public static ListArray<String> listDirectory(ListArray<String> list,
                                                  File f) {
        File[] fs = f.listFiles();
        if (fs == null)
            return null;
        for (int i = 0; i < fs.length; i++) {
            f = fs[i];
            String name;
            if (f.isDirectory()) {
                name = f.getName() + "/";
            } else {
                name = f.getName();
            }
            if (list == null) {
                list = new ListArray<String>();
                list.add(name);
            } else {
                if (!list.contains(name))
                    list.add(name);
            }
        }
        return list;
    }

    /*****************************************************************/
    /* ZIP Files                                                     */
    /*****************************************************************/

    /**
     * <p>List the entries of an archive.</p>
     * <p>Will close and thas release the inflater.</p>
     *
     * @param list   The target list.
     * @param in     The archive stream.
     * @param prefix The prefix filter.
     * @return The target list.
     * @throws IOException Shit happens.
     */
    public static ListArray<String> listArchive(ListArray<String> list,
                                                InputStream in, String prefix)
            throws IOException {
        ZipInputStream zip = new ZipInputStream(
                new BufferedInputStream(in, 8192));
        try {
            ZipEntry e = zip.getNextEntry();
            for (; e != null; e = zip.getNextEntry()) {
                String name = e.getName();
                if (name.length() == prefix.length())
                    continue;
                if (!name.startsWith(prefix))
                    continue;
                int k = name.indexOf('/', prefix.length());
                if (k != -1) {
                    name = name.substring(prefix.length(), k + 1);
                } else {
                    name = name.substring(prefix.length());
                }
                if (list == null) {
                    list = new ListArray<String>();
                    list.add(name);
                } else {
                    if (!list.contains(name))
                        list.add(name);
                }
            }
        } catch (IOException x) {
            zip.close();
            throw x;
        }
        zip.close();
        return list;
    }


    /**
     * <p>Check whether an entry exists.</p>
     *
     * @param in   The archive stream.
     * @param name The entry name.
     * @return True if the entry exist, otherwise false.
     * @throws IOException Shit happens.
     */
    public static boolean existsEntry(InputStream in, String name)
            throws IOException {
        ZipInputStream zip = new ZipInputStream(
                new BufferedInputStream(in, 8192));
        boolean found = false;
        try {
            ZipEntry e = zip.getNextEntry();
            for (; e != null && !found; e = zip.getNextEntry())
                found = name.equals(e.getName());
        } catch (IOException x) {
            zip.close();
            throw x;
        }
        zip.close();
        return found;
    }

    /*****************************************************************/
    /* URI Paths                                                     */
    /*****************************************************************/

    /**
     * <p>Extract a file path from an uri address.</p>
     *
     * @param adr The uri address.
     * @return The file path or null.
     */
    public static String extractPath(String adr) {
        String spec = ForeignUri.sysUriSpec(adr);
        String scheme = ForeignUri.sysSpecScheme(spec);
        if (!ForeignUri.SCHEME_FILE.equals(scheme))
            return null;
        String path = ForeignUri.sysSpecPath(spec);
        return path.replace('/', File.separatorChar);
    }

    /**
     * <p>Condense a file path to an uri address.</p>
     *
     * @param path The file path.
     * @return The uri address.
     */
    public static String pathCondense(String path) {
        try {
            String spec = ForeignUri.sysSpecMake(ForeignUri.SCHEME_FILE,
                    ForeignFile.STRING_EMPTY, path);
            return ForeignUri.sysUriMake(spec, ForeignFile.STRING_EMPTY,
                    ForeignFile.STRING_EMPTY);
        } catch (MalformedURLException x) {
            throw new RuntimeException("shouldn't happen", x);
        }
    }

    /******************************************************/
    /* Package Name Helper                                */
    /******************************************************/

    /**
     * <p>Retrieve the name of a package name.</p>
     *
     * @param p The package name.
     * @return The name.
     */
    public static String sysPackName(String p) {
        int k = p.lastIndexOf(CHAR_PACK);
        if (k != -1) {
            return p.substring(0, k);
        } else {
            return p;
        }
    }

    /**
     * <p>Retrieve the version of a package name.</p>
     *
     * @param p The package name.
     * @return The version.
     */
    public static String sysPackVersion(String p) {
        int k = p.lastIndexOf(CHAR_PACK);
        if (k != -1) {
            return p.substring(k + 1);
        } else {
            return "";
        }
    }

    /**
     * <p>Create a package name from name and version.</p>
     *
     * @param n The name-
     * @param v The version.
     * @return The package name.
     */
    public static String sysPackMake(String n, String v) {
        if (!"".equals(v)) {
            return n + CHAR_PACK + v;
        } else {
            return n;
        }
    }

}