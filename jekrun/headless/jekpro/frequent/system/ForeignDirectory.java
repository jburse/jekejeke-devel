package jekpro.frequent.system;

import jekpro.model.molec.EngineMessage;
import jekpro.tools.call.ArrayEnumeration;
import jekpro.tools.call.CallOut;
import jekpro.tools.call.InterpreterMessage;
import matula.util.data.ListArray;
import matula.util.system.ForeignUri;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * <p>The foreign predicates for the module system/file.</p>
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
public final class ForeignDirectory {

    /************************************************************/
    /* File Ops                                                 */
    /************************************************************/

    /**
     * <p>Create a file.</p>
     *
     * @param uristr The file name.
     * @return True if the file could be created, otherwise false.
     * @throws InterpreterMessage Not file scheme.
     */
    public static boolean sysCreateFile(String uristr)
            throws InterpreterMessage, IOException {
        File f = new File(ForeignDirectory.uriToFilePath(uristr));
        return f.createNewFile();
    }

    /**
     * <p>Delete a file.</p>
     *
     * @param uristr The file name.
     * @return True if the file could be deleted, otherwise false.
     * @throws InterpreterMessage Not file scheme.
     */
    public static boolean sysDeleteFile(String uristr)
            throws InterpreterMessage {
        File f = new File(ForeignDirectory.uriToFilePath(uristr));
        return f.delete();
    }

    /**
     * <p>Rename a file.</p>
     *
     * @param uristr1 The old file name.
     * @param uristr2 The new file anme.
     * @return True if the file could be renamed, otherwise false.
     */
    public static boolean sysRenameFile(String uristr1, String uristr2)
            throws InterpreterMessage {
        File f1 = new File(ForeignDirectory.uriToFilePath(uristr1));
        File f2 = new File(ForeignDirectory.uriToFilePath(uristr2));
        return f1.renameTo(f2);
    }

    /************************************************************/
    /* Directory Ops                                            */
    /************************************************************/

    /**
     * <p>Create a directory.</p>
     *
     * @param uristr The file name.
     * @return True if the directory could be created, otherwise false.
     * @throws InterpreterMessage Not file scheme.
     */
    public static boolean sysMakeDirectory(String uristr)
            throws InterpreterMessage {
        File f = new File(ForeignDirectory.uriToFilePath(uristr));
        return f.mkdir();
    }

    /**
     * <p>Enumerate the entries of a directory.</p>
     *
     * @param co     The call out.
     * @param uristr The directory name.
     * @return The relative entries.
     * @throws InterpreterMessage Not file scheme.
     */
    public static String sysDirectoryFile(CallOut co, String uristr)
            throws InterpreterMessage {
        ArrayEnumeration<File> dc;
        if (co.getFirst()) {
            File f = new File(uriToFilePath(uristr));
            File[] list = f.listFiles();
            if (list == null)
                return null;
            dc = new ArrayEnumeration<File>(list);
            co.setData(dc);
        } else {
            dc = (ArrayEnumeration<File>) co.getData();
        }
        if (!dc.hasMoreElements())
            return null;
        File f = dc.nextElement();
        co.setRetry(dc.hasMoreElements());
        if (f.isDirectory()) {
            return f.getName() + "/";
        } else {
            return f.getName();
        }
    }

    /**
     * <p>Check whether a file is a directory.</p>
     *
     * @param uristr The file name.
     * @return True if the file is a directory, otherwise false.
     * @throws InterpreterMessage Not file scheme.
     */
    public static boolean sysIsFile(String uristr)
            throws InterpreterMessage {
        File f = new File(ForeignDirectory.uriToFilePath(uristr));
        return f.isFile();
    }

    /**
     * <p>Check whether a file is a directory.</p>
     *
     * @param uristr The file name.
     * @return True if the file is a directory, otherwise false.
     * @throws InterpreterMessage Not file scheme.
     */
    public static boolean sysIsDirectory(String uristr)
            throws InterpreterMessage {
        File f = new File(ForeignDirectory.uriToFilePath(uristr));
        return f.isDirectory();
    }

    /************************************************************/
    /* Time Stamp                                               */
    /************************************************************/

    /**
     * <p>Retrieve the last modified date of a file.</p>
     *
     * @param uristr The file name.
     * @return The last modified as a long.
     * @throws InterpreterMessage Not file scheme.
     */
    public static long sysGetTimeFile(String uristr)
            throws InterpreterMessage {
        File f = new File(uriToFilePath(uristr));
        return f.lastModified();
    }

    /**
     * <p>Set the last modified date of a file.</p>
     *
     * @param uristr The file name.
     * @param date   The last modified as a long.
     * @return True if the date was set, otherwise false.
     * @throws InterpreterMessage Not file scheme.
     */
    public static boolean sysSetTimeFile(String uristr, long date)
            throws InterpreterMessage {
        File f = new File(uriToFilePath(uristr));
        return f.setLastModified(date);
    }

    /************************************************************************/
    /* uri Helper                                                           */
    /************************************************************************/

    /**
     * <p>Extract the file path from an uri and replace separator.</p>
     *
     * @param adr The uri string.
     * @return The file path.
     */
    public static String uriToFilePath(String adr)
            throws InterpreterMessage {
        String spec = ForeignUri.sysUriSpec(adr);
        String scheme = ForeignUri.sysSpecScheme(spec);
        if (!ForeignUri.SCHEME_FILE.equals(scheme))
            throw new InterpreterMessage(InterpreterMessage.permissionError(
                    InterpreterMessage.OP_PERMISSION_ACCESS,
                    EngineMessage.OP_PERMISSION_SOURCE_SINK, adr));
        String path = ForeignUri.sysSpecPath(spec);
        return path.replace('/', File.separatorChar);
    }

    /*****************************************************************/
    /* Environment Variables                                         */
    /*****************************************************************/

    /**
     * <p>List the environment variable names.</p>
     *
     * @param co The call out.
     * @return The environment variable names.
     */
    public static String sysCurrentEnv(CallOut co) {
        ArrayEnumeration<String> dc;
        if (co.getFirst()) {
            Set<String> keys = System.getenv().keySet();
            String[] names = new String[keys.size()];
            keys.toArray(names);
            dc = new ArrayEnumeration<String>(names);
            co.setData(dc);
        } else {
            dc = (ArrayEnumeration<String>) co.getData();
        }
        if (!dc.hasMoreElements())
            return null;
        String res = dc.nextElement();
        co.setRetry(dc.hasMoreElements());
        return res;
    }

    /*****************************************************************/
    /* ZIP Files                                                     */
    /*****************************************************************/

    /**
     * <p>Enuerate the entries of an archive.</p>
     *
     * @param co     The call out.
     * @param uristr The archive name.
     * @param prefix The prefix filter.
     * @return The relative entries.
     * @throws IOException        Shit happens.
     * @throws InterpreterMessage Not file scheme.
     */
    public static String sysArchiveFile(CallOut co, String uristr, String prefix)
            throws InterpreterMessage, IOException {
        ArrayEnumeration<String> dc;
        if (co.getFirst()) {
            File f = new File(uriToFilePath(uristr));
            ZipInputStream zip = new ZipInputStream(new FileInputStream(f));
            String[] list;
            try {
                list = listArchive(zip, prefix);
            } catch (IOException x) {
                zip.close();
                throw x;
            }
            zip.close();
            if (list == null)
                return null;
            dc = new ArrayEnumeration<String>(list);
            co.setData(dc);
        } else {
            dc = (ArrayEnumeration<String>) co.getData();
        }
        if (!dc.hasMoreElements())
            return null;
        String res = dc.nextElement();
        co.setRetry(dc.hasMoreElements());
        return res;
    }

    /**
     * <p>List the entries of an archive.</p>
     *
     * @param zip    The archive stream.
     * @param prefix The prefix filter.
     * @return The entries or null.
     * @throws IOException Shit happens.
     */
    private static String[] listArchive(ZipInputStream zip, String prefix)
            throws IOException {
        ListArray<String> list = null;
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
        if (list == null)
            return null;
        String[] res = new String[list.size()];
        list.toArray(res);
        return res;
    }

}
