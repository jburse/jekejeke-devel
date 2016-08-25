package jekpro.frequent.system;

import jekpro.frequent.stream.ForeignStream;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.TermCompound;
import matula.util.system.ForeignUri;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;

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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class ForeignDirectory {

    /************************************************************/
    /* File Ops                                                 */
    /************************************************************/

    /**
     * <p>Delete a file.</p>
     *
     * @param uristr The file name.
     * @return True if the file could be deleted, otherwise false.
     */
    public static boolean sysDeleteFile(String uristr)
            throws UnsupportedEncodingException, InterpreterMessage {
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
            throws UnsupportedEncodingException, InterpreterMessage {
        File f1 = new File(ForeignDirectory.uriToFilePath(uristr1));
        File f2 = new File(ForeignDirectory.uriToFilePath(uristr2));
        return f1.renameTo(f2);
    }

    /************************************************************/
    /* Directory Ops                                            */
    /************************************************************/

    /**
     * <p>Check whether a file is a directory.</p>
     *
     * @param uristr The file name.
     * @return True if the file is a directory, otherwise false.
     */
    public static boolean sysIsDirectory(String uristr)
            throws UnsupportedEncodingException, InterpreterMessage {
        File f = new File(ForeignDirectory.uriToFilePath(uristr));
        return f.isDirectory();
    }

    /**
     * <p>Create a directory.</p>
     *
     * @param uristr The file name.
     * @return True if the directory could be created, otherwise false.
     */
    public static boolean sysMakeDirectory(String uristr)
            throws UnsupportedEncodingException, InterpreterMessage {
        File f = new File(ForeignDirectory.uriToFilePath(uristr));
        return f.mkdir();
    }

    /**
     * <p>List the entries of a directory.</p>
     *
     * @param uristr The file name.
     * @return The relative entries.
     */
    public static Object sysDirectoryFiles(String uristr)
            throws MalformedURLException, UnsupportedEncodingException, InterpreterMessage {
        File f = new File(uriToFilePath(uristr));
        String[] list = f.list();
        Object res = Knowledgebase.OP_NIL;
        if (list == null)
            return res;
        for (int i = list.length - 1; i >= 0; i--) {
            String spec = ForeignUri.sysSpecMake("", "", list[i]);
            String adr = ForeignUri.sysUriMake(spec, "", "");
            res = new TermCompound(Knowledgebase.OP_CONS,
                    adr, res);
        }
        return res;
    }

    /************************************************************/
    /* Time Stamp                                               */
    /************************************************************/

    /**
     * <p>Retrieve the last modified date of a file.</p>
     *
     * @param uristr The file name.
     * @return The last modified as a long.
     * @throws UnsupportedEncodingException Problem decoding uri.
     * @throws InterpreterMessage           Not file scheme.
     */
    public static long sysGetTimeFile(String uristr)
            throws UnsupportedEncodingException, InterpreterMessage {
        File f = new File(uriToFilePath(uristr));
        return f.lastModified();
    }

    /**
     * <p>Set the last modified date of a file.</p>
     *
     * @param uristr The file name.
     * @param date   The last modified as a long.
     * @return True if the date was set, otherwise false.
     * @throws UnsupportedEncodingException Problem decoding uri.
     * @throws InterpreterMessage           Not file scheme.
     */
    public static boolean sysSetTimeFile(String uristr, long date)
            throws UnsupportedEncodingException, InterpreterMessage {
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
            throws InterpreterMessage, UnsupportedEncodingException {
        String spec = ForeignUri.sysUriSpec(adr);
        String scheme = ForeignUri.sysSpecScheme(spec);
        if (!ForeignUri.SCHEME_FILE.equals(scheme))
            throw new InterpreterMessage(InterpreterMessage.permissionError(
                    ForeignStream.OP_PERMISSION_ACCESS, EngineMessage.OP_PERMISSION_SOURCE_SINK, adr));
        String path = ForeignUri.sysSpecPath(spec);
        return path.replace('/', File.separatorChar);
    }

}
