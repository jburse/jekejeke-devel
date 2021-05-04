package matula.util.swing;

import matula.util.config.GestaltEntry;
import matula.util.data.ListArray;
import matula.util.system.ForeignArchive;
import matula.util.system.ForeignFile;
import matula.util.system.ForeignUri;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.jar.Attributes;
import java.util.jar.JarInputStream;
import java.util.jar.Manifest;

/**
 * <p>Hotspot current directory and path discovery.</p>
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
public final class SwingGestalt {

    /**
     * <p>Retrieve the base.</p>
     *
     * @return The base.
     * @throws IOException Shit happens.
     */
    public static String getBase() throws IOException {
        String path = System.getProperty("user.dir");
        path = ForeignFile.sysCanonicalPath(path);
        return ForeignArchive.pathCondense(path);
    }

    /**
     * <p>Load the discovery.</p>
     *
     * @param base The base URI.
     * @return The discovery.
     * @throws IOException Shit happens.
     */
    public static ListArray<GestaltEntry> loadDiscoveries(String base)
            throws IOException {
        ListArray<GestaltEntry> paths = new ListArray<GestaltEntry>();
        String path = ForeignArchive.extractPath(base);
        if (path == null)
            return paths;
        File file = new File(path);
        ListArray<String> list = ForeignArchive.listDirectory(null, file);
        if (list == null)
            return paths;
        for (int i = 0; i < list.size(); i++) {
            path = list.get(i);
            if (path.endsWith("/"))
                continue;
            if (path.startsWith("."))
                continue;
            File file2 = new File(file, path);
            Attributes at = SwingGestalt.getAttributes(file2);
            String estr = (at != null ? at.getValue(GestaltEntry.ATTR_ISEXTENSION) : null);
            boolean isextension = (estr != null ? Boolean.valueOf(estr) : false);
            if (!isextension)
                continue;
            String dstr = (at != null ? at.getValue(GestaltEntry.ATTR_DONTASK) : null);
            boolean dontask = (dstr != null ? Boolean.valueOf(dstr) : true);
            path = file2.toString();
            path = ForeignFile.sysCanonicalPath(path);
            path = ForeignArchive.pathCondense(path);
            path = ForeignUri.sysUriRelative(base, path);
            GestaltEntry pse = new GestaltEntry(path, dontask);
            paths.add(pse);
        }
        return paths;
    }

    /**
     * <p>Retrieve the manifest attributes for a file.</p>
     *
     * @param file The file.
     * @return The manifest attributes.
     * @throws IOException Shit happens.
     */
    private static Attributes getAttributes(File file)
            throws IOException {
        FileInputStream in = new FileInputStream(file);
        JarInputStream jr;
        try {
            jr = new JarInputStream(in);
        } catch (IOException x) {
            in.close();
            throw x;
        }
        Manifest mf = jr.getManifest();
        jr.close();
        return (mf != null ? mf.getMainAttributes() : null);
    }

    /**
     * <p>Some testing.</p>
     * @param args Ingnored.
     */
    /*
    public static void main(String[] args) {
        ListArray<String> cp=getClassPath();
        if (cp==null)
            return;
        for (int i=0; i<cp.size(); i++)
            System.out.println(cp.get(i));
    }
     */

}
