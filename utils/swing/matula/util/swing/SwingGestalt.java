package matula.util.swing;

import matula.util.config.ForeignArchive;
import matula.util.config.GestaltEntry;
import matula.util.data.ListArray;

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
    private static final String DIRECTORY_APK = "apk";

    /**
     * <p>Retrieve the base.</p>
     *
     * @return The base.
     */
    public static String getBase() {
        File userdir = new File(System.getProperty("user.dir"));
        return userdir.toURI().toString();
    }

    /**
     * <p>Load the discovery.</p>
     *
     * @param base The base.
     * @return The discovery.
     * @throws IOException Shit happens.
     */
    public static ListArray<GestaltEntry> loadDiscoveries(String base)
            throws IOException {
        ListArray<GestaltEntry> paths = new ListArray<GestaltEntry>();
        base = ForeignArchive.extractPath(base);
        if (base == null)
            return paths;
        File file = new File(base, SwingGestalt.DIRECTORY_APK);
        ListArray<String> list = ForeignArchive.listDirectory(null, file);
        if (list == null)
            return paths;
        for (int i = 0; i < list.size(); i++) {
            base = list.get(i);
            if (base.endsWith("/"))
                continue;
            File file2 = new File(file, base);
            Attributes at = SwingGestalt.getAttributes(file2);
            String dstr = (at != null ? at.getValue("dontask") : null);
            boolean dontask = (dstr != null ? Boolean.valueOf(dstr) : true);
            String path = SwingGestalt.DIRECTORY_APK + "/" + base;
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

}
