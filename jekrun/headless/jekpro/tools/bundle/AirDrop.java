package jekpro.tools.bundle;

import derek.util.protect.LicenseError;
import jekpro.model.pretty.LookupBase;
import matula.comp.sharik.Enforced;
import matula.comp.text.DefaultRecognizer;
import matula.util.config.AbstractBundle;
import matula.util.config.FileExtension;
import matula.util.config.ForeignArchive;
import matula.util.data.ListArray;
import matula.util.wire.LangProperties;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;
import java.util.Properties;

/**
 * <p>This class provides discovery of capabilities.</p>
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
public final class AirDrop {
    public final static String MODEL_DEFAULT = "model/builtin/description";
    public final static String MODEL_SWI = "pack";

    public final static String PLATFORM_DIR = "platform/";
    public final static String PLATFORM_FILE = "/description";

    /*************************************************************/
    /* Known Paths                                               */
    /*************************************************************/

    /**
     * <p>Discover capabilities from know paths.</p>
     *
     * @param e   The enforced.
     * @param cps The class paths.
     * @return The discovered capabilities.
     */
    public static ListArray<AirDropEntry> load(Enforced e, ListArray<String> cps)
            throws IOException {
        ListArray<String> roots = findRoots(e, cps);
        if (roots == null)
            return null;
        return findKnown(e, roots);
    }

    /**
     * <p>Find the candidate roots of the class path.</p>
     *
     * @param e   The enforced.
     * @param cps The class paths.
     * @return The condidate roots or null.
     * @throws IOException Shit happens.
     */
    public static ListArray<String> findRoots(Enforced e, ListArray<String> cps)
            throws IOException {
        ListArray<String> roots = null;
        for (int i = 0; i < cps.size(); i++) {
            String path = cps.get(i);
            if (path.startsWith("#"))
                continue;
            path = LookupBase.findWrite(path, e.getRoot());
            path = ForeignArchive.extractPath(path);
            if (path == null)
                continue;
            File f = new File(path);
            if (!f.exists())
                continue;
            if (path.endsWith("/")) {
                roots = ForeignArchive.listDirectory(roots, f);
            } else {
                InputStream in = new FileInputStream(f);
                roots = ForeignArchive.listArchive(roots, in, "");
            }
        }
        return roots;
    }

    /**
     * <p>Find the known paths from the candidate roots.</p></p>
     *
     * @param e     The enforced.
     * @param roots The candidate roots.
     */
    public static ListArray<AirDropEntry> findKnown(Enforced e, ListArray<String> roots) {
        ClassLoader loader = e.getRoot().getLoader();
        ListArray<AirDropEntry> slips = null;

        for (int i = 0; i < roots.size(); i++) {
            String root = roots.get(i);

            String name = root + MODEL_DEFAULT;
            Properties prop = LangProperties.getLangCheck(loader, name,
                    Locale.getDefault(), DefaultRecognizer.DEFAULT, FileExtension.MASK_USES_RSCS);
            if (prop != null) {
                if (slips == null)
                    slips = new ListArray<AirDropEntry>();
                String capa = prop.getProperty(AbstractBundle.PROP_SLIP_CAPA);
                boolean dontask = "true".equals(prop.getProperty(AbstractBundle.PROP_SLIP_DONTASK));
                slips.add(new AirDropEntry(capa, dontask));
            }

            name = root + MODEL_SWI;
            String adr = LangProperties.getURL(loader, name,
                    RecognizerSWI.DEFAULT, FileExtension.MASK_USES_TEXT);
            if (adr != null) {
                if (slips == null)
                    slips = new ListArray<AirDropEntry>();
                String capa = CapabilitySWI.class.getName() + "(" + root + ")";
                slips.add(new AirDropEntry(capa, true));
            }
        }

        return slips;
    }

    /*************************************************************/
    /* Load Utilties                                             */
    /*************************************************************/

    /**
     * <p>Init a list of paths.</p>
     *
     * @param e   The enforced.
     * @param cps The class paths.
     */
    public static void initPaths(Enforced e, ListArray<String> cps)
            throws IOException, LicenseError {
        for (int i = 0; i < cps.size(); i++) {
            String path = cps.get(i);
            if (path.startsWith("#"))
                continue;
            path = LookupBase.findWrite(path, e.getRoot());
            e.getRoot().addClassPath(path);
        }
    }

}
