package matula.util.config;

import derek.util.protect.LicenseError;
import matula.comp.sharik.Enforced;
import matula.comp.text.DefaultRecognizer;
import matula.util.data.ListArray;
import matula.util.regex.ScannerError;
import matula.util.system.ForeignCache;

import java.io.IOException;
import java.net.URL;
import java.util.Enumeration;
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

    /*************************************************************/
    /* Directory Property                                        */
    /*************************************************************/

    /**
     * <p>Discover teh directory properties.</p>
     *
     * @param e The enforced.
     */
    public static ListArray<AirDropEntry> load(Enforced e)
            throws IOException, LicenseError, ScannerError {
        ClassLoader loader = e.getRoot().getLoader();
        ListArray<AirDropEntry> slips = null;

        Enumeration<URL> urls = loader.getResources("jekpub.platform.apk.propertiesx");
        while (urls.hasMoreElements()) {
            URL url = urls.nextElement();
            Properties prop = new Properties();
            ForeignCache.loadBinary(DefaultRecognizer.DEFAULT, url.toString(), prop);
            if (slips == null)
                slips = new ListArray<AirDropEntry>();
            AirDrop.loadEntries(slips, prop);
        }

        return slips;
    }

    /**
     * <p>Digest the directory property.</p>
     *
     * @param slips The entries.
     * @param prop The properties.
     */
    private static void loadEntries(ListArray<AirDropEntry> slips, Properties prop) {
        String countstr = prop.getProperty("slip.count");
        if (countstr == null)
            return;
        int count = Integer.parseInt(countstr);
        for (int i = 0; i < count; i++) {
            String cstr = prop.getProperty("slip." + i + ".capa");
            String dstr = prop.getProperty("slip." + i + ".dontask");
            if (cstr != null || dstr != null) {
                String capa = (cstr != null ? cstr : "");
                boolean dontask = "true".equals(dstr);
                AirDropEntry pse = new AirDropEntry(capa, dontask);
                slips.add(pse);
            }
        }
    }

    /*************************************************************/
    /* Known Paths                                               */
    /*************************************************************/

}
