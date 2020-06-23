package jekpro.tools.bundle;

import matula.util.config.AbstractDescription;
import matula.util.config.AbstractRuntime;
import matula.util.config.FileExtension;
import matula.util.wire.LangProperties;

import java.util.Locale;
import java.util.Properties;

/**
 * <p>The internal implementation of a SWI package.</p>
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
public final class DescriptionSWI extends AbstractDescription {
    public final static String MODEL_SWI = "pack";
    public final static String PLATFORM_SWI = "icon";

    /**
     * <p>Create a description SWI.</p>
     *
     * @param root The main root.
     */
    DescriptionSWI(String root) {
        setMainRoot(root);
    }

    /**
     * <p>Retrieve the bundle description.</p>
     *
     * @param locale The locale.
     * @param loader The class loader.
     * @return The properties or null.
     */
    public Properties getDescrModel(Locale locale, ClassLoader loader) {
        String name = getMainRoot() + MODEL_SWI;
        return LangProperties.getLangCheck(loader, name, locale,
                RecognizerSWI.DEFAULT, null, FileExtension.MASK_USES_TEXT);
    }

    /**
     * <p>Retrieve the bundle description.</p>
     * <p>Will be configured to display SWI-Prolog icons from here:
     * https://github.com/SWI-Prolog/plweb-www/tree/master/icons</p>
     *
     * @param locale  The locale.
     * @param loader  The class loader.
     * @param runtime The runtime.
     * @return The properties.
     */
    public Properties getDescrPlatform(Locale locale, ClassLoader loader, AbstractRuntime runtime) {
        String name = getMainRoot() + PLATFORM_SWI;
        return LangProperties.getLangCheck(loader, name, locale,
                RecognizerSWI.DEFAULT, getMainRoot(), FileExtension.MASK_USES_TEXT);
    }

}