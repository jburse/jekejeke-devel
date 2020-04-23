package jekpro.frequent.advanced;

import jekpro.model.molec.Display;
import jekpro.model.pretty.Foyer;
import jekpro.reference.structure.AbstractLexical;
import jekpro.reference.structure.LexicalCollator;
import jekpro.tools.call.CallOut;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.AbstractMap;
import matula.util.data.MapEntry;
import matula.util.regex.IgnoreCase;

/**
 * <p>Provides built-in predicates for the module aggregate.</p>
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
public final class ForeignAggregate {

    /**
     * <p>Check if the variant comparator is eager.</p>
     *
     * @param el The variant comparator.
     * @return True if the variant comparator is eager, otherwise false.
     */
    public static boolean sysVariantEager(AbstractLexical el) {
        return ((el.getFlags() & AbstractLexical.MASK_FLAG_EAGR) != 0);
    }

    /**
     * <p>Check if the variant comparator is reverse.</p>
     *
     * @param el The variant comparator.
     * @return True if the variant comparator is reverse, otherwise false.
     */
    public static boolean sysVariantReverse(AbstractLexical el) {
        return ((el.getFlags() & AbstractLexical.MASK_FLAG_RVRS) != 0);
    }

    /**
     * <p>Check if the variant comparator is natural.</p>
     *
     * @param el The variant comparator.
     * @return True if the variant comparator is natural, otherwise false.
     */
    public static boolean sysVariantNatural(AbstractLexical el) {
        return (el instanceof LexicalCollator &&
                ((LexicalCollator) el).getCmpStr() == IgnoreCase.DEFAULT_TERTIARY);
    }

}