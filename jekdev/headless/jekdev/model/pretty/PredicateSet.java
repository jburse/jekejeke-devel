package jekdev.model.pretty;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.inter.Predicate;
import matula.util.data.ListArray;

/**
 * <p>This class provides a unsynchronized predicate set.</p>
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
 * Only to be distributed with programs that add sgnificant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class PredicateSet extends ListArray<Predicate> {
    private Predicate[] cachepreds;

    /**
     * <p>Add a predicate.</p>
     *
     * @param pick The predicate.
     * @return True if cache was cleared, otherwise false.
     */
    public boolean addPredicate(Predicate pick) {
        if (contains(pick))
            return false;
        add(pick);
        cachepreds = null;
        return true;
    }

    /**
     * <p>Retrieve all predicates.</p>
     *
     * @return The predicates.
     */
    public Predicate[] allPredicates() {
        Predicate[] res = cachepreds;
        if (res != null)
            return res;
        if (size() != 0) {
            res = new Predicate[size()];
            toArray(res);
        } else {
            res = AbstractBranch.FALSE_PREDS;
        }
        cachepreds = res;
        return res;
    }

}