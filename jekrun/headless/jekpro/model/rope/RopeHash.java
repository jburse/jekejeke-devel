package jekpro.model.rope;

import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.tools.term.SkelAtom;
import matula.util.data.SetEntry;
import matula.util.data.SetHashLink;

/**
 * <p>This class provides a hash clauses implementation.</p>
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
public final class RopeHash
        extends SetHashLink<Clause>
        implements InterfaceRope {
    private static final int MIN_LARGE = 6;

    /**
     * <p>Create a clauses hash.</p>
     */
    public RopeHash() {
        super();
    }

    /**
     * <p>Create a clauses hash.</p>
     *
     * @param capa The ahead capacity.
     */
    public RopeHash(int capa) {
        super(capa);
    }

    /**
     * <p>Add the clause to the list of clause.</p>
     *
     * @param clause The clause to be added.
     * @param flags  The flags
     */
    public InterfaceRope addClause(Clause clause, int flags) {
        if ((flags & AbstractDefined.OPT_ACTI_BOTT) != 0) {
            add(clause);
        } else {
            addFirst(clause);
        }
        return null;
    }

    /**
     * <p>Remove the clause from the list of clause.</p>
     *
     * @param clause The clause to be removed.
     */
    public InterfaceRope removeClause(Clause clause) {
        remove(clause);
        if (size() < MIN_LARGE)
            return toSmall();
        return null;
    }

    /**
     * <p>Create a small set from this large set.</p>
     * <p>Carry over the clauses.</p>
     *
     * @return The small set.
     */
    private InterfaceRope toSmall() {
        RopeArray res = new RopeArray(size());
        for (SetEntry<Clause> entry = getFirstEntry();
             entry != null; entry = successor(entry))
            res.add(entry.value);
        return res;
    }

    /**
     * <p>Build an index.</p>
     *
     * @param ci The clause index.
     * @param at The position.
     */
    public void buildIndex(Index ci, int at) {
        for (SetEntry<Clause> entry = getFirstEntry();
             entry != null; entry = successor(entry))
            ci.buildIndex(entry.value, at);
    }

    /**
     * <p>Retrieve the clauses length for the given scope.</p>
     *
     * @param en The engine.
     * @return The length for the scope.
     */
    public int getLengthScope(Engine en) {
        int len = 0;
        for (SetEntry<Clause> entry = getFirstEntry();
             entry != null; entry = successor(entry)) {
            SkelAtom sa = StackElement.callableToName(entry.value.head);
            if (Clause.ancestorSource(sa.scope, en))
                len++;
        }
        return len;
    }

}