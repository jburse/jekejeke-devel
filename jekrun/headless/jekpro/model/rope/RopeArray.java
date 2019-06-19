package jekpro.model.rope;

import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.tools.term.SkelAtom;
import matula.util.data.ListArray;

/**
 * <p>This class provides an array clauses implementation.</p>
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
public final class RopeArray
        extends ListArray<Clause>
        implements InterfaceRope {
    private static final int MAX_SMALL = 12;

    /**
     * <p>Create a clauses array.</p>
     */
    public RopeArray() {
        super();
    }

    /**
     * <p>Create a clauses array.</p>
     *
     * @param capa The ahead capacity.
     */
    public RopeArray(int capa) {
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
        if (size() > MAX_SMALL)
            return toLarge();
        return null;
    }

    /**
     * <p>Create a large set from this small set.</p>
     * <p>Carry over the clauses.</p>
     *
     * @return The large set.
     */
    private InterfaceRope toLarge() {
        RopeHash res = new RopeHash(size());
        for (int i = 0; i < size(); i++)
            res.add(get(i));
        return res;
    }

    /**
     * <p>Remove the clause from the list of clause.</p>
     *
     * @param clause The clause to be removed.
     */
    public InterfaceRope removeClause(Clause clause) {
        remove(clause);
        return null;
    }

    /**
     * <p>Build an index.</p>
     *
     * @param ci The clause index.
     * @param at The position.
     */
    public void buildIndex(Index ci, int at) {
        for (int i = 0; i < size(); i++)
            ci.buildIndex(get(i), at);
    }

    /**
     * <p>Retrieve the clauses length for the given scope.</p>
     *
     * @param en The engine.
     * @return The length for the scope.
     */
    public int getLengthScope(Engine en) {
        int len = 0;
        for (int i = 0; i < size(); i++) {
            SkelAtom sa = StackElement.callableToName(get(i).head);
            if (Clause.ancestorSource(sa.scope, en))
                len++;
        }
        return len;
    }

}