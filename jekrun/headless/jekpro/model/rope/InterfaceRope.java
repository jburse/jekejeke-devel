package jekpro.model.rope;

import jekpro.model.inter.Engine;

/**
 * <p>This class provides an interface for clauses.</p>
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
public interface InterfaceRope {

    /**
     * <p>Retrieve size.</p>
     *
     * @return The size.
     */
    int size();

    /**
     * <p>Copy elements to an array.</p>
     *
     * @param target The array.
     */
    void toArray(Clause[] target);

    /**
     * <p>Add the clause to the list of clause.</p>
     *
     * @param clause The clause to be added.
     * @param flags  The flags
     */
    InterfaceRope addClause(Clause clause, int flags);

    /**
     * <p>Remove the clause from the list of clause.</p>
     *
     * @param rule The clause to be removed.
     */
    InterfaceRope removeClause(Clause rule);

    /**
     * <p>Build index.</p>
     *
     * @param ci The clause index.
     * @param at The position.
     */
    void buildIndex(Index ci, int at);

    /**
     * <p>Retrieve the clauses length for the given scope.</p>
     *
     * @param en The engine.
     * @return The length for the scope.
     */
    int getLengthScope(Engine en);

    /**
     * <p>Create a shallow copy.</p>
     *
     * @return The shallow copy.
     */
    Object clone();

}