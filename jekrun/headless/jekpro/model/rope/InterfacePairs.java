package jekpro.model.rope;

import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>This class provides an interface for pairs.</p>
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
public interface InterfacePairs {

    /**
     * <p>Retrieve size.</p>
     *
     * @return The size.
     */
    int size();

    /**
     * <p>Find the key in the map.</p>
     *
     * @param key The key.
     * @return The value, or null.
     */
    Bouquet get(Object key);

    /**
     * <p>Add the clause to the list of pairs.</p>
     *
     * @param clause The clause to be added.
     */
    void addClause(Clause clause);

    /**
     * <p>Add the clause to the list of pairs.</p>
     *
     * @param m      The key.
     * @param clause The clause to be added.
     * @param b      The nonguard.
     */
    InterfacePairs addClause(Object m, Clause clause, Bouquet b);

    /**
     * <p>Assert the clause to the list of pairs.</p>
     *
     * @param clause The clause to be added.
     * @param at     The position.
     * @param flags  The flags.
     */
    void assertClause(Clause clause, int at, int flags);

    /**
     * <p>Assert the clause to the list of pairs.</p>
     *
     * @param m      The key.
     * @param clause The clause to be added.
     * @param at     The position.
     * @param flags  The flags.
     * @param b      The nonguard.
     * @return The new interface pairs, or null.
     */
    InterfacePairs assertClause(Object m, Clause clause, int at, int flags, Bouquet b);

    /**
     * <p>Retract the clause to the list of pairs.</p>
     *
     * @param clause The clause to be added.
     * @param at     The position.
     * @return The new interface pairs, or null.
     */
    InterfacePairs retractClause(Clause clause, int at);

    /**
     * <p>Retract the clause to the list of pairs.</p>
     *
     * @param m      The key.
     * @param clause The clause to be added.
     * @param at     The position.
     */
    InterfacePairs retractClause(Object m, Clause clause, int at);

    /**
     * <p>Dump the index.</p>
     *
     * @param wr    The writer.
     * @param off   The left indentation.
     * @param start The start position.
     * @param en    The engine copy.
     * @throws IOException     IO error.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    void inspectIndex(Writer wr, int off, int start,
                      Engine en)
            throws IOException, EngineMessage, EngineException;

}