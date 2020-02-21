package jekpro.model.rope;

import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>This class provides an index.</p>
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
final class Index {
    public final static String OP_SYS_EQ = "sys_eq";
    public final static String OP_IDENTITY = "==";
    public final static String OP_FUNCTOR = "functor";
    public final static String OP_VAR = "var";
    public final static String OP_NONVAR = "nonvar";

    static final Object VALUE_GUARD = new Object();

    Bouquet nonguard;
    Bouquet guard;
    public InterfacePairs map;

    /*********************************************************/
    /* Retrieval and Index Building                          */
    /*********************************************************/

    /**
     * <p>Compute the indexing value.</p>
     * <p>Computation is done from skeleton only.</p>
     *
     * @param at     The position.
     * @param clause The clause.
     * @return The indexing value, or null.
     */
    static Object indexValue(int at, Clause clause) {
        SkelCompound tc = (SkelCompound) clause.head;
        Object term = tc.args[at];
        if (term instanceof SkelVar) {
            return getGuard(clause.next, term);
        } else {
            return keyValue(term);
        }
    }

    /**
     * <p>Compute the key value.</p>
     *
     * @param term The term.
     * @return The key value.
     */
    static Object keyValue(Object term) {
        if (term instanceof SkelAtom) {
            return ((SkelAtom) term).fun;
        } else if (term instanceof SkelCompound) {
            return ((SkelCompound) term).sym.fun;
        } else {
            return term;
        }
    }

    /**
     * <p>Check whether the variable has a guard.</p>
     * <p>Variant of step 2.2.2 of algorithm I of:</p>
     * <p>http://user.it.uu.se/~kostis/Papers/iclp07.pdf</p>
     *
     * @param list The body of the clause.
     * @param sv   The variable.
     * @return The guard or null.
     */
    private static Object getGuard(Intermediate list, Object sv) {
        while (list instanceof Goal) {
            Object molec = ((Goal) list).term;
            if (molec instanceof SkelCompound &&
                    ((SkelCompound) molec).args.length == 2 &&
                    (((SkelCompound) molec).sym.fun.equals(Foyer.OP_EQUAL) ||
                            ((SkelCompound) molec).sym.fun.equals(Index.OP_SYS_EQ) ||
                            ((SkelCompound) molec).sym.fun.equals(Index.OP_IDENTITY))) {
                SkelCompound sc = (SkelCompound) molec;
                if (sc.args[0] == sv) {
                    Object term = sc.args[1];
                    if (term instanceof SkelVar) {
                        sv = term;
                    } else {
                        return keyValue(term);
                    }
                } else if (sc.args[1] == sv) {
                    Object term = sc.args[0];
                    if (term instanceof SkelVar) {
                        sv = term;
                    } else {
                        return keyValue(term);
                    }
                }
            } else if (molec instanceof SkelCompound &&
                    ((SkelCompound) molec).args.length == 3 &&
                    ((SkelCompound) molec).sym.fun.equals(Index.OP_FUNCTOR)) {
                SkelCompound sc = (SkelCompound) molec;
                if (sc.args[0] == sv) {
                    Object term = sc.args[1];
                    if (!(term instanceof SkelVar) && !(term instanceof SkelCompound))
                        return keyValue(term);
                }
            } else if (molec instanceof SkelCompound &&
                    ((SkelCompound) molec).args.length == 1 &&
                    ((SkelCompound) molec).sym.fun.equals(Index.OP_VAR)) {
                SkelCompound sc = (SkelCompound) molec;
                if (sc.args[0] == sv)
                    return VALUE_GUARD;
            } else if (molec instanceof SkelCompound &&
                    ((SkelCompound) molec).args.length == 1 &&
                    ((SkelCompound) molec).sym.fun.equals(Index.OP_NONVAR)) {
                /* */
            } else {
                return null;
            }
            list = list.next;
        }
        return null;
    }

    /**
     * <p>Add a clause to a new index.</p>
     * <p>The insertion strategy works as follows:</p>
     * <ul>
     * <li><b>Variable, Nonguard:</b> Insert into all hash entrys and the nonguard.</li>
     * <li><b>Variable, Guard:</b> Insert into guard.</li>
     * <li><b>Not a variable:</b> Insert into corresponding hash entry.</li>
     * </ul>
     *
     * @param clause The clause.
     * @param at     The position.
     */
    void buildIndex(Clause clause, int at) {
        Object m = Index.indexValue(at, clause);
        if (m == null) {
            if (map != null)
                map.addClause(clause);
            if (nonguard == null)
                nonguard = new Bouquet();
            nonguard.addClause(clause, AbstractDefined.OPT_ACTI_BOTT);
        } else if (m == Index.VALUE_GUARD) {
            if (guard == null)
                guard = new Bouquet();
            guard.addClause(clause, AbstractDefined.OPT_ACTI_BOTT);
        } else {
            if (map == null)
                map = new PairsArray();
            InterfacePairs res = map.addClause(m, clause, nonguard);
            if (res != null)
                map = res;
        }
    }

    /**
     * <p>Extend an existing index by a clause.</p>
     *
     * @param clause The clause.
     * @param at     The position.
     * @param flags  The flags.
     */
    void extendIndex(Clause clause, int at, int flags) {
        Object m = Index.indexValue(at, clause);
        if (m == null) {
            if (map != null)
                map.assertClause(clause, at, flags);
            if (nonguard == null)
                nonguard = new Bouquet();
            nonguard.assertClause(at + 1, clause, flags);
        } else if (m == Index.VALUE_GUARD) {
            if (guard == null)
                guard = new Bouquet();
            guard.assertClause(at + 1, clause, flags);
        } else {
            if (map == null)
                map = new PairsArray();
            InterfacePairs res = map.assertClause(m, clause, at, flags, nonguard);
            if (res != null)
                map = res;
        }
    }

    /**
     * <p>Reduce an existing index by a clause.</p>
     *
     * @param clause The clause.
     * @param at     The position.
     */
    void reduceIndex(Clause clause, int at) {
        Object m = Index.indexValue(at, clause);
        if (m == null) {
            Bouquet cp = nonguard;
            if (cp != null) {
                cp.retractClause(at + 1, clause);
                if (cp.set == null)
                    nonguard = null;
            }
            if (map == null)
                return;
            InterfacePairs res = map.retractClause(clause, at);
            if (res != null)
                map = res;
            if (map.size() == 0)
                map = null;
        } else if (m == Index.VALUE_GUARD) {
            Bouquet cp = guard;
            if (cp != null) {
                cp.retractClause(at + 1, clause);
                if (cp.set == null)
                    guard = null;
            }
        } else {
            if (map == null)
                return;
            InterfacePairs res = map.retractClause(m, clause, at);
            if (res != null)
                map = res;
            if (map.size() == 0)
                map = null;
        }
    }

    /*********************************************************/
    /* Index Inspection                                      */
    /*********************************************************/

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
    public void inspectIndex(Writer wr, int off, int start,
                             Engine en)
            throws IOException, EngineMessage, EngineException {
        if (map != null) {
            map.inspectIndex(wr, off, start, en);
        } else {
            wr.write("\n");
        }
        if (nonguard != null) {
            InterfaceRope set = nonguard.set;
            int len = (set != null ? set.getLengthScope(en) : 0);
            if (len != 0) {
                for (int i = 0; i < off; i++)
                    wr.write(" ");
                wr.write("nonguard, ");
                nonguard.inspectPaths(wr, off + 2, start, len, en);
            }
        }
        if (guard != null) {
            InterfaceRope set = guard.set;
            int len = (set != null ? set.getLengthScope(en) : 0);
            if (len != 0) {
                for (int i = 0; i < off; i++)
                    wr.write(" ");
                wr.write("guard, ");
                guard.inspectPaths(wr, off + 2, start, len, en);
            }
        }
    }

}
