package jekpro.model.rope;

import jekpro.model.inter.AbstractDefined;
import jekpro.model.pretty.Foyer;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.AbstractAssoc;
import matula.util.data.AssocArray;
import matula.util.data.MapHash;

/**
 * <p>This class provides an index with condition indexing.</p>
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
final class IndexFront extends Index {
    private static final Bouquet BOUQUETFRONT_VOID = new BouquetFront();

    public final static String OP_SYS_EQ = "sys_eq";
    public final static String OP_IDENTITY = "==";
    public final static String OP_FUNCTOR = "functor";
    public final static String OP_VAR = "var";
    public final static String OP_NONVAR = "nonvar";

    private static final Object VALUE_GUARD = new Object();

    public Bouquet guard;

    /**
     * <p>Create a bouquet for this index.</p>
     *
     * @return The new bouquet.
     */
    public Bouquet createBouquet() {
        return new BouquetFront();
    }

    /**
     * <p>Retrieve the void bouquet.</p>
     *
     * @return The void bouqet.
     */
    public Bouquet getVoid() {
        return BOUQUETFRONT_VOID;
    }

    /**
     * <p>Check whether the index can be skipped.</p>
     *
     * @return True if the index can be skipped, otherwise false.
     */
    public boolean canSkip() {
        return (map == null && guard == null);
    }

    /**
     * <p>Retrieve the guard,</p>
     *
     * @return The guard.
     */
    public Bouquet getGuard() {
        return guard;
    }

    /**
     * <p>Compute the indexing value.</p>
     * <p>Computation is done from skeleton only.</p>
     *
     * @param at     The position.
     * @param clause The clause.
     * @return The indexing value, or null.
     */
    private static Object indexValueFront(int at, Clause clause) {
        SkelCompound tc = (SkelCompound) clause.head;
        Object term = tc.args[at];
        if (!(term instanceof SkelVar))
            return Index.keyValue(term);
        return getGuard(clause.next, term);
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
                            ((SkelCompound) molec).sym.fun.equals(OP_SYS_EQ) ||
                            ((SkelCompound) molec).sym.fun.equals(OP_IDENTITY))) {
                SkelCompound sc = (SkelCompound) molec;
                if (sc.args[0] == sv) {
                    Object term = sc.args[1];
                    if (term instanceof SkelVar) {
                        sv = term;
                    } else {
                        return Index.keyValue(term);
                    }
                } else if (sc.args[1] == sv) {
                    Object term = sc.args[0];
                    if (term instanceof SkelVar) {
                        sv = term;
                    } else {
                        return Index.keyValue(term);
                    }
                }
            } else if (molec instanceof SkelCompound &&
                    ((SkelCompound) molec).args.length == 3 &&
                    ((SkelCompound) molec).sym.fun.equals(OP_FUNCTOR)) {
                SkelCompound sc = (SkelCompound) molec;
                if (sc.args[0] == sv) {
                    Object term = sc.args[1];
                    if (!(term instanceof SkelVar) && !(term instanceof SkelCompound))
                        return Index.keyValue(term);
                }
            } else if (molec instanceof SkelCompound &&
                    ((SkelCompound) molec).args.length == 1 &&
                    ((SkelCompound) molec).sym.fun.equals(OP_VAR)) {
                SkelCompound sc = (SkelCompound) molec;
                if (sc.args[0] == sv)
                    return VALUE_GUARD;
            } else if (molec instanceof SkelCompound &&
                    ((SkelCompound) molec).args.length == 1 &&
                    ((SkelCompound) molec).sym.fun.equals(OP_NONVAR)) {
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
     *
     * @param clause The clause.
     * @param at     The position.
     */
    void buildIndex(Clause clause, int at) {
        Object m = indexValueFront(at, clause);
        if (m == null) {
            AbstractAssoc<Object, Bouquet> temp = map;
            if (temp != null)
                Index.addClause(temp, clause);
            if (nonguard == null)
                nonguard = new BouquetFront();
            nonguard.addClause(clause, AbstractDefined.OPT_ACTI_BOTT);
        } else if (m == VALUE_GUARD) {
            if (guard == null)
                guard = new BouquetFront();
            guard.addClause(clause, AbstractDefined.OPT_ACTI_BOTT);
        } else {
            AbstractAssoc<Object, Bouquet> temp = map;
            if (temp == null) {
                temp = new AssocArray<>();
                map = temp;
            }
            addClause(temp, m, clause, nonguard);
            if (temp instanceof AssocArray && temp.size() > Index.MAX_ASSOC_SMALL) {
                MapHash<Object, Bouquet> res = new MapHash<>(temp.size());
                temp.toAssoc(res);
                map = res;
            }
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
        Object m = indexValueFront(at, clause);
        if (m == null) {
            AbstractAssoc<Object, Bouquet> temp = map;
            if (temp != null)
                Index.assertClause(temp, clause, at + 1, flags);
            if (nonguard == null)
                nonguard = new BouquetFront();
            nonguard.assertClause(at + 1, clause, flags);
        } else if (m == VALUE_GUARD) {
            if (guard == null)
                guard = new BouquetFront();
            guard.assertClause(at + 1, clause, flags);
        } else {
            AbstractAssoc<Object, Bouquet> temp = map;
            if (temp == null) {
                temp = new AssocArray<>();
                map = temp;
            }
            assertClause(temp, m, clause, at + 1, flags, nonguard);
            if (temp instanceof AssocArray && temp.size() > Index.MAX_ASSOC_SMALL) {
                MapHash<Object, Bouquet> res = new MapHash<>(temp.size());
                temp.toAssoc(res);
                map = res;
            }
        }
    }

    /**
     * <p>Reduce an existing index by a clause.</p>
     *
     * @param clause The clause.
     * @param at     The position.
     */
    void reduceIndex(Clause clause, int at) {
        Object m = indexValueFront(at, clause);
        if (m == null) {
            Bouquet cp = nonguard;
            if (cp != null) {
                cp.retractClause(at + 1, clause);
                if (cp.set == null)
                    nonguard = null;
            }
            AbstractAssoc<Object, Bouquet> temp = map;
            if (temp == null || !Index.retractClause(temp, clause, at + 1))
                return;
            if (temp instanceof MapHash && temp.size() < Index.MIN_ASSOC_LARGE) {
                AssocArray<Object, Bouquet> res = new AssocArray<>(temp.size());
                temp.toAssoc(res);
                map = res;
            } else if (temp.size() == 0) {
                map = null;
            }
        } else if (m == VALUE_GUARD) {
            Bouquet cp = guard;
            if (cp != null) {
                cp.retractClause(at + 1, clause);
                if (cp.set == null)
                    guard = null;
            }
        } else {
            AbstractAssoc<Object, Bouquet> temp = map;
            if (temp == null || !Index.retractClause(temp, m, clause, at + 1))
                return;
            if (temp instanceof MapHash && temp.size() < Index.MIN_ASSOC_LARGE) {
                AssocArray<Object, Bouquet> res = new AssocArray<>(temp.size());
                temp.toAssoc(res);
                map = res;
            } else if (temp.size() == 0) {
                map = null;
            }
        }
    }

}