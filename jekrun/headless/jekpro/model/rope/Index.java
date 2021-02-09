package jekpro.model.rope;

import jekpro.model.inter.AbstractDefined;
import jekpro.model.pretty.Foyer;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.AbstractAssoc;
import matula.util.data.AssocArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;

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
public final class Index {
    public final static String OP_SYS_EQ = "sys_eq";
    public final static String OP_IDENTITY = "==";
    public final static String OP_FUNCTOR = "functor";
    public final static String OP_VAR = "var";
    public final static String OP_NONVAR = "nonvar";

    static final Object VALUE_GUARD = new Object();

    public static final int MAX_SMALL = 6;
    public static final int MIN_LARGE = 3;

    public Bouquet nonguard;
    public Bouquet guard;
    public AbstractAssoc<Object, Bouquet> map;

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
                Index.addClause(map, clause);
            if (nonguard == null)
                nonguard = new Bouquet();
            nonguard.addClause(clause, AbstractDefined.OPT_ACTI_BOTT);
        } else if (m == Index.VALUE_GUARD) {
            if (guard == null)
                guard = new Bouquet();
            guard.addClause(clause, AbstractDefined.OPT_ACTI_BOTT);
        } else {
            if (map == null)
                map = new AssocArray<>();
            Index.addClause(map, m, clause, nonguard);
            if (map instanceof AssocArray && map.size() > Index.MAX_SMALL)
                map = Index.toLarge((AssocArray) map);
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
                Index.assertClause(map, clause, at + 1, flags);
            if (nonguard == null)
                nonguard = new Bouquet();
            nonguard.assertClause(at + 1, clause, flags);
        } else if (m == Index.VALUE_GUARD) {
            if (guard == null)
                guard = new Bouquet();
            guard.assertClause(at + 1, clause, flags);
        } else {
            if (map == null)
                map = new AssocArray<>();
            Index.assertClause(map, m, clause, at + 1, flags, nonguard);
            if (map instanceof AssocArray && map.size() > Index.MAX_SMALL)
                map = Index.toLarge((AssocArray) map);
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
            Index.retractClause(map, clause, at + 1);
            if (map instanceof MapHash && map.size() < Index.MIN_LARGE) {
                map = toSmall((MapHash) map);
            } else if (map.size() == 0) {
                map = null;
            }
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
            if (!Index.retractClause(map, m, clause, at + 1))
                return;
            if (map instanceof MapHash && map.size() < Index.MIN_LARGE) {
                map = toSmall((MapHash) map);
            } else if (map.size() == 0) {
                map = null;
            }
        }
    }

    /*********************************************************/
    /* Pairs Polymorphism                                    */
    /*********************************************************/

    /**
     * <p>Add the clause to the list of pairs.</p>
     *
     * @param pairs  The pairs.
     * @param clause The clause to be added.
     */
    private static void addClause(AbstractAssoc<Object, Bouquet> pairs, Clause clause) {
        if (pairs instanceof AssocArray) {
            AssocArray<Object, Bouquet> list = (AssocArray) pairs;
            for (int j = 0; j < list.size(); j++) {
                Bouquet cp = list.getValue(j);
                cp.addClause(clause, AbstractDefined.OPT_ACTI_BOTT);
            }
        } else {
            MapHash<Object, Bouquet> hash = (MapHash) pairs;
            for (MapEntry<Object, Bouquet> entry = hash.getFirstEntry();
                 entry != null; entry = hash.successor(entry)) {
                Bouquet cp = entry.value;
                cp.addClause(clause, AbstractDefined.OPT_ACTI_BOTT);
            }
        }
    }

    /**
     * <p>Add the clause to the list of pairs.</p>
     *
     * @param pairs  The pairs.
     * @param m      The key.
     * @param clause The clause to be added.
     * @param b      The nonguard.
     */
    private static void addClause(AbstractAssoc<Object, Bouquet> pairs,
                                  Object m, Clause clause, Bouquet b) {
        Bouquet cp = pairs.get(m);
        if (cp == null) {
            if (b != null) {
                cp = b.copyBouquet();
            } else {
                cp = new Bouquet();
            }
            pairs.add(m, cp);
        }
        cp.addClause(clause, AbstractDefined.OPT_ACTI_BOTT);
    }

    /**
     * <p>Create a large map from this small map.</p>
     * <p>Carry over the bouquets.</p>
     *
     * @param pairs The pairs.
     * @return The large map.
     */
    private static MapHash<Object, Bouquet> toLarge(AssocArray<Object, Bouquet> pairs) {
        MapHash<Object, Bouquet> res = new MapHash<>(pairs.size());
        for (int j = 0; j < pairs.size(); j++) {
            Object m = pairs.getKey(j);
            Bouquet cp = pairs.getValue(j);
            res.add(m, cp);
        }
        return res;
    }

    /**
     * <p>Assert the clause to the list of pairs.</p>
     *
     * @param pairs  The pairs.
     * @param clause The clause to be added.
     * @param at     The position.
     * @param flags  The flags.
     */
    private static void assertClause(AbstractAssoc<Object, Bouquet> pairs,
                                     Clause clause, int at, int flags) {
        if (pairs instanceof AssocArray) {
            AssocArray<Object, Bouquet> list = (AssocArray) pairs;
            for (int j = 0; j < list.size(); j++) {
                Bouquet cp = list.getValue(j);
                cp.assertClause(at, clause, flags);
            }
        } else {
            MapHash<Object, Bouquet> hash = (MapHash) pairs;
            for (MapEntry<Object, Bouquet> entry = hash.getFirstEntry();
                 entry != null; entry = hash.successor(entry)) {
                Bouquet cp = entry.value;
                cp.assertClause(at, clause, flags);
            }
        }
    }

    /**
     * <p>Assert the clause to the list of pairs.</p>
     *
     * @param pairs  The pairs.
     * @param m      The key.
     * @param clause The clause to be added.
     * @param at     The position.
     * @param flags  The flags.
     * @param b      The nonguard.
     */
    public static void assertClause(AbstractAssoc<Object, Bouquet> pairs,
                                    Object m, Clause clause,
                                    int at, int flags, Bouquet b) {
        Bouquet cp = pairs.get(m);
        if (cp == null) {
            if (b != null) {
                cp = b.copyBouquet();
            } else {
                cp = new Bouquet();
            }
            pairs.add(m, cp);
        }
        cp.assertClause(at, clause, flags);
    }

    /**
     * <p>Retract the clause to the list of pairs.</p>
     *
     * @param pairs  The pairs.
     * @param clause The clause to be added.
     * @param at     The position.
     */
    private static void retractClause(AbstractAssoc<Object, Bouquet> pairs,
                                      Clause clause, int at) {
        if (pairs instanceof AssocArray) {
            AssocArray<Object, Bouquet> list = (AssocArray) pairs;
            boolean dirty = false;
            for (int j = list.size() - 1; j >= 0; j--) {
                Bouquet cp = list.getValue(j);
                cp.retractClause(at, clause);
                if (cp.set == null) {
                    list.removeEntry(j);
                    dirty = true;
                }
            }
            if (dirty)
                list.resize();
        } else {
            MapHash<Object, Bouquet> hash = (MapHash) pairs;
            boolean dirty = false;
            for (MapEntry<Object, Bouquet> entry = hash.getFirstEntry();
                 entry != null; entry = hash.successor(entry)) {
                Bouquet cp = entry.value;
                cp.retractClause(at, clause);
                if (cp.set == null) {
                    hash.removeEntry(entry);
                    dirty = true;
                }
            }
            if (dirty)
                hash.resize();
        }
    }

    /**
     * <p>Create a small map from this large map.</p>
     * <p>Carry over the bouquets.</p>
     *
     * @param pairs The pairs.
     * @return The small map.
     */
    private AssocArray<Object, Bouquet> toSmall(MapHash<Object, Bouquet> pairs) {
        AssocArray<Object, Bouquet> res = new AssocArray<>(pairs.size());
        for (MapEntry<Object, Bouquet> entry = pairs.getFirstEntry();
             entry != null; entry = pairs.successor(entry)) {
            Object m = entry.key;
            Bouquet cp = entry.value;
            res.add(m, cp);
        }
        return res;
    }

    /**
     * <p>Retract the clause to the list of pairs.</p>
     *
     * @param pairs  The pairs.
     * @param m      The key.
     * @param clause The clause to be added.
     * @param at     The position.
     */
    private static boolean retractClause(AbstractAssoc<Object, Bouquet> pairs,
                                      Object m, Clause clause, int at) {
        if (pairs instanceof AssocArray) {
            AssocArray<Object, Bouquet> list = (AssocArray) pairs;
            int j = list.indexOf(m);
            if (j < 0)
                return false;
            Bouquet cp = list.getValue(j);
            cp.retractClause(at, clause);
            if (cp.set == null) {
                list.removeEntry(j);
                list.resize();
            }
            return true;
        } else {
            MapHash<Object, Bouquet> hash = (MapHash) pairs;
            MapEntry<Object, Bouquet> entry = hash.getEntry(m);
            if (entry == null)
                return false;
            Bouquet cp = entry.value;
            cp.retractClause(at, clause);
            if (cp.set == null) {
                hash.removeEntry(entry);
                hash.resize();
            }
            return true;
        }
    }

}
