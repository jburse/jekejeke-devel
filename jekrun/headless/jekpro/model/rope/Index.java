package jekpro.model.rope;

import jekpro.model.inter.AbstractDefined;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.*;

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
public class Index {
    private static final Bouquet BOUQUET_VOID = new Bouquet();

    public static final int MIN_ASSOC_LARGE = 3;
    public static final int MAX_ASSOC_SMALL = 6;

    public Bouquet nonguard;
    public AbstractAssoc<Object, Bouquet> map;

    /**
     * <p>Create a bouquet for this index.</p>
     *
     * @return The new bouquet.
     */
    public Bouquet createBouquet() {
        return new Bouquet();
    }

    /**
     * <p>Retrieve the void bouquet.</p>
     *
     * @return The void bouqet.
     */
    public Bouquet getVoid() {
        return BOUQUET_VOID;
    }

    /**
     * <p>Check whether the index can be skipped.</p>
     *
     * @return True if the index can be skipped, otherwise false.
     */
    public boolean canSkip() {
        return (map == null);
    }

    /**
     * <p>Retrieve the guard,</p>
     *
     * @return The guard.
     */
    public Bouquet getGuard() {
        return null;
    }

    /*********************************************************/
    /* Retrieval and Index Building                          */
    /*********************************************************/

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
     * <p>Compute the indexing value.</p>
     * <p>Computation is done from skeleton only.</p>
     *
     * @param at     The position.
     * @param clause The clause.
     * @return The indexing value, or null.
     */
    private static Object indexValue(int at, Clause clause) {
        SkelCompound tc = (SkelCompound) clause.head;
        Object term = tc.args[at];
        if (!(term instanceof SkelVar))
            return keyValue(term);
        return null;
    }

    /**
     * <p>Add a clause to a new index.</p>
     *
     * @param clause The clause.
     * @param at     The position.
     */
    void buildIndex(Clause clause, int at) {
        Object m = Index.indexValue(at, clause);
        if (m == null) {
            AbstractAssoc<Object, Bouquet> temp = map;
            if (temp != null)
                Index.addClause(temp, clause);
            if (nonguard == null)
                nonguard = new Bouquet();
            nonguard.addClause(clause, AbstractDefined.OPT_ACTI_BOTT);
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
        Object m = Index.indexValue(at, clause);
        if (m == null) {
            AbstractAssoc<Object, Bouquet> temp = map;
            if (temp != null)
                Index.assertClause(temp, clause, at + 1, flags);
            if (nonguard == null)
                nonguard = new Bouquet();
            nonguard.assertClause(at + 1, clause, flags);
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
        Object m = Index.indexValue(at, clause);
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

    /*********************************************************/
    /* Pairs Polymorphism                                    */
    /*********************************************************/

    /**
     * <p>Copy a bouquet without its index.</p>
     *
     * @return The copy of this bouquet,
     */
    private Bouquet copyBouquet(Bouquet bc) {
        Bouquet res = createBouquet();
        if (bc == null)
            return res;
        AbstractList<Clause> temp = bc.set;
        if (temp != null)
            res.set = (AbstractList<Clause>) temp.clone();
        res.cache = bc.cache;
        return res;
    }

    /**
     * <p>Add the clause to the list of pairs.</p>
     *
     * @param pairs  The pairs.
     * @param m      The key.
     * @param clause The clause to be added.
     * @param b      The nonguard.
     */
    void addClause(AbstractAssoc<Object, Bouquet> pairs,
                   Object m, Clause clause, Bouquet b) {
        Bouquet cp = pairs.get(m);
        if (cp == null) {
            cp = copyBouquet(b);
            pairs.add(m, cp);
        }
        cp.addClause(clause, AbstractDefined.OPT_ACTI_BOTT);
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
    void assertClause(AbstractAssoc<Object, Bouquet> pairs,
                      Object m, Clause clause,
                      int at, int flags, Bouquet b) {
        Bouquet cp = pairs.get(m);
        if (cp == null) {
            cp = copyBouquet(b);
            pairs.add(m, cp);
        }
        cp.assertClause(at, clause, flags);
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
    protected static void addClause(AbstractAssoc<Object, Bouquet> pairs, Clause clause) {
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
     * <p>Retract the clause to the list of pairs.</p>
     *
     * @param pairs  The pairs.
     * @param m      The key.
     * @param clause The clause to be added.
     * @param at     The position.
     * @return True if dirty, otherwise false.
     */
    protected static boolean retractClause(AbstractAssoc<Object, Bouquet> pairs,
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

    /**
     * <p>Assert the clause to the list of pairs.</p>
     *
     * @param pairs  The pairs.
     * @param clause The clause to be added.
     * @param at     The position.
     * @param flags  The flags.
     */
    protected static void assertClause(AbstractAssoc<Object, Bouquet> pairs,
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
     * <p>Retract the clause to the list of pairs.</p>
     *
     * @param pairs  The pairs.
     * @param clause The clause to be added.
     * @param at     The position.
     * @return True if dirty, otherwise false.
     */
    protected static boolean retractClause(AbstractAssoc<Object, Bouquet> pairs,
                                 Clause clause, int at) {
        boolean dirty = false;
        if (pairs instanceof AssocArray) {
            AssocArray<Object, Bouquet> list = (AssocArray) pairs;
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
        return dirty;
    }

}
