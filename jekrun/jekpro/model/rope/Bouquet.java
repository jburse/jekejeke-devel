package jekpro.model.rope;

import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.*;

/**
 * <p>This class provides a bouquet.</p>
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
public class Bouquet {
    private static final Clause[] ARRAY_VOID = new Clause[0];

    private static final int MIN_LIST_LARGE = 6;
    private static final int MAX_LIST_SMALL = 12;

    public AbstractList<Clause> set;
    public Index[] args;
    public Clause[] cache;

    /**
     * <p>Create an index for this bouquet.</p>
     *
     * @return The new index.
     */
    public Index createIndex() {
        return new Index();
    }

    /**
     * <p>Add the clause to the list of clause.</p>
     *
     * @param clause The clause to be added.
     * @param flags  The flags
     */
    public void addClause(Clause clause, int flags) {
        AbstractList<Clause> temp = set;
        if (temp == null) {
            temp = new ListArray<>();
            set = temp;
        }
        if ((flags & AbstractDefined.OPT_ACTI_BOTT) != 0) {
            temp.add(clause);
        } else {
            temp.addFirst(clause);
        }
        if (temp instanceof ListArray && temp.size() > MAX_LIST_SMALL) {
            SetHashLink<Clause> res = new SetHashLink<>(temp.size());
            temp.toList(res);
            set = res;
        }
        cache = null;
    }

    /**
     * <p>Add the clause to the clause paths.</p>
     *
     * @param start  The start position.
     * @param clause The clause.
     * @param flags  The flags.
     */
    public void assertClause(int start, Clause clause, int flags) {
        addClause(clause, flags);

        Index[] help = args;
        if (help == null)
            return;
        for (int i = 0; i < help.length; i++) {
            Index ci = help[i];
            if (ci == null)
                continue;
            ci.extendIndex(clause, start + i, flags);
        }
    }

    /**
     * <p>Remove the clause from the clause paths.</p>
     *
     * @param start  The start position.
     * @param clause The clause to retract.
     */
    public void retractClause(int start, Clause clause) {
        AbstractList<Clause> temp = set;
        temp.remove(clause);
        if (temp.size() == 0) {
            set = null;
        } else if (temp instanceof SetHashLink && temp.size() < MIN_LIST_LARGE) {
            ListArray<Clause> res = new ListArray<>(temp.size());
            temp.toList(res);
            set = res;
        }
        cache = null;

        Index[] help = args;
        if (help == null)
            return;
        for (int i = 0; i < help.length; i++) {
            Index ci = help[i];
            if (ci == null)
                continue;
            ci.reduceIndex(clause, start + i);
        }
    }

    /*********************************************************/
    /* Retrieval and Index Building                          */
    /*********************************************************/

    /**
     * <p>Retrieve the clauses.</p>
     * <p>Can be used by multiple readers, with overhead.</p>
     *
     * @param cr The bouquet.
     * @return The clauses.
     */
    public static Clause[] getClauses(Bouquet cr) {
        Clause[] help = cr.cache;
        if (help != null)
            return help;

        AbstractList<Clause> temp = cr.set;
        if (temp != null) {
            help = new Clause[temp.size()];
            temp.toArray(help);
        } else {
            help = ARRAY_VOID;
        }
        cr.cache = help;
        return help;
    }

    /**
     * <p>Retrieve a clause list for the given term.</p>
     *
     * @param cr The bouquet.
     * @param m  The term skel.
     * @param d  The term display.
     * @param en The engine.
     * @return The clauses, or null.
     */
    public static Bouquet definedClauses(Bouquet cr,
                                         Object m, Display d,
                                         Engine en) {
        if (!(m instanceof SkelCompound))
            return cr;
        Object[] tc = ((SkelCompound) m).args;
        int at = 0;
        int start = 0;
        for (; ; ) {
            at = cr.firstValue(at, start, tc, d, en);
            if (at == -1)
                return cr;
            Index ci = cr.nthIndex(at, start);
            if (ci != null) {
                AbstractAssoc<Object, Bouquet> temp = ci.map;
                if (temp != null) {
                    m = Index.keyValue(en.skel);
                    cr = temp.get(m);
                    if (cr == null)
                        cr = ci.nonguard;
                } else {
                    cr = ci.nonguard;
                }
                if (cr == null)
                    return ci.getVoid();
                if (cr.set.size() == 1)
                    return cr;
                at++;
                start = at;
            } else {
                at++;
            }
        }
    }

    /**
     * <p>Compute the first indexing value.</p>
     *
     * @param at    The at.
     * @param start The start.
     * @param tc    The term.
     * @param d     The display of the term.
     * @param en    The engine copy.
     * @return The indexing value.
     */
    private int firstValue(int at, int start,
                           Object[] tc, Display d,
                           Engine en) {
        Index[] help = args;
        if (help != null) {
            int h;
            for (; (h = at - start) < help.length; at++) {
                Index ci = help[h];
                if (ci != null && ci.canSkip())
                    continue;
                Object m = tc[at];
                Display d1 = d;
                BindUniv b1;
                while (m instanceof SkelVar &&
                        (b1 = d1.bind[((SkelVar) m).id]).display != null) {
                    m = b1.skel;
                    d1 = b1.display;
                }
                if (m instanceof SkelVar)
                    continue;
                en.skel = m;
                return at;
            }
        }
        for (; at < tc.length; at++) {
            Object m = tc[at];
            Display d1 = d;
            BindUniv b1;
            while (m instanceof SkelVar &&
                    (b1 = d1.bind[((SkelVar) m).id]).display != null) {
                m = b1.skel;
                d1 = b1.display;
            }
            if (m instanceof SkelVar)
                continue;
            en.skel = m;
            return at;
        }
        return -1;
    }

    /**
     * <p>Retrieve the nth index.</p>
     * <p>Can be used by multiple readers, with overhead.</p>
     *
     * @param at    The at.
     * @param start The start.
     * @return The nth index.
     */
    private Index nthIndex(int at, int start) {
        Index[] help = args;

        int h = at - start;
        if (help == null || h >= help.length) {
            Index[] newargs = new Index[h + 1];
            if (help != null)
                System.arraycopy(help, 0, newargs, 0, help.length);
            help = newargs;
            args = help;
        }

        Index ci = help[h];
        if (ci != null)
            return ci;

        ci = createIndex();
        AbstractList<Clause> rope = set;
        if (rope != null)
            Bouquet.buildIndex(rope, ci, at);
        help[h] = ci;

        return (!ci.canSkip() ? ci : null);
    }

    /*********************************************************/
    /* Rope Polymorphism                                     */
    /*********************************************************/

    /**
     * <p>Build an index.</p>
     *
     * @param rope The rope.
     * @param ci   The clause index.
     * @param at   The position.
     */
    private static void buildIndex(AbstractList<Clause> rope, Index ci, int at) {
        if (rope instanceof ListArray) {
            ListArray<Clause> list = (ListArray<Clause>) rope;
            for (int i = 0; i < list.size(); i++)
                ci.buildIndex(list.get(i), at);
        } else {
            SetHashLink<Clause> hash = (SetHashLink<Clause>) rope;
            for (SetEntry<Clause> entry = hash.getFirstEntry();
                 entry != null; entry = hash.successor(entry))
                ci.buildIndex(entry.value, at);
        }
    }

}
