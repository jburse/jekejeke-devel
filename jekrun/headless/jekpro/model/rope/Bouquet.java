package jekpro.model.rope;

import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

import java.io.IOException;
import java.io.Writer;

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
public final class Bouquet {
    public static final Clause[] ARRAY_VOID = new Clause[0];
    public static final Bouquet BOUQUET_VOID = new Bouquet();

    public InterfaceRope set;
    private Index[] args;
    private Clause[] cache;

    /**
     * <p>Copy a bouquet without its index.</p>
     *
     * @return The copy of this bouquet,
     */
    public Bouquet copyBouquet() {
        Bouquet res = new Bouquet();
        InterfaceRope temp = set;
        if (temp != null)
            res.set = (InterfaceRope) temp.clone();
        res.cache = cache;
        return res;
    }

    /**
     * <p>Add the clause to the list of clause.</p>
     *
     * @param clause The clause to be added.
     * @param flags  The flags
     */
    public void addClause(Clause clause, int flags) {
        if (set == null)
            set = new RopeArray();
        InterfaceRope res = set.addClause(clause, flags);
        if (res != null)
            set = res;
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
        InterfaceRope res = set.removeClause(clause);
        if (res != null)
            set = res;
        if (set.size() == 0)
            set = null;
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
     * @return The clauses.
     */
    public Clause[] getClauses() {
        Clause[] help = cache;
        if (help != null)
            return help;

        InterfaceRope temp = set;
        if (temp != null) {
            help = new Clause[temp.size()];
            temp.toArray(help);
        } else {
            help = ARRAY_VOID;
        }
        cache = help;
        return help;
    }

    /**
     * <p>Retrieve a clause list for the given term.</p>
     *
     * @param cr The clause paths.
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
            m = en.skel;
            Index ci = cr.nthIndex(at, start);
            InterfacePairs temp = ci.map;
            if (temp != null || ci.guard != null) {
                if (temp != null) {
                    m = Index.keyValue(m);
                    cr = temp.get(m);
                    if (cr == null)
                        cr = ci.nonguard;
                } else {
                    cr = ci.nonguard;
                }
                if (cr == null)
                    return Bouquet.BOUQUET_VOID;
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
                if (ci != null && (ci.map == null && ci.guard == null))
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

        ci = new Index();
        InterfaceRope temp = set;
        if (temp != null)
            temp.buildIndex(ci, at);
        help[h] = ci;

        return ci;
    }

    /*********************************************************/
    /* Index Inspection                                      */
    /*********************************************************/

    /**
     * <p>Dump the indexes.</p>
     *
     * @param wr    The writer.
     * @param off   The left indentation.
     * @param start The start position.
     * @param len   The length.
     * @param en    The engine copy.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final void inspectPaths(Writer wr, int off, int start,
                                   int len, Engine en)
            throws IOException, EngineMessage, EngineException {
        wr.write("length=");
        wr.write(Integer.toString(len));
        wr.write("\n");
        Index[] help = args;
        if (help == null)
            return;
        for (int j = 0; j < help.length; j++) {
            Index ci = help[j];
            if (ci == null)
                continue;
            for (int i = 0; i < off; i++)
                wr.write(" ");
            wr.write("at=");
            wr.write(Integer.toString(start + j));
            ci.inspectIndex(wr, off + 2, start + j + 1, en);
        }
    }

}
