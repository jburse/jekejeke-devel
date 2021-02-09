package jekpro.model.inter;

import jekpro.model.molec.Display;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.rope.Bouquet;
import jekpro.model.rope.Clause;
import matula.util.data.AbstractList;
import matula.util.data.ListArray;

import java.util.concurrent.locks.ReadWriteLock;

/**
 * <p>The delegate class for thread local delegate.</p>
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
final class DefinedThreadLocal extends AbstractDefined {
    private final int seqid;

    /**
     * <p>Create a thread local predicate.</p>
     *
     * @param s     The sequence id.
     * @param flags The store flags.
     */
    DefinedThreadLocal(int s, int flags) {
        super(flags);
        seqid = s;
    }

    /**************************************************************/
    /* Variation Points Predicate                                 */
    /**************************************************************/

    /**
     * <p>Shrink this predicate from the store for a source.</p>
     *
     * @param pick  The predicate.
     * @param scope The source.
     */
    public void shrinkPredicate(Predicate pick, AbstractSource scope) {
        /* do nothing */
    }

    /**
     * <p>Release this predicate from the store.</p>
     *
     * @param pick The predicate.
     */
    public void releasePredicate(Predicate pick) {
        pick.getSource().getStore().foyer.releaseHole(seqid);
    }

    /**************************************************************/
    /* Variation Points Predicate Defined                         */
    /**************************************************************/

    /**
     * <p>Retrieve the clause list.</p>
     *
     * @param en The engine.
     * @return The clause list or null.
     */
    public Clause[] listClauses(Engine en) {
        LocalLockfree ep = defineLocalLockfree(en);
        return ep.cr.getClauses();
    }

    /**
     * <p>Retrieve a clause list for the given term.</p>
     *
     * @param m  The term skel.
     * @param d  The term display.
     * @param en The engine.
     */
    final Clause[] definedClauses(Object m, Display d, Engine en) {
        LocalLockfree ep = defineLocalLockfree(en);
        Bouquet temp = ep.cr;
        AbstractList<Clause> set = temp.set;
        if (set != null && set.size() != 1 &&
                (subflags & AbstractDefined.MASK_DEFI_NIDX) == 0)
            temp = Bouquet.definedClauses(temp, m, d, en);
        return temp.getClauses();
    }

    /**
     * <p>Add the clause to the predicate.</p>
     *
     * @param clause The clause.
     * @param flags  The flags.
     * @param en     The engine.
     */
    public boolean assertClause(Clause clause,
                                int flags, Engine en) {
        if ((clause.flags & Clause.MASK_CLAUSE_ASSE) != 0)
            return false;
        LocalLockfree ep = defineLocalLockfree(en);
        clause.flags |= Clause.MASK_CLAUSE_ASSE;
        ep.cr.assertClause(0, clause, flags);
        return true;
    }

    /**
     * <p>Remove the clause from the predicate.</p>
     *
     * @param clause The clause.
     * @param en     The engine.
     * @return True if clause was found and removed, otherwise false.
     */
    public boolean retractClause(Clause clause, Engine en) {
        if ((clause.flags & Clause.MASK_CLAUSE_ASSE) == 0)
            return false;
        LocalLockfree ep = defineLocalLockfree(en);
        clause.flags &= ~Clause.MASK_CLAUSE_ASSE;
        ep.cr.retractClause(0, clause);
        return true;
    }

    /**
     * <p>Retrieve the read write lock.</p>
     *
     * @param en The engine.
     * @return The read write lock.
     */
    public ReadWriteLock getLock(Engine en) {
        return null;
    }

    /**
     * <p>Retrieve the clause and index bouquet.</p>
     *
     * @param en The engine.
     * @return The read write lock.
     */
    public Bouquet getBouquet(Engine en) {
        LocalLockfree ep = defineLocalLockfree(en);
        return ep.cr;
    }

    /***********************************************************/
    /* Locale State                                            */
    /***********************************************************/

    /**
     * <p>Retrieve an engine private.</p>
     *
     * @param en The engine.
     * @return The engine private.
     */
    private LocalLockfree defineLocalLockfree(Engine en) {
        ListArray<LocalLockfree> privs = en.visor.privates;
        if (privs == null) {
            privs = new ListArray<>();
            en.visor.privates = privs;
        }
        while (seqid >= privs.size())
            privs.add(null);
        LocalLockfree ep = privs.get(seqid);
        if (ep == null) {
            ep = new LocalLockfree(this);
            ep.del = this;
            privs.set(seqid, ep);
        } else if (ep.del != this) {
            ep.cr = new Bouquet();
            ep.del = this;
        }
        return ep;
    }

}
