package jekpro.model.inter;

import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.rope.Bouquet;
import jekpro.model.rope.Clause;
import jekpro.model.rope.InterfaceClauses;
import matula.util.data.ListArray;
import matula.util.wire.AbstractLivestock;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>The delegate class for group local delegate.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class DefinedGroupLocal extends AbstractDefined {
    private final int seqid;

    /**
     * <p>Create a thread local group predicate.</p>
     *
     * @param s     The sequence id.
     * @param flags The store flags.
     */
    DefinedGroupLocal(int s, int flags) {
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
    public final void shrinkPredicate(Predicate pick, AbstractSource scope) {
        /* do nothing */
    }

    /**
     * <p>Release this predicate from the store.</p>
     *
     * @param pick The predicate.
     */
    public final void releasePredicate(Predicate pick) {
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
     * @throws EngineMessage Shit happens.
     */
    public final Clause[] listClauses(Engine en)
            throws EngineMessage {
        LocalBlocking ep = defineLocalBlocking(en);
        try {
            ep.getRead().acquire();
        } catch (InterruptedException x) {
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        }
        try {
            return ep.cr.getClauses();
        } finally {
            ep.getRead().release();
        }
    }

    /**
     * <p>Retrieve a clause list for the given goal.</p>
     *
     * @param m  The goal skel.
     * @param d  The goal display.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    final Clause[] definedClauses(Object m, Display d, Engine en)
            throws EngineMessage {
        LocalBlocking ep = defineLocalBlocking(en);
        try {
            ep.getRead().acquire();
        } catch (InterruptedException x) {
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        }
        try {
            InterfaceClauses set = ep.cr.set;
            if ((set == null || set.size() == 1) ||
                    (en.store.foyer.getBits() & Foyer.MASK_STORE_NIDX) != 0)
                return ep.cr.getClauses();
            return Bouquet.definedClauses(ep.cr, m, d, en);
        } finally {
            ep.getRead().release();
        }
    }

    /**
     * <p>Retrieve the length of the clause list.</p>
     *
     * @param en The engine.
     * @return The length of the clause list.
     */
    public final int lengthClauses(Engine en) {
        LocalBlocking ep = defineLocalBlocking(en);
        InterfaceClauses set = ep.cr.set;
        return (set != null ? set.size() : 0);
    }

    /**
     * <p>Add the clause to the predicate.</p>
     *
     * @param clause The clause.
     * @param flags  The flags.
     * @param en     The engine.
     * @throws EngineMessage Shit happens.
     */
    public final boolean assertClause(Clause clause,
                                      int flags, Engine en)
            throws EngineMessage {
        if ((clause.flags & Clause.MASK_CLAUSE_ASSE) != 0)
            return false;
        LocalBlocking ep = defineLocalBlocking(en);
        try {
            ep.getWrite().acquire();
        } catch (InterruptedException x) {
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        }
        try {
            if ((clause.flags & Clause.MASK_CLAUSE_ASSE) != 0)
                return false;
            clause.flags |= Clause.MASK_CLAUSE_ASSE;
            ep.cr.assertClause(0, clause, flags);
            return true;
        } finally {
            ep.getWrite().release();
        }
    }

    /**
     * <p>Remove the clause from the predicate.</p>
     *
     * @param clause The clause.
     * @param en     The engine.
     * @return True if clause was found and removed, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public final boolean retractClause(Clause clause, Engine en)
            throws EngineMessage {
        if ((clause.flags & Clause.MASK_CLAUSE_ASSE) == 0)
            return false;
        LocalBlocking ep = defineLocalBlocking(en);
        try {
            ep.getWrite().acquire();
        } catch (InterruptedException x) {
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        }
        try {
            if ((clause.flags & Clause.MASK_CLAUSE_ASSE) == 0)
                return false;
            clause.flags &= ~Clause.MASK_CLAUSE_ASSE;
            ep.cr.retractClause(0, clause);
            return true;
        } finally {
            ep.getWrite().release();
        }
    }

    /**
     * <p>Inspect the index of a predicate.</p>
     *
     * @param wr The write.
     * @param en The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final void inspectClauses(Writer wr, Engine en)
            throws EngineMessage, EngineException {
        LocalBlocking ep = defineLocalBlocking(en);
        try {
            ep.getRead().acquire();
        } catch (InterruptedException x) {
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        }
        try {
            try {
                InterfaceClauses set = ep.cr.set;
                int len = (set != null ? set.getLengthScope(en) : 0);
                ep.cr.inspectPaths(wr, 0, 0, len, en);
            } catch (IOException x) {
                throw EngineMessage.mapIOException(x);
            }
        } finally {
            ep.getRead().release();
        }
    }

    /***********************************************************/
    /* Locale Group State                                      */
    /***********************************************************/

    /**
     * <p>Retrieve an engine private locked.</p>
     *
     * @param en The engine.
     * @return The engine private locked.
     */
    private LocalBlocking defineLocalBlocking(Engine en) {
        LocalBlocking ep = prefetchLocalBlocking(en);
        if (ep != null)
            return ep;
        synchronized (en.store) {
            ListArray<LocalBlocking> privs = en.store.privates;
            if (privs == null) {
                privs = new ListArray<LocalBlocking>();
                en.store.privates = privs;
            }
            while (seqid >= privs.size())
                privs.add(null);
            ep = privs.get(seqid);
            if (ep == null) {
                ep = new LocalBlocking(this);
                privs.set(seqid, ep);
            } else if (ep.del != this) {
                ep.cr = Bouquet.newBouquet();
                ep.del = this;
            }
        }
        return ep;
    }

    /**
     * <p>Prefetch an engine private locked.</p>
     *
     * @param en The engine.
     * @return The engine private locked.
     */
    private LocalBlocking prefetchLocalBlocking(Engine en) {
        ListArray<LocalBlocking> privs = en.store.privates;
        if (privs == null)
            return null;
        if (seqid >= privs.size())
            return null;
        LocalBlocking ep = privs.get(seqid);
        if (ep == null)
            return null;
        if (ep.del != this)
            return null;
        return ep;
    }

}