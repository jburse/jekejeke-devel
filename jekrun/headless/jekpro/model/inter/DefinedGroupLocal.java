package jekpro.model.inter;

import jekpro.frequent.system.ForeignThread;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.rope.Bouquet;
import jekpro.model.rope.Clause;
import jekpro.model.rope.InterfaceRope;
import matula.util.data.ListArray;

import java.io.IOException;
import java.io.Writer;
import java.util.concurrent.locks.ReadWriteLock;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
final class DefinedGroupLocal extends AbstractDefined {
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
     * @throws EngineMessage Shit happens.
     */
    public Clause[] listClauses(Engine en)
            throws EngineMessage {
        LocalBlocking ep = defineLocalBlocking(en);
        try {
            ep.lock.readLock().lockInterruptibly();
        } catch (InterruptedException x) {
            throw (EngineMessage) ForeignThread.sysThreadClear();
        }
        try {
            return ep.cr.getClauses();
        } finally {
            ep.lock.readLock().unlock();
        }
    }

    /**
     * <p>Retrieve a clause list for the given term.</p>
     *
     * @param m  The term skel.
     * @param d  The term display.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    final Clause[] definedClauses(Object m, Display d, Engine en)
            throws EngineMessage {
        LocalBlocking ep = defineLocalBlocking(en);
        try {
            ep.lock.readLock().lockInterruptibly();
        } catch (InterruptedException x) {
            throw (EngineMessage) ForeignThread.sysThreadClear();
        }
        try {
            Bouquet temp = ep.cr;
            InterfaceRope set = temp.set;
            if (set != null && set.size() != 1 &&
                    (subflags & AbstractDefined.MASK_DEFI_NIDX) == 0)
                temp = Bouquet.definedClauses(temp, m, d, en);
            return temp.getClauses();
        } finally {
            ep.lock.readLock().unlock();
        }
    }

    /**
     * <p>Retrieve the length of the clause list.</p>
     *
     * @param en The engine.
     * @return The length of the clause list.
     */
    public int lengthClauses(Engine en) {
        LocalBlocking ep = defineLocalBlocking(en);
        InterfaceRope set = ep.cr.set;
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
    public boolean assertClause(Clause clause,
                                int flags, Engine en)
            throws EngineMessage {
        if ((clause.flags & Clause.MASK_CLAUSE_ASSE) != 0)
            return false;
        LocalBlocking ep = defineLocalBlocking(en);
        try {
            ep.lock.writeLock().lockInterruptibly();
        } catch (InterruptedException x) {
            throw (EngineMessage) ForeignThread.sysThreadClear();
        }
        try {
            if ((clause.flags & Clause.MASK_CLAUSE_ASSE) != 0)
                return false;
            clause.flags |= Clause.MASK_CLAUSE_ASSE;
            ep.cr.assertClause(0, clause, flags);
            return true;
        } finally {
            ep.lock.writeLock().unlock();
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
    public boolean retractClause(Clause clause, Engine en)
            throws EngineMessage {
        if ((clause.flags & Clause.MASK_CLAUSE_ASSE) == 0)
            return false;
        LocalBlocking ep = defineLocalBlocking(en);
        try {
            ep.lock.writeLock().lockInterruptibly();
        } catch (InterruptedException x) {
            throw (EngineMessage) ForeignThread.sysThreadClear();
        }
        try {
            if ((clause.flags & Clause.MASK_CLAUSE_ASSE) == 0)
                return false;
            clause.flags &= ~Clause.MASK_CLAUSE_ASSE;
            ep.cr.retractClause(0, clause);
            return true;
        } finally {
            ep.lock.writeLock().unlock();
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
    public void inspectClauses(Writer wr, Engine en)
            throws EngineMessage, EngineException {
        LocalBlocking ep = defineLocalBlocking(en);
        try {
            ep.lock.readLock().lockInterruptibly();
        } catch (InterruptedException x) {
            throw (EngineMessage) ForeignThread.sysThreadClear();
        }
        try {
            try {
                InterfaceRope set = ep.cr.set;
                int len = (set != null ? set.getLengthScope(en) : 0);
                ep.cr.inspectPaths(wr, 0, 0, len, en);
            } catch (IOException x) {
                throw EngineMessage.mapIOException(x);
            }
        } finally {
            ep.lock.readLock().unlock();
        }
    }

    /**
     * <p>Retrieve the read write lock.</p>
     *
     * @param en The engine.
     * @return The read write lock.
     */
    public ReadWriteLock getLock(Engine en) {
        LocalBlocking ep = defineLocalBlocking(en);
        return ep.lock;
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
                privs = new ListArray<>();
                en.store.privates = privs;
            }
            while (seqid >= privs.size())
                privs.add(null);
            ep = privs.get(seqid);
            if (ep == null) {
                ep = new LocalBlocking(this);
                privs.set(seqid, ep);
            } else if (ep.del != this) {
                ep.cr = new Bouquet();
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