package jekpro.model.inter;

import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.rope.Bouquet;
import jekpro.model.rope.Clause;
import jekpro.model.rope.InterfaceRope;
import jekpro.tools.term.SkelAtom;
import matula.util.misc.AbstractLock;
import matula.util.misc.Nonescalable;
import matula.util.wire.AbstractLivestock;

import java.io.IOException;
import java.io.Writer;

/**
 * <p>The delegate class for a multi blocking delegate.</p>
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
public final class DefinedBlockingMulti extends AbstractDefinedMultifile {
    private final Bouquet cr = new Bouquet();
    private final Nonescalable lock = new Nonescalable();

    /**
     * <p>Create a blocking delegate.</p>
     *
     * @param flags The store flags.
     */
    DefinedBlockingMulti(int flags) {
        super(flags);
    }

    /**
     * <p>Retrieve the write lock.</p>
     *
     * @return The write lock.
     */
    public AbstractLock getWrite() {
        return lock.getWrite();
    }

    /**
     * <p>Retrieve the read lock.</p>
     *
     * @return The read lock.
     */
    public AbstractLock getRead() {
        return lock.getRead();
    }

    /**************************************************************/
    /* Variation Points Predicate                                 */
    /**************************************************************/

    /**
     * <p>Shrink this predicate from the store for a source.</p>
     *
     * @param pick  The predicate.
     * @param scope The source.
     * @throws EngineMessage Shit happens.
     */
    public final void shrinkPredicate(Predicate pick, AbstractSource scope)
            throws EngineMessage {
        Clause[] list = listClauses(null);
        for (int j = 0; j < list.length; j++) {
            Clause clause = list[j];
            SkelAtom sa = StackElement.callableToName(clause.term);
            if (scope == sa.scope)
                retractClause(clause, null);
        }
    }

    /**
     * <p>Release this predicate from the store.</p>
     *
     * @param pick The predicate.
     */
    public final void releasePredicate(Predicate pick) {
        /* do nothing */
    }

    /**************************************************************/
    /* Variation Points Predicate Defined                         */
    /**************************************************************/

    /**
     * <p>Retrieve the clause list.</p>
     *
     * @param en The engine.
     * @return the clause list or null.
     * @throws EngineMessage Shit happens.
     */
    public final Clause[] listClauses(Engine en)
            throws EngineMessage {
        try {
            getRead().acquire();
        } catch (InterruptedException x) {
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        }
        try {
            return cr.getClauses();
        } finally {
            getRead().release();
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
        try {
            getRead().acquire();
        } catch (InterruptedException x) {
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        }
        try {
            Bouquet temp = cr;
            InterfaceRope set = temp.set;
            if (set != null && set.size() != 1 &&
                    (en.store.foyer.getBits() & Foyer.MASK_FOYER_NIDX) == 0)
                temp = Bouquet.definedClauses(temp, m, d, en);
            return temp.getClauses();
        } finally {
            getRead().release();
        }
    }

    /**
     * <p>Retrieve the length of the clause list.</p>
     *
     * @param en The engine.
     * @return The length of the clause list.
     */
    public final int lengthClauses(Engine en) {
        InterfaceRope set = cr.set;
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
        try {
            getWrite().acquire();
        } catch (InterruptedException x) {
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        }
        try {
            if ((clause.flags & Clause.MASK_CLAUSE_ASSE) != 0)
                return false;
            clause.flags |= Clause.MASK_CLAUSE_ASSE;
            cr.assertClause(0, clause, flags);
            return true;
        } finally {
            getWrite().release();
        }
    }

    /**
     * <p>Remove the clause from the predicate.</p>
     *
     * @param clause The clause.
     * @param en     The engine.
     * @return True if clause was found and removed, otherwise false.
     */
    public final boolean retractClause(Clause clause, Engine en)
            throws EngineMessage {
        if ((clause.flags & Clause.MASK_CLAUSE_ASSE) == 0)
            return false;
        try {
            getWrite().acquire();
        } catch (InterruptedException x) {
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        }
        try {
            if ((clause.flags & Clause.MASK_CLAUSE_ASSE) == 0)
                return false;
            clause.flags &= ~Clause.MASK_CLAUSE_ASSE;
            cr.retractClause(0, clause);
            return true;
        } finally {
            getWrite().release();
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
        try {
            getRead().acquire();
        } catch (InterruptedException x) {
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        }
        try {
            try {
                InterfaceRope set = cr.set;
                int len = (set != null ? set.getLengthScope(en) : 0);
                cr.inspectPaths(wr, 0, 0, len, en);
            } catch (IOException x) {
                throw EngineMessage.mapIOException(x);
            }
        } finally {
            getRead().release();
        }
    }

}