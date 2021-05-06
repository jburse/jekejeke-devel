package jekpro.model.inter;

import jekpro.model.pretty.AbstractSource;
import jekpro.model.rope.Bouquet;
import matula.util.data.ListArray;

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
    /* Variation Points                                           */
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

    /**
     * <p>Retrieve the clause and index bouquet.</p>
     *
     * @param en The engine.
     * @return The read write lock.
     */
    public Bouquet getBouquet(Engine en) {
        LocalBlocking ep = defineLocalBlocking(en);
        return ep.cr;
    }

    /***********************************************************/
    /* Groupe State                                            */
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