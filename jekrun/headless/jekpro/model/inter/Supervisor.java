package jekpro.model.inter;

import jekpro.frequent.system.ForeignThread;
import jekpro.model.molec.BindVar;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.StoreKey;
import jekpro.model.rope.InterfaceRope;
import jekpro.model.rope.LoadOpts;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.term.AbstractTerm;
import matula.util.data.ListArray;
import matula.util.wire.AbstractLivestock;
import matula.util.wire.LivestockEvent;
import matula.util.wire.LivestockEventClose;
import matula.util.wire.LivestockEventMemory;

/**
 * <p>The class provides a supervisor.</p>
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
public class Supervisor extends AbstractLivestock {
    public final static int MASK_VISOR_NOCNT = 0x00000002;

    public ListArray<BindVar> cont;
    public Object curinput;
    public Object curoutput;
    public Object curerror;
    public Object dispinput;
    public Object dispoutput;
    public Object disperror;
    public ListArray<LocalLockfree> privates;
    public Object attachedto;
    public int breaklevel = -1;
    public ListArray<AbstractSource> modstack;
    public Frame ref;
    public StoreKey lastsk;
    public LoadOpts cond;
    public Object proxy;
    public Engine inuse;

    /**
     * <p>Create a supervisor for a store.</p>
     *
     * @param foyer The foyer.
     */
    public Supervisor(Foyer foyer) {
        source = foyer;

        /* init default streams */
        curinput = foyer.getFactory().toolinput;
        curoutput = foyer.getFactory().tooloutput;
        curerror = foyer.getFactory().toolerror;

        dispinput = foyer.getFactory().toolinput;
        dispoutput = foyer.getFactory().tooloutput;
    }

    /***************************************************************/
    /* Suspend Handling                                            */
    /***************************************************************/

    /**
     * <p>Set the verify flag.</p>
     * <p>Needs to be synchronize, since flags is shared.</p>
     *
     * @param f The new verify flag.
     * @return The old verify flag.
     */
    public final boolean setVerify(boolean f) {
        synchronized (this) {
            boolean h = (flags & Supervisor.MASK_VISOR_NOCNT) == 0;
            if (f) {
                flags &= ~Supervisor.MASK_VISOR_NOCNT;
            } else {
                flags |= Supervisor.MASK_VISOR_NOCNT;
            }
            return h;
        }
    }

    /*****************************************************************/
    /* Event Handling                                                */
    /*****************************************************************/

    /**
     * <p>Handle an enforced event.</p>
     *
     * @param t The thread.
     * @param e The enforced event.
     * @throws InterruptedException The current tread was interrupted.
     */
    public final void handleEvent(Thread t, LivestockEvent e)
            throws InterruptedException {
        if (e instanceof LivestockEventClose) {
            AbstractTerm userClose = InterpreterMessage.systemError(EngineMessage.OP_SYSTEM_USER_CLOSE);
            ForeignThread.sysThreadAbort(t, userClose);
        } else if (e instanceof LivestockEventMemory) {
            AbstractTerm memoryThreshold = InterpreterMessage.systemError(EngineMessage.OP_SYSTEM_MEMORY_THRESHOLD);
            ForeignThread.sysThreadAbort(t, memoryThreshold);
        }
    }

    /**
     * <p>Retrieve the offender score.</p>
     *
     * @return The offender score.
     */
    public long getOffenderScore() {
        return getThreadLocalClauses();
    }

    /*****************************************************************/
    /* Inuse Livecycle                                               */
    /*****************************************************************/

    /**
     * <p>Set the inuse.</p>
     *
     * @param e The new inuse or null.
     * @return The old inuse or null.
     */
    public final Engine setInuse(Engine e) {
        Engine h = inuse;
        inuse = e;
        return h;
    }

    /****************************************************************/
    /* Module Stack                                                 */
    /****************************************************************/

    /**
     * <p>Push a module on the stack.</p>
     *
     * @param source The module.
     */
    public void pushStack(AbstractSource source) {
        if (source == null)
            throw new NullPointerException("source missing");
        if (modstack == null)
            modstack = new ListArray<AbstractSource>();
        modstack.add(source);
    }

    /**
     * <p>Pop a module from the stack.</p>
     */
    public void popStack() {
        modstack.remove(modstack.size() - 1);
        if (modstack.size() == 0)
            modstack = null;
    }

    /**
     * <p>Retrieve the top of the stack.</p>
     *
     * @return The top.
     */
    public AbstractSource peekStack() {
        if (modstack == null)
            return null;
        return modstack.get(modstack.size() - 1);
    }

    /****************************************************************/
    /* Thread Statistics                                            */
    /****************************************************************/

    /**
     * <p>Retrieve the thread local length.</p>
     *
     * @return The thread local length.
     */
    public long getThreadLocalClauses() {
        ListArray<LocalLockfree> privs = privates;
        if (privs == null)
            return 0;
        long total = 0;
        for (int i = 0; i < privs.size(); i++) {
            LocalLockfree ep = privs.get(i);
            if (ep == null)
                continue;
            InterfaceRope set = ep.cr.set;
            if (set == null)
                continue;
            total += set.size();
        }
        return total;
    }

    /***************************************************************/
    /* Ignore Handling                                             */
    /***************************************************************/

    /**
     * <p>Set the ignore flag.</p>
     * <p>Can be overridden by subclasses.</p>
     *
     * @param f The new ignore flag.
     * @return The old ignore flag.
     */
    public boolean setIgnore(boolean f) {
        return false;
    }

}
