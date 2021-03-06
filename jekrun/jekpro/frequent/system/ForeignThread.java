package jekpro.frequent.system;

import matula.util.misc.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.call.*;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import matula.util.config.BaseTracking;
import matula.util.config.BaseBundle;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;
import matula.util.wire.AbstractLivestock;

import java.util.Enumeration;

/**
 * The foreign predicates for the module system/thread.
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
public final class ForeignThread {

    /****************************************************************/
    /* Thread Creation                                              */
    /****************************************************************/

    /**
     * <p>Make a copy of the given term and create a thread
     * running the term once.</p>
     *
     * @param inter The interpreter.
     * @param t     The term.
     * @return The new thread.
     */
    public static Thread sysThreadNew(Interpreter inter, AbstractTerm t)
            throws InterpreterMessage, InterpreterException {
        Object obj = AbstractTerm.copyMolec(inter, t);
        Interpreter inter2 = makeInterpreter(inter);
        CallIn callin = inter2.iterator(obj);
        return new Thread(callin);
    }

    /**
     * <p>Make an interpreter.</p>
     *
     * @param inter The old interpreter.
     * @return The new interpreter.
     * @throws InterpreterMessage Shit happens.
     */
    static Interpreter makeInterpreter(Interpreter inter)
            throws InterpreterMessage, InterpreterException {
        final Interpreter inter2 = inter.getKnowledgebase().iterable();
        Object rd = inter.getProperty(Toolkit.PROP_SYS_CUR_INPUT);
        inter2.setProperty(Toolkit.PROP_SYS_CUR_INPUT, rd);
        Object wr = inter.getProperty(Toolkit.PROP_SYS_CUR_OUTPUT);
        inter2.setProperty(Toolkit.PROP_SYS_CUR_OUTPUT, wr);
        wr = inter.getProperty(Toolkit.PROP_SYS_CUR_ERROR);
        inter2.setProperty(Toolkit.PROP_SYS_CUR_ERROR, wr);
        inter2.setProperty(Toolkit.PROP_SYS_ATTACHED_TO, inter.getProperty(Toolkit.PROP_SYS_ATTACHED_TO));
        return inter2;
    }

    /****************************************************************/
    /* Thread Signalling                                            */
    /****************************************************************/

    /**
     * <p>Set the signal of a thread.</p>
     * <p>Block if thread has already a signal.</p>
     *
     * @param t The thread.
     * @param m The message.
     * @throws InterruptedException The current thread was interrupted.
     */
    public static void sysThreadAbort(Thread t, AbstractTerm m)
            throws InterruptedException {
        EngineMessage h = (EngineMessage) new InterpreterMessage(m).getException();
        synchronized (t) {
            while (AbstractLivestock.liveGetSignal(t) != null)
                t.wait();
            AbstractLivestock.liveSetSignal(t, h);
        }
    }

    /**
     * <p>Set the signal of a thread.</p>
     * <p>Fail if thread has already a signal.</p>
     *
     * @param t The thread.
     * @param m The message.
     */
    public static boolean sysThreadDown(Thread t, AbstractTerm m) {
        EngineMessage h = (EngineMessage) new InterpreterMessage(m).getException();
        synchronized (t) {
            if (AbstractLivestock.liveGetSignal(t) == null) {
                AbstractLivestock.liveSetSignal(t, h);
                return true;
            } else {
                return false;
            }
        }
    }

    /**
     * <p>Set the signal of a thread or timeout.</p>
     *
     * @param t     The thread.
     * @param m     The message.
     * @param sleep The time-out.
     * @return True if signal is set, otherwise false.
     * @throws InterruptedException The current thread was interrupted.
     */
    public static boolean sysThreadDown(Thread t, AbstractTerm m, long sleep)
            throws InterruptedException {
        EngineMessage h = (EngineMessage) new InterpreterMessage(m).getException();
        long when = System.currentTimeMillis() + sleep;
        synchronized (t) {
            for (; ; ) {
                if (AbstractLivestock.liveGetSignal(t) == null) {
                    AbstractLivestock.liveSetSignal(t, h);
                    return true;
                } else if (sleep > 0) {
                    t.wait(sleep);
                    sleep = when - System.currentTimeMillis();
                } else {
                    return false;
                }
            }
        }
    }

    /**
     * <p>Clear the signal.</p>
     *
     * @return The old signal, can be null.
     */
    public static Throwable sysThreadClear() {
        Thread t = Thread.currentThread();
        synchronized (t) {
            Throwable m = AbstractLivestock.liveSetSignal(t, null);
            t.notifyAll();
            return m;
        }
    }

    /****************************************************************/
    /* Thread Joining                                               */
    /****************************************************************/

    /**
     * <p>Check if a thread has terminated.</p>
     *
     * @param t The thread.
     * @return True if thread is terminated, otherwise false.
     */
    public static boolean sysThreadCombine(Thread t) {
        return !t.isAlive();
    }

    /**
     * <p>Wait till a thread has terminated or time-out.</p>
     *
     * @param t     The thread.
     * @param sleep The time-out.
     * @return True if thread is terminated, otherwise false.
     * @throws InterruptedException The current thread was interrupted.
     */
    public static boolean sysThreadCombine(Thread t, long sleep)
            throws InterruptedException {
        t.join(sleep);
        return !t.isAlive();
    }

    /****************************************************************/
    /* Thread Inspection                                            */
    /****************************************************************/

    /**
     * <p>Retrieve the known thread flags.</p>
     *
     * @param inter The interpreter.
     * @param co    The call out.
     * @return The thread flags.
     */
    public static String sysCurrentThreadFlag(Interpreter inter, CallOut co) {
        Enumeration<String> dc;
        if (co.getFirst()) {
            Engine en = inter.getEngine();
            ListArray<String> list = ForeignThread.listThreadFlags(en);
            dc = list.elements();
            if (!dc.hasMoreElements())
                return null;
            co.setData(dc);
        } else {
            dc = (Enumeration<String>) co.getData();
        }
        String res = dc.nextElement();
        co.setRetry(dc.hasMoreElements());
        return res;
    }

    /**
     * <p>Retrieve a thread flag.</p>
     *
     * @param inter The interpreter.
     * @param t     The thread.
     * @param flag  The thread flag.
     * @return The value.
     * @throws InterpreterMessage Flag undefined.
     */
    public static Object sysGetThreadFlag(Interpreter inter, Thread t,
                                          String flag)
            throws InterpreterMessage {
        Engine en = inter.getEngine();
        AbstractFlag<Thread> af = findThreadFlag(flag, en);
        if (af != null)
            return af.getObjFlag(t);
        throw new InterpreterMessage(InterpreterMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_FLAG, flag));
    }

    /**
     * <p>Set a thread flag.</p>
     *
     * @param inter The interpreter.
     * @param t     The thread.
     * @param flag  The thread flag.
     * @param val   The value.
     * @throws InterpreterMessage Flag undefined.
     */
    public static void sysSetThreadFlag(Interpreter inter, Thread t,
                                        String flag, Object val)
            throws InterpreterMessage {
        Engine en = inter.getEngine();
        try {
            AbstractFlag<Thread> af = findThreadFlag(flag, en);
            if (af != null) {
                if (!af.setObjFlag(t, AbstractTerm.getSkel(val), AbstractTerm.getDisplay(val)))
                    throw new EngineMessage(EngineMessage.permissionError(
                            EngineMessage.OP_PERMISSION_MODIFY,
                            EngineMessage.OP_PERMISSION_FLAG, new SkelAtom(flag)));
                return;
            }
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_PROLOG_FLAG,
                    new SkelAtom(flag)));
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
    }

    /****************************************************************/
    /* Thread Flags                                                 */
    /****************************************************************/

    /**
     * <p>Retrieve the list of thread flags.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param en The engine.
     * @return The list of flags.
     */
    public static ListArray<String> listThreadFlags(Engine en) {
        ListArray<String> res = new ListArray<>();
        MapEntry<BaseBundle, BaseTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<BaseBundle, BaseTracking> entry = snapshot[i];
            BaseTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            MapHash<String, AbstractFlag<Thread>> pfs = branch.getThreadFlags();
            if (pfs == null)
                continue;
            for (MapEntry<String, AbstractFlag<Thread>> entry2 = pfs.getFirstEntry();
                 entry2 != null; entry2 = pfs.successor(entry2)) {
                res.add(entry2.key);
            }
        }
        return res;
    }

    /**
     * <p>Find a thread flag.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param flag The flag name.
     * @param en   The engine.
     * @return The thread flag.
     */
    private static AbstractFlag<Thread> findThreadFlag(String flag, Engine en) {
        MapEntry<BaseBundle, BaseTracking>[] snapshot
                = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<BaseBundle, BaseTracking> entry = snapshot[i];
            BaseTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            MapHash<String, AbstractFlag<Thread>> pfs = branch.getThreadFlags();
            if (pfs == null)
                continue;
            AbstractFlag<Thread> af = pfs.get(flag);
            if (af != null)
                return af;
        }
        return null;
    }


    /**
     * <p>Some testing.</p>
     *
     * @param args No used.
     */
    /*
    public static void main(String[] args) {
        Thread thread = Thread.currentThread();
        Thread.State state = thread.getState();
        System.out.println("state.ordinal=" + state.ordinal());
        System.out.println("state.name=" + state.name());

        String str="TIMED_WAITING";
        System.out.println("str="+str);
        System.out.println("str.length="+str.length());
    }
    */

}
