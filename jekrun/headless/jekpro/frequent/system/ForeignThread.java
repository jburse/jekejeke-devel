package jekpro.frequent.system;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.call.*;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import matula.comp.sharik.AbstractTracking;
import matula.util.config.AbstractBundle;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;
import matula.util.system.ConnectionReader;
import matula.util.system.ConnectionWriter;
import matula.util.wire.AbstractLivestock;
import matula.util.wire.ManagedGroup;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.util.ArrayList;

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
    public final static int BUF_SIZE = 1024;

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
        ConnectionReader cr;
        if (rd instanceof ConnectionReader && (cr = (ConnectionReader) rd).getBuffer() != 0) {
            ConnectionReader rp = new ConnectionReader(new BufferedReader(cr.getUnbuf(), BUF_SIZE));
            rp.setBuffer(BUF_SIZE);
            rp.setEncoding(cr.getEncoding());
            rp.setUnbuf(cr.getUnbuf());
            rd = rp;
        }
        inter2.setProperty(Toolkit.PROP_SYS_CUR_INPUT, rd);
        Object wr = inter.getProperty(Toolkit.PROP_SYS_CUR_OUTPUT);
        ConnectionWriter cw;
        if (wr instanceof ConnectionWriter && (cw = (ConnectionWriter) wr).getBuffer() != 0) {
            ConnectionWriter wp = new ConnectionWriter(new BufferedWriter(cw.getUnbuf(), BUF_SIZE));
            wp.setBuffer(BUF_SIZE);
            wp.setEncoding(cw.getEncoding());
            wp.setUnbuf(cw.getUnbuf());
            wr = wp;
        }
        inter2.setProperty(Toolkit.PROP_SYS_CUR_OUTPUT, wr);
        wr = inter.getProperty(Toolkit.PROP_SYS_CUR_ERROR);
        if (wr instanceof ConnectionWriter && (cw = (ConnectionWriter) wr).getBuffer() != 0) {
            ConnectionWriter wp = new ConnectionWriter(new BufferedWriter(cw.getUnbuf(), BUF_SIZE));
            wp.setBuffer(BUF_SIZE);
            wp.setEncoding(cw.getEncoding());
            wp.setUnbuf(cw.getUnbuf());
            wr = wp;
        }
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
            while (AbstractLivestock.liveGetSignal(t) != null & sleep > 0) {
                t.wait(sleep);
                sleep = when - System.currentTimeMillis();
            }
            if (sleep > 0) {
                AbstractLivestock.liveSetSignal(t, h);
                return true;
            } else {
                return false;
            }
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
        ArrayEnumeration<String> dc;
        if (co.getFirst()) {
            Engine en = (Engine) inter.getEngine();
            ArrayList<String> list = ForeignThread.listThreadFlags(en);
            String[] arr = new String[list.size()];
            list.toArray(arr);
            dc = new ArrayEnumeration<String>(arr);
            if (!dc.hasMoreElements())
                return null;
            co.setData(dc);
        } else {
            dc = (ArrayEnumeration<String>) co.getData();
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
     * @throws InterpreterException Flag undefined.
     * @throws InterpreterMessage   Flag undefined.
     */
    public static Object sysGetThreadFlag(Interpreter inter, Thread t,
                                          String flag)
            throws InterpreterException, InterpreterMessage {
        Engine en = (Engine) inter.getEngine();
        Object val;
        try {
            val = ForeignThread.getThreadFlag(flag, t, en);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
        if (val == null)
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    "prolog_flag", flag));
        return val;
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
        Engine en = (Engine) inter.getEngine();
        try {
            ForeignThread.setFlag(flag, AbstractTerm.getSkel(val),
                    AbstractTerm.getDisplay(val), t, en);
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
    public static ArrayList<String> listThreadFlags(Engine en) {
        ArrayList<String> res = new ArrayList<String>();
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
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
     * <p>Retrieve the value of the given thread flag.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param flag The flag.
     * @param t    The thread.
     * @param en   The engine.
     * @return The value or null.
     * @throws EngineMessage Shit happens.
     */
    public static Object getThreadFlag(String flag,
                                       Thread t, Engine en)
            throws EngineMessage, EngineException {
        AbstractFlag af = findThreadFlag(flag, en);
        return af.getObjFlag(t, en);
    }

    /**
     * <p>Change the value of a prolog Prolog flag.</p>
     * <p>Throws a domain error for undefined flags.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param flag The name of the flag.
     * @param m    The value skel.
     * @param d    The value display.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void setFlag(String flag, Object m, Display d,
                               Thread t, Engine en)
            throws EngineMessage {
        AbstractFlag af = findThreadFlag(flag, en);
        if (!af.setObjFlag(t, m, d, en))
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_MODIFY,
                    EngineMessage.OP_PERMISSION_FLAG, new SkelAtom(flag)));
    }

    /**
     * <p>Find a thread flag.</p>
     *
     * @param flag The flag name.
     * @param en   The engine.
     * @return The thread flag.
     * @throws EngineMessage Shit happens.
     */
    private static AbstractFlag findThreadFlag(String flag, Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot
                = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            MapHash<String, AbstractFlag<Thread>> pfs = branch.getThreadFlags();
            if (pfs == null)
                continue;
            AbstractFlag af = pfs.get(flag);
            if (af != null)
                return af;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_FLAG,
                new SkelAtom(flag)));
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
