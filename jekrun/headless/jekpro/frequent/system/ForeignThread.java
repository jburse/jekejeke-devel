package jekpro.frequent.system;

import jekpro.tools.call.*;
import jekpro.tools.term.Term;
import matula.util.system.ConnectionReader;
import matula.util.system.ConnectionWriter;

import java.io.BufferedReader;
import java.io.BufferedWriter;

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
    public static Thread sysThreadNew(Interpreter inter, Term t)
            throws InterpreterMessage {
        t = Term.copyTermWrapped(inter, t);
        final Interpreter inter2 = inter.getKnowledgebase().iterable();
        Object rd = inter.getProperty(Toolkit.PROP_SYS_DISP_INPUT);
        ConnectionReader cr;
        if (rd instanceof ConnectionReader && (cr = (ConnectionReader) rd).getBuffer() != 0) {
            ConnectionReader rp = new ConnectionReader(new BufferedReader(cr.getUnbuf(), BUF_SIZE));
            rp.setBuffer(BUF_SIZE);
            rp.setEncoding(cr.getEncoding());
            rp.setUnbuf(cr.getUnbuf());
            rd = rp;
        }
        inter2.setProperty(Toolkit.PROP_SYS_DISP_INPUT, rd);
        inter2.setProperty(Toolkit.PROP_SYS_CUR_INPUT, rd);
        Object wr = inter.getProperty(Toolkit.PROP_SYS_DISP_OUTPUT);
        ConnectionWriter cw;
        if (wr instanceof ConnectionWriter && (cw = (ConnectionWriter) wr).getBuffer() != 0) {
            ConnectionWriter wp = new ConnectionWriter(new BufferedWriter(cw.getUnbuf(), BUF_SIZE));
            wp.setBuffer(BUF_SIZE);
            wp.setEncoding(cw.getEncoding());
            wp.setUnbuf(cw.getUnbuf());
            wr = wp;
        }
        inter2.setProperty(Toolkit.PROP_SYS_DISP_OUTPUT, wr);
        inter2.setProperty(Toolkit.PROP_SYS_CUR_OUTPUT, wr);
        wr = inter.getProperty(Toolkit.PROP_SYS_DISP_ERROR);
        if (wr instanceof ConnectionWriter && (cw = (ConnectionWriter) wr).getBuffer() != 0) {
            ConnectionWriter wp = new ConnectionWriter(new BufferedWriter(cw.getUnbuf(), BUF_SIZE));
            wp.setBuffer(BUF_SIZE);
            wp.setEncoding(cw.getEncoding());
            wp.setUnbuf(cw.getUnbuf());
            wr = wp;
        }
        inter2.setProperty(Toolkit.PROP_SYS_DISP_ERROR, wr);
        inter2.setProperty(Toolkit.PROP_SYS_CUR_ERROR, wr);
        inter2.setProperty(Toolkit.PROP_SYS_ATTACHED_TO, inter.getProperty(Toolkit.PROP_SYS_ATTACHED_TO));
        final CallIn callin = inter2.iterator(t);

        Thread thread = new Thread(new Runnable() {
            public void run() {
                try {
                    try {
                        callin.nextClose();
                    } catch (InterpreterMessage y) {
                        InterpreterException x = new InterpreterException(y,
                                InterpreterException.fetchStack(inter2));
                        systemDeathBreak(inter2, x);
                    } catch (InterpreterException x) {
                        systemDeathBreak(inter2, x);
                    }
                } catch (ThreadDeath x) {
                    /* */
                } catch (Throwable x) {
                    x.printStackTrace();
                }
                inter2.getController().setFence(null);
            }
        });
        inter2.getController().setFence(thread);
        return thread;
    }

    /**
     * <p>Show the death exception.</p>
     *
     * @param inter The interpreter.
     * @param x     The death exception.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    private static void systemDeathBreak(Interpreter inter, InterpreterException x)
            throws InterpreterMessage, InterpreterException {
        InterpreterMessage m;
        if ((m = x.exceptionType("error")) != null &&
                m.messageType("system_error") != null) {
            InterpreterException rest = x.causeChainRest();
            if (rest != null)
                rest.printStackTrace(inter);
        } else {
            x.printStackTrace(inter);
        }
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
    public static void sysThreadAbort(Thread t, Term m)
            throws InterruptedException {
        synchronized (t) {
            while (systemGetSignal(t) != null)
                t.wait();
            systemSetSignal(t, m);
            t.notifyAll();
        }
    }

    /**
     * <p>Set the signal of a thread.</p>
     * <p>Fail if thread has already a signal.</p>
     *
     * @param t The thread.
     * @param m The message.
     */
    public static boolean sysThreadDown(Thread t, Term m) {
        synchronized (t) {
            if (systemGetSignal(t) == null) {
                systemSetSignal(t, m);
                t.notifyAll();
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
    public static boolean sysThreadDown(Thread t, Term m, long sleep)
            throws InterruptedException {
        long when = System.currentTimeMillis() + sleep;
        synchronized (t) {
            while (systemGetSignal(t) != null & sleep > 0) {
                t.wait(sleep);
                sleep = when - System.currentTimeMillis();
            }
            if (sleep > 0) {
                systemSetSignal(t, m);
                t.notifyAll();
                return true;
            } else {
                return false;
            }
        }
    }

    /**
     * <p>Retrieve the signal.</p>
     *
     * @param t The thread.
     * @return The old signal, can be null.
     */
    private static InterpreterMessage systemGetSignal(Thread t) {
        Controller contr = Controller.currentController(t);
        if (contr == null)
            return null;
        return contr.getSignal();
    }

    /**
     * <p>Set the signal.</p>
     *
     * @param t The thread.
     * @param m The message.
     */
    private static void systemSetSignal(Thread t, Term m) {
        Controller contr = Controller.currentController(t);
        if (contr == null)
            return;
        InterpreterMessage im = new InterpreterMessage(m);
        contr.setSignal(im);
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

}
