package jekpro.frequent.system;

import jekpro.tools.call.*;
import jekpro.tools.term.Term;
import matula.util.misc.Alarm;
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
        Interpreter inter2 = inter.getKnowledgebase().iterable();
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
        CallIn callin = inter2.iterator(t);

        Thread thread = new Thread(callin);
        inter2.getController().setFence(thread);
        return thread;
    }

    /**
     * <p>Abort a thread with a message.</p>
     *
     * @param t The thread.
     * @param m The message.
     */
    public static void sysThreadAbort(Thread t, Term m) {
        Controller contr = Controller.currentController(t);
        if (contr == null)
            return;
        InterpreterMessage im = new InterpreterMessage(m);
        contr.setSignal(im);
    }

    /**
     * <p>Wait till a thread has terminated or time-out.</p>
     *
     * @param t     The thread.
     * @param sleep The time-out.
     * @return True if thread is terminated, otherwise false.
     * @throws InterruptedException Join was interrupted.
     */
    public static boolean sysThreadCombine(Thread t, long sleep)
            throws InterruptedException {
        t.join(sleep);
        return !t.isAlive();
    }

}
