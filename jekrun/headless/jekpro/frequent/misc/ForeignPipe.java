package jekpro.frequent.misc;

import jekpro.tools.call.Interpreter;
import jekpro.tools.term.Term;
import matula.util.misc.AbstractPipe;
import matula.util.misc.Queue;

/**
 * The foreign predicates for the module misc/queue.
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
public final class ForeignPipe {

    /**
     * <p>Enqueue a copy of a term in a pipe. If the pipe has not
     * reached max size, the copy is enqueued. Otherwise the current
     * thread waits until the pipe is depleted. There is no
     * assurance of fairness.</p>
     *
     * @param inter The interpreter.
     * @param q     The pipe.
     * @param t     The term.
     * @throws InterruptedException Thread was interrupted.
     */
    public static void sysPipePut(Interpreter inter, AbstractPipe q, Term t)
            throws InterruptedException {
        t = Term.copyTermWrapped(inter, t);
        q.put(t);
    }

    /**
     * <p>Enqueue a copy of a term in a pipe. If the pipe has not
     * reached max size, the copy is enqueued. Otherwise we
     * return false. There is no assurance of fairness.</p>
     *
     * @param inter The interpreter.
     * @param q     The bounded queue.
     * @param t     The term.
     * @throws InterruptedException Thread was interrupted.
     */
    public static boolean sysPipeOffer(Interpreter inter, Queue q, Term t)
            throws InterruptedException {
        t = Term.copyTermWrapped(inter, t);
        return q.offer(t);
    }

    /**
     * <p>Enqueue a copy of a term in a pipe with a timeout. If
     * the pipe has not reached max size, the copy is enqueued.
     * Otherwise the current thread waits maximum timeout time until
     * the pipe is depleted. There is no assurance of fairness.</p>
     *
     * @param inter The interpreter.
     * @param q     The bounded queue.
     * @param t     The term.
     * @param sleep The timeout.
     * @return True if the term was enqueued, otherwise false.
     * @throws InterruptedException Thread was interrupted.
     */
    public static boolean sysPipeOffer(Interpreter inter, Queue q,
                                       Term t, long sleep)
            throws InterruptedException {
        t = Term.copyTermWrapped(inter, t);
        return q.offer(t, sleep);
    }

}