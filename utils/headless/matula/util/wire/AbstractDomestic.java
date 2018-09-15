package matula.util.wire;

/**
 * <p>This class provides an abstract domestic.</p>
 * </p>
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
public abstract class AbstractDomestic {
    public final static int MASK_LIVESTOCK_NOSG = 0x00000001;

    public int flags;
    public Object signal;
    public Thread thread;
    public Object source;

    /**
     * <p>Set the mask.</p>
     *
     * @param f The new mask.
     * @return The old mask.
     */
    public final boolean setMask(boolean f) {
        synchronized (this) {
            boolean h = (flags & MASK_LIVESTOCK_NOSG) == 0;
            if (f) {
                flags &= ~MASK_LIVESTOCK_NOSG;
                if (signal != null && thread != null)
                    thread.interrupt();
            } else {
                if (signal != null && thread != null) {
                    if (thread != Thread.currentThread())
                        throw new RuntimeException("not owner");
                    Thread.interrupted();
                }
                flags |= MASK_LIVESTOCK_NOSG;
            }
            return h;
        }
    }

    /**
     * <p>Set the signal.</p>
     *
     * @param m The signal.
     * @return The old signal.
     */
    public final Object setSignal(Object m) {
        synchronized (this) {
            Object h = signal;
            if (m != null) {
                signal = m;
                if ((flags & MASK_LIVESTOCK_NOSG) == 0
                        && thread != null)
                    thread.interrupt();
            } else {
                if ((flags & MASK_LIVESTOCK_NOSG) == 0
                        && thread != null) {
                    if (thread != Thread.currentThread())
                        throw new RuntimeException("not owner");
                    Thread.interrupted();
                }
                signal = null;
            }
            return h;
        }
    }

    /**
     * <p>Set the thread.</p>
     *
     * @param t The thread.
     */
    public final void setThread(Thread t) {
        synchronized (this) {
            if (t != null) {
                thread = t;
                if ((flags & MASK_LIVESTOCK_NOSG) == 0
                        && signal != null)
                    thread.interrupt();
            } else {
                if ((flags & MASK_LIVESTOCK_NOSG) == 0
                        && signal != null) {
                    if (thread != Thread.currentThread())
                        throw new RuntimeException("not owner");
                    Thread.interrupted();
                }
                thread = null;
            }
        }
    }

}