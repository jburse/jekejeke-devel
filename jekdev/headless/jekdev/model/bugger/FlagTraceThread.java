package jekdev.model.bugger;

import jekdev.reference.debug.SpecialDefault;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Supervisor;
import jekpro.model.molec.BindCount;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.term.SkelAtom;
import matula.util.data.MapHash;
import matula.util.wire.AbstractLivestock;

/**
 * <p>Thread flags on development environment level.</p>
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
public final class FlagTraceThread extends AbstractFlag {
    public final static String OP_FLAG_SYS_TDEBUG = "sys_tdebug";

    private static final int FLAG_SYS_TDEBUG = 0;

    /**
     * <p>Create a thread flag.</p>
     *
     * @param i The id of the thread flag.
     */
    private FlagTraceThread(int i) {
        super(i);
    }

    /**
     * <p>Define the thread flags.</p>
     *
     * @return The thread flags.
     */
    static MapHash<String, AbstractFlag> defineThreadFlags() {
        MapHash<String, AbstractFlag> threadflags = new MapHash<String, AbstractFlag>();
        threadflags.add(OP_FLAG_SYS_TDEBUG, new FlagTraceThread(FLAG_SYS_TDEBUG));
        return threadflags;
    }

    /**
     * <p>Retrieve the value of this thread flag.</p>
     *
     * @param t  The thread.
     * @param en The engine.
     * @return The value.
     */
    public Object getThreadFlag(Thread t, Engine en) {
        switch (id) {
            case FLAG_SYS_TDEBUG:
                Supervisor s = (Supervisor) AbstractLivestock.currentLivestock(t);
                if (s == null) return new SkelAtom("null");
                return SpecialDefault.modeToAtom(s.flags & SpecialDefault.MASK_MODE_DEBG);
            default:
                throw new IllegalArgumentException("illegal flag");
        }
    }

    /**
     * <p>Set the value of a this flag.</p>
     *
     * @param m  The value skel.
     * @param d  The value display.
     * @param t  The thread.
     * @param en The engine.
     * @return True if flag could be changed, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean setThreadFlag(Object m, BindCount[] d,
                                 Thread t, Engine en)
            throws EngineMessage {
        switch (id) {
            case FLAG_SYS_TDEBUG:
                SupervisorTrace s = (SupervisorTrace) AbstractLivestock.currentLivestock(t);
                if (s == null) return true;
                s.setThreadMode(SpecialDefault.atomToMode(m, d));
                return true;
            default:
                throw new IllegalArgumentException("illegal flag");
        }
    }

}