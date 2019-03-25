package jekdev.model.bugger;

import jekdev.reference.debug.SpecialDefault;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.inter.StackElement;
import jekpro.model.inter.Supervisor;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.call.Controller;
import jekpro.tools.call.Interpreter;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.TermAtomic;
import matula.util.data.MapHash;

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
public final class FlagTraceThread extends AbstractFlag<Thread> {
    public final static String OP_FLAG_SYS_TDEBUG = "sys_tdebug";
    public final static String OP_FLAG_SYS_TOP_FRAME = "sys_top_frame";
    public final static String OP_FLAG_SYS_THREAD_STORE = "sys_thread_store";
    public final static String OP_FLAG_SYS_THREAD_LASTMOD = "sys_thread_lastmod";

    private static final int FLAG_SYS_TDEBUG = 0;
    private static final int FLAG_SYS_TOP_FRAME = 1;
    private static final int FLAG_SYS_THREAD_STORE = 2;
    private static final int FLAG_SYS_THREAD_LASTMOD = 3;

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
    static MapHash<String, AbstractFlag<Thread>> defineThreadFlags() {
        MapHash<String, AbstractFlag<Thread>> threadflags = new MapHash<String, AbstractFlag<Thread>>();
        threadflags.add(OP_FLAG_SYS_TDEBUG, new FlagTraceThread(FLAG_SYS_TDEBUG));
        threadflags.add(OP_FLAG_SYS_TOP_FRAME, new FlagTraceThread(FLAG_SYS_TOP_FRAME));
        threadflags.add(OP_FLAG_SYS_THREAD_STORE, new FlagTraceThread(FLAG_SYS_THREAD_STORE));
        threadflags.add(OP_FLAG_SYS_THREAD_LASTMOD, new FlagTraceThread(FLAG_SYS_THREAD_LASTMOD));
        return threadflags;
    }

    /**
     * <p>Retrieve the value of this thread flag.</p>
     *
     * @param t  The thread.
     * @param en The engine.
     * @return The value.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public Object getObjFlag(Thread t, Engine en)
            throws EngineException, EngineMessage {
        switch (id) {
            case FLAG_SYS_TDEBUG:
                Controller contr = Controller.currentController(t);
                if (contr == null) return new SkelAtom(AbstractFlag.OP_NULL);

                Supervisor s = (Supervisor) contr.getVisor();
                return SpecialDefault.modeToAtom(s.flags & SpecialDefault.MASK_MODE_DEBG);
            case FLAG_SYS_TOP_FRAME:
                contr = Controller.currentController(t);
                if (contr == null) return new SkelAtom(AbstractFlag.OP_NULL);

                Interpreter inter = contr.getInuse();
                if (inter == null) return new SkelAtom(AbstractFlag.OP_NULL);

                Engine en2 = (Engine) inter.getEngine();
                StackElement stack = StackElement.skipNoTrace(en2, en);
                return (stack != null ? stack : new SkelAtom(AbstractFlag.OP_NULL));
            case FLAG_SYS_THREAD_STORE:
                contr = Controller.currentController(t);
                if (contr == null) return new SkelAtom(AbstractFlag.OP_NULL);

                inter = contr.getInuse();
                if (inter == null) return new SkelAtom(AbstractFlag.OP_NULL);

                return inter.getKnowledgebase();
            case FLAG_SYS_THREAD_LASTMOD:
                contr = Controller.currentController(t);
                if (contr == null) return Integer.valueOf(0);

                s = (Supervisor) contr.getVisor();
                return TermAtomic.normBigInteger(((SupervisorTrace) s).getLastModified());
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
    public boolean setObjFlag(Thread t, Object m, Display d,
                              Engine en)
            throws EngineMessage {
        try {
            switch (id) {
                case FLAG_SYS_TDEBUG:
                    Controller contr = Controller.currentController(t);
                    if (contr == null) return true;

                    Supervisor s = (Supervisor) contr.getVisor();
                    ((SupervisorTrace) s).setThreadMode(SpecialDefault.atomToMode(m, d));
                    return true;
                case FLAG_SYS_TOP_FRAME:
                    /* can't modify */
                    return false;
                case FLAG_SYS_THREAD_STORE:
                    /* can't modify */
                    return false;
                case FLAG_SYS_THREAD_LASTMOD:
                    contr = Controller.currentController(t);
                    if (contr == null) return true;

                    s = (Supervisor) contr.getVisor();
                    Number num = SpecialEval.derefAndCastInteger(m, d);
                    ((SupervisorTrace) s).setLastModified(SpecialEval.castLongValue(num));
                    return true;
                default:
                    throw new IllegalArgumentException("illegal flag");
            }
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

}