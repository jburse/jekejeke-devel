package jekdev.reference.system;

import jekdev.model.builtin.SupervisorTrace;
import jekdev.reference.debug.SpecialDefault;
import jekdev.reference.inspection.SpecialFrame;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.inter.Supervisor;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.array.Types;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.TermAtomic;
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
public final class FlagTraceThread extends AbstractFlag<Thread> {
    public final static MapHash<String, AbstractFlag<Thread>> DEFAULT =
            new MapHash<>();

    public final static String OP_SYS_TDEBUG = "sys_tdebug";
    public final static String OP_SYS_TOP_FRAME = "sys_top_frame";
    public final static String OP_SYS_THREAD_STORE = "sys_thread_store";
    public final static String OP_SYS_THREAD_LASTMOD = "sys_thread_lastmod";
    public final static String OP_SYS_TLEASH = "sys_tleash";
    public final static String OP_SYS_TVISIBLE = "sys_tvisible";
    public final static String OP_SYS_TSKIP_FRAME = "sys_tskip_frame";

    private static final int FLAG_SYS_TDEBUG = 0;
    private static final int FLAG_SYS_TOP_FRAME = 1;
    private static final int FLAG_SYS_THREAD_STORE = 2;
    private static final int FLAG_SYS_THREAD_LASTMOD = 3;
    private static final int FLAG_SYS_TLEASH = 4;
    private static final int FLAG_SYS_TVISIBLE = 5;
    private static final int FLAG_SYS_TSKIP_FRAME = 6;

    static {
        DEFAULT.add(OP_SYS_TDEBUG, new FlagTraceThread(FLAG_SYS_TDEBUG));
        DEFAULT.add(OP_SYS_TOP_FRAME, new FlagTraceThread(FLAG_SYS_TOP_FRAME));
        DEFAULT.add(OP_SYS_THREAD_STORE, new FlagTraceThread(FLAG_SYS_THREAD_STORE));
        DEFAULT.add(OP_SYS_THREAD_LASTMOD, new FlagTraceThread(FLAG_SYS_THREAD_LASTMOD));
        DEFAULT.add(OP_SYS_TLEASH, new FlagTraceThread(FLAG_SYS_TLEASH));
        DEFAULT.add(OP_SYS_TVISIBLE, new FlagTraceThread(FLAG_SYS_TVISIBLE));
        DEFAULT.add(OP_SYS_TSKIP_FRAME, new FlagTraceThread(FLAG_SYS_TSKIP_FRAME));
    }

    /**
     * <p>Create a thread flag.</p>
     *
     * @param i The id of the thread flag.
     */
    private FlagTraceThread(int i) {
        super(i);
    }

    /**
     * <p>Retrieve the value of this thread flag.</p>
     *
     * @param obj The thread.
     * @return The value.
     */
    public Object getObjFlag(Thread obj) {
        switch (id) {
            case FLAG_SYS_TDEBUG:
                Supervisor s = (Supervisor) AbstractLivestock.currentLivestock(obj);
                if (s == null) return new SkelAtom(AbstractFlag.OP_NULL);

                return SpecialDefault.modeToAtom(s.flags & SpecialDefault.MASK_MODE_DEBG);
            case FLAG_SYS_TOP_FRAME:
                s = (Supervisor) AbstractLivestock.currentLivestock(obj);
                if (s == null) return new SkelAtom(AbstractFlag.OP_NULL);

                Engine en2 = s.inuse;
                if (en2 == null) return new SkelAtom(AbstractFlag.OP_NULL);

                StackElement stack;
                try {
                    stack = StackElement.skipNoTrace(en2, en2);
                } catch (EngineMessage x) {
                    throw new RuntimeException("shouldn't happen", x);
                } catch (EngineException x) {
                    throw new RuntimeException("shouldn't happen", x);
                }
                return (stack != null ? stack : new SkelAtom(AbstractFlag.OP_NULL));
            case FLAG_SYS_THREAD_STORE:
                s = (Supervisor) AbstractLivestock.currentLivestock(obj);
                if (s == null) return new SkelAtom(AbstractFlag.OP_NULL);

                en2 = s.inuse;
                if (en2 == null) return new SkelAtom(AbstractFlag.OP_NULL);

                return en2.store.proxy;
            case FLAG_SYS_THREAD_LASTMOD:
                s = (Supervisor) AbstractLivestock.currentLivestock(obj);
                if (s == null) return Integer.valueOf(0);

                return TermAtomic.normBigInteger(((SupervisorTrace) s).lastmod);
            case FLAG_SYS_TLEASH:
                s = (Supervisor) AbstractLivestock.currentLivestock(obj);
                if (s == null) return new SkelAtom(AbstractFlag.OP_NULL);

                return SpecialDefault.portsToList(s.flags >> 16);
            case FLAG_SYS_TVISIBLE:
                s = (Supervisor) AbstractLivestock.currentLivestock(obj);
                if (s == null) return new SkelAtom(AbstractFlag.OP_NULL);

                return SpecialDefault.portsToList(s.flags >> 24);
            case FLAG_SYS_TSKIP_FRAME:
                s = (Supervisor) AbstractLivestock.currentLivestock(obj);
                if (s == null) return new SkelAtom(AbstractFlag.OP_NULL);

                stack = ((SupervisorTrace) s).skipframe;
                return (stack != null ? stack : new SkelAtom(AbstractFlag.OP_NULL));
            default:
                throw new IllegalArgumentException("illegal flag");
        }
    }

    /**
     * <p>Set the value of a this flag.</p>
     *
     * @param m   The value skel.
     * @param d   The value display.
     * @param obj The thread.
     * @return True if flag could be changed, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean setObjFlag(Thread obj, Object m, Display d)
            throws EngineMessage {
        try {
            switch (id) {
                case FLAG_SYS_TDEBUG:
                    Supervisor s = (Supervisor) AbstractLivestock.currentLivestock(obj);
                    if (s == null) return true;

                    ((SupervisorTrace) s).setThreadMode(SpecialDefault.atomToMode(m, d));
                    return true;
                case FLAG_SYS_TOP_FRAME:
                    /* can't modify */
                    return false;
                case FLAG_SYS_THREAD_STORE:
                    /* can't modify */
                    return false;
                case FLAG_SYS_THREAD_LASTMOD:
                    s = (Supervisor) AbstractLivestock.currentLivestock(obj);
                    if (s == null) return true;

                    Number num = SpecialEval.derefAndCastInteger(m, d);
                    ((SupervisorTrace) s).lastmod = SpecialEval.castLongValue(num);
                    return true;
                case FLAG_SYS_TLEASH:
                    s = (Supervisor) AbstractLivestock.currentLivestock(obj);
                    if (s == null) return true;

                    ((SupervisorTrace) s).setThreadLeash(SpecialDefault.listToPorts(m, d) << 16);
                    return true;
                case FLAG_SYS_TVISIBLE:
                    s = (Supervisor) AbstractLivestock.currentLivestock(obj);
                    if (s == null) return true;

                    ((SupervisorTrace) s).setThreadVisible(SpecialDefault.listToPorts(m, d) << 24);
                    return true;
                case FLAG_SYS_TSKIP_FRAME:
                    s = (Supervisor) AbstractLivestock.currentLivestock(obj);
                    if (s == null) return true;

                    StackElement stack = SpecialFrame.derefAndCastStackElement(m, d);
                    ((SupervisorTrace) s).skipframe = stack;
                    return true;
                default:
                    throw new IllegalArgumentException("illegal flag");
            }
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
        }
    }

}