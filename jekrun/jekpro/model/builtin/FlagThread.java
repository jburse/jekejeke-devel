package jekpro.model.builtin;

import jekpro.model.inter.Supervisor;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelVar;
import matula.util.data.MapHash;
import matula.util.wire.AbstractLivestock;

/**
 * <p>Thread flags on runtime library level.</p>
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
public final class FlagThread extends AbstractFlag<Thread> {
    public final static int MASK_MODE_PRMT = 0x0000F000;

    public final static int MASK_PRMT_PROF = 0x00000000;
    public final static int MASK_PRMT_PCUT = 0x00001000;
    public final static int MASK_PRMT_PDBG = 0x00002000;
    public final static int MASK_PRMT_PRON = 0x00003000;

    public final static String OP_SYS_THREAD_NAME = "sys_thread_name";
    public final static String OP_SYS_THREAD_STATE = "sys_thread_state";
    public final static String OP_SYS_THREAD_GROUP = "sys_thread_group";
    public final static String OP_SYS_TPROMPT = "sys_tprompt";
    private final static String OP_SYS_PRINT_MAP = "sys_print_map";

    public final static String OP_ANSWER_CUT = "answer_cut";
    public final static String OP_ASK_DEBUG = "ask_debug";

    private static final int FLAG_SYS_THREAD_NAME = 0;
    private static final int FLAG_SYS_THREAD_STATE = 1;
    private static final int FLAG_SYS_THREAD_GROUP = 2;
    private static final int FLAG_SYS_TPROMPT = 3;
    private static final int FLAG_SYS_PRINT_MAP = 4;

    public final static MapHash<String, AbstractFlag<Thread>> DEFAULT
            = new MapHash<>();

    static {
        DEFAULT.add(OP_SYS_THREAD_NAME, new FlagThread(FLAG_SYS_THREAD_NAME));
        DEFAULT.add(OP_SYS_THREAD_STATE, new FlagThread(FLAG_SYS_THREAD_STATE));
        DEFAULT.add(OP_SYS_THREAD_GROUP, new FlagThread(FLAG_SYS_THREAD_GROUP));
        DEFAULT.add(OP_SYS_TPROMPT, new FlagThread(FLAG_SYS_TPROMPT));
        DEFAULT.add(OP_SYS_PRINT_MAP, new FlagThread(FLAG_SYS_PRINT_MAP));
    }

    /**
     * <p>Create a thread flag.</p>
     *
     * @param i The id of the thread flag.
     */
    private FlagThread(int i) {
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
            case FLAG_SYS_THREAD_NAME:
                return obj.getName();
            case FLAG_SYS_THREAD_STATE:
                return obj.getState().name();
            case FLAG_SYS_THREAD_GROUP:
                return obj.getThreadGroup();
            case FLAG_SYS_TPROMPT:
                Supervisor s = (Supervisor) AbstractLivestock.currentLivestock(obj);
                if (s == null) return new SkelAtom(AbstractFlag.OP_NULL);

                return promptToAtom(s.flags & MASK_MODE_PRMT);
            case FLAG_SYS_PRINT_MAP:
                s = (Supervisor) AbstractLivestock.currentLivestock(obj);
                if (s == null) return new SkelAtom(AbstractFlag.OP_NULL);

                return s.printmap;
            default:
                throw new IllegalArgumentException("illegal flag");
        }
    }

    /**
     * <p>Set the value of a this flag.</p>
     *
     * @param obj The thread.
     * @param m   The value skel.
     * @param d   The value display.
     * @return True if flag could be changed, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean setObjFlag(Thread obj, Object m, Display d)
            throws EngineMessage {
        switch (id) {
            case FLAG_SYS_THREAD_NAME:
                String name = SpecialUniv.derefAndCastString(m, d);
                obj.setName(name);
                return true;
            case FLAG_SYS_THREAD_STATE:
                /* can't modify */
                return false;
            case FLAG_SYS_THREAD_GROUP:
                /* can't modify */
                return false;
            case FLAG_SYS_TPROMPT:
                Supervisor s = (Supervisor) AbstractLivestock.currentLivestock(obj);
                if (s == null) return true;

                s.setThreadPrompt(atomToPrompt(m, d));
                return true;
            case FLAG_SYS_PRINT_MAP:
                s = (Supervisor) AbstractLivestock.currentLivestock(obj);
                if (s == null) return true;

                BindUniv b;
                while (m instanceof SkelVar &&
                        (b = d.bind[((SkelVar) m).id]).display != null) {
                    m = b.skel;
                    d = b.display;
                }
                s.printmap = AbstractTerm.createMolec(m, d);
                return true;
            default:
                throw new IllegalArgumentException("illegal flag");
        }
    }

    /*******************************************************************/
    /* Prompt Conversion                                               */
    /*******************************************************************/

    /**
     * <p>Convert a prompt mode to an atom.</p>
     *
     * @param m The prompt mode.
     * @return The atom.
     */
    public static Object promptToAtom(int m) {
        switch (m) {
            case MASK_PRMT_PROF:
                return new SkelAtom(OP_OFF);
            case MASK_PRMT_PCUT:
                return new SkelAtom(OP_ANSWER_CUT);
            case MASK_PRMT_PDBG:
                return new SkelAtom(OP_ASK_DEBUG);
            case MASK_PRMT_PRON:
                return new SkelAtom(OP_ON);
            default:
                throw new IllegalArgumentException("illegal mode");
        }
    }

    /**
     * <p>Convert an atom to a prompt mode.</p>
     *
     * @param t The atom skeleton.
     * @param d The atom display.
     * @return The prompt mode.
     */
    public static int atomToPrompt(Object t, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(t, d);
        if (fun.equals(OP_OFF)) {
            return MASK_PRMT_PROF;
        } else if (fun.equals(OP_ANSWER_CUT)) {
            return MASK_PRMT_PCUT;
        } else if (fun.equals(OP_ASK_DEBUG)) {
            return MASK_PRMT_PDBG;
        } else if (fun.equals(OP_ON)) {
            return MASK_PRMT_PRON;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    "prompt_mode", t), d);
        }
    }

}