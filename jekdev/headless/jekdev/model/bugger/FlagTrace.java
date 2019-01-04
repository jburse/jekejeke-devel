package jekdev.model.bugger;

import jekdev.reference.debug.SpecialDefault;
import jekdev.reference.inspection.SpecialFrame;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.Engine;
import jekpro.model.inter.InterfaceStack;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.ReadOpts;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.term.SkelAtom;
import matula.util.data.MapHash;

/**
 * <p>Prolog flags on development environment level.</p>
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
public final class FlagTrace extends AbstractFlag {
    public final static String OP_FLAG_UNKNOWN = "unknown"; /* ISO */
    public final static String OP_FLAG_DEBUG = "debug"; /* ISO */
    public final static String OP_FLAG_SYS_LEASH = "sys_leash";
    public final static String OP_FLAG_SYS_VISIBLE = "sys_visible";
    public final static String OP_FLAG_SYS_TLEASH = "sys_tleash";
    public final static String OP_FLAG_SYS_TVISIBLE = "sys_tvisible";
    public final static String OP_FLAG_SYS_CLAUSE_INSTRUMENT = "sys_clause_instrument";
    public final static String OP_FLAG_SYS_HEAD_WAKEUP = "sys_head_wakeup";
    public final static String OP_FLAG_SYS_SKIP_FRAME = "sys_skip_frame";
    public final static String OP_FLAG_SYS_QUERY_FRAME = "sys_query_frame";
    public final static String OP_FLAG_SYS_CLOAK = "sys_cloak";
    public final static String OP_FLAG_SYS_MAX_STACK = "sys_max_stack";

    private static final int FLAG_UNKNOWN = 0;
    private static final int FLAG_DEBUG = 1;
    private static final int FLAG_SYS_LEASH = 2;
    private static final int FLAG_SYS_VISIBLE = 3;
    private static final int FLAG_SYS_TLEASH = 4;
    private static final int FLAG_SYS_TVISIBLE = 5;
    private static final int FLAG_SYS_CLAUSE_INSTRUMENT = 6;
    private static final int FLAG_SYS_HEAD_WAKEUP = 7;
    private static final int FLAG_SYS_SKIP_FRAME = 8;
    private static final int FLAG_SYS_QUERY_FRAME = 9;
    private static final int FLAG_SYS_CLOAK = 10;
    private static final int FLAG_SYS_MAX_STACK = 11;

    /**
     * <p>Create a Prolog flag.</p>
     *
     * @param i The id of the Prolog flag.
     */
    private FlagTrace(int i) {
        super(i);
    }

    /**
     * <p>Define the prolog flags.</p>
     *
     * @return The prolog flags.
     */
    static MapHash<String, AbstractFlag> defineFlags() {
        MapHash<String, AbstractFlag> prologflags = new MapHash<String, AbstractFlag>();
        prologflags.add(OP_FLAG_UNKNOWN, new FlagTrace(FLAG_UNKNOWN));
        prologflags.add(OP_FLAG_DEBUG, new FlagTrace(FLAG_DEBUG));
        prologflags.add(OP_FLAG_SYS_LEASH, new FlagTrace(FLAG_SYS_LEASH));
        prologflags.add(OP_FLAG_SYS_VISIBLE, new FlagTrace(FLAG_SYS_VISIBLE));
        prologflags.add(OP_FLAG_SYS_TLEASH, new FlagTrace(FLAG_SYS_TLEASH));
        prologflags.add(OP_FLAG_SYS_TVISIBLE, new FlagTrace(FLAG_SYS_TVISIBLE));
        prologflags.add(OP_FLAG_SYS_CLAUSE_INSTRUMENT, new FlagTrace(FLAG_SYS_CLAUSE_INSTRUMENT));
        prologflags.add(OP_FLAG_SYS_HEAD_WAKEUP, new FlagTrace(FLAG_SYS_HEAD_WAKEUP));
        prologflags.add(OP_FLAG_SYS_SKIP_FRAME, new FlagTrace(FLAG_SYS_SKIP_FRAME));
        prologflags.add(OP_FLAG_SYS_QUERY_FRAME, new FlagTrace(FLAG_SYS_QUERY_FRAME));
        prologflags.add(OP_FLAG_SYS_CLOAK, new FlagTrace(FLAG_SYS_CLOAK));
        prologflags.add(OP_FLAG_SYS_MAX_STACK, new FlagTrace(FLAG_SYS_MAX_STACK));
        return prologflags;
    }

    /**
     * <p>Retrieve the value of this flag.</p>
     *
     * @param en The engine.
     * @return The value.
     */
    public Object getFlag(Engine en) {
        switch (id) {
            case FLAG_UNKNOWN:
                return new SkelAtom(ReadOpts.OP_VALUE_ERROR);
            case FLAG_DEBUG:
                return SpecialDefault.modeToAtom(((StoreTrace) en.store).flags & SpecialDefault.MASK_MODE_DEBG);
            case FLAG_SYS_LEASH:
                return SpecialDefault.portsToList(en, ((StoreTrace) en.store).flags >> 16);
            case FLAG_SYS_VISIBLE:
                return SpecialDefault.portsToList(en, ((StoreTrace) en.store).flags >> 24);
            case FLAG_SYS_TLEASH:
                return SpecialDefault.portsToList(en, en.visor.flags >> 16);
            case FLAG_SYS_TVISIBLE:
                return SpecialDefault.portsToList(en, en.visor.flags >> 24);
            case FLAG_SYS_CLAUSE_INSTRUMENT:
                return AbstractFlag.switchToAtom((en.store.foyer.getBits() & Foyer.MASK_STORE_NIST) == 0);
            case FLAG_SYS_HEAD_WAKEUP:
                return AbstractFlag.switchToAtom((en.store.foyer.getBits() & Foyer.MASK_STORE_NHWK) == 0);
            case FLAG_SYS_SKIP_FRAME:
                InterfaceStack frame = ((SupervisorTrace) en.visor).getSkipFrame();
                return (frame != null ? frame : new SkelAtom(AbstractFlag.OP_NULL));
            case FLAG_SYS_QUERY_FRAME:
                frame = en.visor.ref;
                return (frame != null ? frame : new SkelAtom(AbstractFlag.OP_NULL));
            case FLAG_SYS_CLOAK:
                return AbstractFlag.switchToAtom((en.visor.flags & SpecialDefault.MASK_DEBG_NOFL) == 0);
            case FLAG_SYS_MAX_STACK:
                return Integer.valueOf(en.store.getMaxStack());
            default:
                throw new IllegalArgumentException("illegal flag");
        }
    }

    /**
     * <p>Set the value of a this flag.</p>
     *
     * @param m  The value skel.
     * @param d  The value display.
     * @param en The engine.
     * @return True if flag could be changed, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean setFlag(Object m, Display d, Engine en)
            throws EngineMessage {
        try {
            switch (id) {
                case FLAG_UNKNOWN:
                    /* */
                    return false;
                case FLAG_DEBUG:
                    ((StoreTrace) en.store).setMode(SpecialDefault.atomToMode(m, d));
                    return true;
                case FLAG_SYS_LEASH:
                    ((StoreTrace) en.store).setLeash(SpecialDefault.listToPorts(m, d) << 16);
                    return true;
                case FLAG_SYS_VISIBLE:
                    ((StoreTrace) en.store).setVisible(SpecialDefault.listToPorts(m, d) << 24);
                    return true;
                case FLAG_SYS_TLEASH:
                    ((SupervisorTrace) en.visor).setThreadLeash(SpecialDefault.listToPorts(m, d) << 16);
                    return true;
                case FLAG_SYS_TVISIBLE:
                    ((SupervisorTrace) en.visor).setThreadVisible(SpecialDefault.listToPorts(m, d) << 24);
                    return true;
                case FLAG_SYS_CLAUSE_INSTRUMENT:
                    if (AbstractFlag.atomToSwitch(m, d)) {
                        en.store.foyer.resetBit(Foyer.MASK_STORE_NIST);
                    } else {
                        en.store.foyer.setBit(Foyer.MASK_STORE_NIST);
                    }
                    return true;
                case FLAG_SYS_HEAD_WAKEUP:
                    if (AbstractFlag.atomToSwitch(m, d)) {
                        en.store.foyer.resetBit(Foyer.MASK_STORE_NHWK);
                    } else {
                        en.store.foyer.setBit(Foyer.MASK_STORE_NHWK);
                    }
                    return true;
                case FLAG_SYS_SKIP_FRAME:
                    InterfaceStack frame = SpecialFrame.derefAndCastStackElement(m, d);
                    ((SupervisorTrace) en.visor).setSkipFrame(frame);
                    return true;
                case FLAG_SYS_QUERY_FRAME:
                    frame = SpecialFrame.derefAndCastStackElement(m, d);
                    en.visor.ref = frame;
                    return true;
                case FLAG_SYS_CLOAK:
                    en.visor.setIgnore(AbstractFlag.atomToSwitch(m, d));
                    return true;
                case FLAG_SYS_MAX_STACK:
                    Number num = SpecialEval.derefAndCastInteger(m, d);
                    SpecialEval.checkNotLessThanZero(num);
                    int n = SpecialEval.castIntValue(num);
                    en.store.setMaxStack(n);
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