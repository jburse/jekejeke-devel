package jekdev.reference.system;

import jekdev.model.pretty.StoreTrace;
import jekdev.reference.debug.SpecialDefault;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.ReadOpts;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.array.Types;
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
public final class FlagTrace extends AbstractFlag<Engine> {
    public final static MapHash<String, AbstractFlag<Engine>> DEFAULT
            = new MapHash<>();

    public final static String OP_UNKNOWN = "unknown"; /* ISO */
    public final static String OP_DEBUG = "debug"; /* ISO */
    public final static String OP_SYS_LEASH = "sys_leash";
    public final static String OP_SYS_VISIBLE = "sys_visible";
    public final static String OP_SYS_CLAUSE_INSTRUMENT = "sys_clause_instrument";
    public final static String OP_SYS_CLOAK = "sys_cloak";
    public final static String OP_SYS_MAX_STACK = "sys_max_stack";

    private static final int FLAG_UNKNOWN = 0;
    private static final int FLAG_DEBUG = 1;
    private static final int FLAG_SYS_LEASH = 2;
    private static final int FLAG_SYS_VISIBLE = 3;
    private static final int FLAG_SYS_CLAUSE_INSTRUMENT = 4;
    private static final int FLAG_SYS_CLOAK = 6;
    private static final int FLAG_SYS_MAX_STACK = 7;

    static {
        DEFAULT.add(OP_UNKNOWN, new FlagTrace(FLAG_UNKNOWN));
        DEFAULT.add(OP_DEBUG, new FlagTrace(FLAG_DEBUG));
        DEFAULT.add(OP_SYS_LEASH, new FlagTrace(FLAG_SYS_LEASH));
        DEFAULT.add(OP_SYS_VISIBLE, new FlagTrace(FLAG_SYS_VISIBLE));
        DEFAULT.add(OP_SYS_CLAUSE_INSTRUMENT, new FlagTrace(FLAG_SYS_CLAUSE_INSTRUMENT));
        DEFAULT.add(OP_SYS_CLOAK, new FlagTrace(FLAG_SYS_CLOAK));
        DEFAULT.add(OP_SYS_MAX_STACK, new FlagTrace(FLAG_SYS_MAX_STACK));
    }

    /**
     * <p>Create a Prolog flag.</p>
     *
     * @param i The id of the Prolog flag.
     */
    private FlagTrace(int i) {
        super(i);
    }

    /**
     * <p>Retrieve the value of this flag.</p>
     *
     * @param en The engine.
     * @return The value.
     */
    public Object getObjFlag(Engine obj, Engine en) {
        switch (id) {
            case FLAG_UNKNOWN:
                return new SkelAtom(ReadOpts.OP_VALUE_ERROR);
            case FLAG_DEBUG:
                return SpecialDefault.modeToAtom(((StoreTrace) en.store).flags &
                        SpecialDefault.MASK_MODE_DEBG);
            case FLAG_SYS_LEASH:
                return SpecialDefault.portsToList(((StoreTrace) en.store).flags >> 16);
            case FLAG_SYS_VISIBLE:
                return SpecialDefault.portsToList(((StoreTrace) en.store).flags >> 24);
            case FLAG_SYS_CLAUSE_INSTRUMENT:
                return AbstractFlag.switchToAtom((en.store.foyer.getBits() &
                        Foyer.MASK_FOYER_NIST) == 0);
            case FLAG_SYS_CLOAK:
                return AbstractFlag.switchToAtom((en.visor.flags &
                        SpecialDefault.MASK_DEBG_NOFL) == 0);
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
    public boolean setObjFlag(Engine obj, Object m, Display d, Engine en)
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
                case FLAG_SYS_CLAUSE_INSTRUMENT:
                    if (AbstractFlag.atomToSwitch(m, d)) {
                        en.store.foyer.resetBit(Foyer.MASK_FOYER_NIST);
                    } else {
                        en.store.foyer.setBit(Foyer.MASK_FOYER_NIST);
                    }
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
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
        }
    }

}