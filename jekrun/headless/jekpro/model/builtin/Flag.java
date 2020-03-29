package jekpro.model.builtin;

import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.ReadOpts;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.TermAtomic;
import matula.util.data.AbstractMap;
import matula.util.data.MapHash;

import java.util.Random;

/**
 * <p>Prolog flags on runtime library level.</p>
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
public final class Flag extends AbstractFlag<Engine> {
    public final static MapHash<String, AbstractFlag<Engine>> DEFAULT
            = new MapHash<String, AbstractFlag<Engine>>();

    private final static String OP_SYS_BODY_VARIABLE = "sys_body_variable";
    private final static String OP_SYS_STACK_FRAME = "sys_stack_frame";
    private final static String OP_SYS_HEAD_VARIABLE = "sys_head_variable";
    private final static String OP_SYS_CLAUSE_EXPAND = "sys_clause_expand";
    private final static String OP_SYS_CLAUSE_INDEX = "sys_clause_index";
    public final static String OP_DOUBLE_QUOTES = "double_quotes"; /* ISO */
    public final static String OP_BACK_QUOTES = "back_quotes";
    private final static String OP_MAX_CODE = "max_code";
    private final static String OP_SYS_BREAK_LEVEL = "sys_break_level";
    private final static String OP_SYS_PRINT_MAP = "sys_print_map";
    private final static String OP_SYS_LAST_PRED = "sys_last_pred";
    public final static String OP_SINGLE_QUOTES = "single_quotes";
    public final static String OP_SYS_ACT_STATUS = "sys_act_status";
    private final static String OP_SYS_VARIABLES = "sys_variables";
    private final static String OP_SYS_CHOICES = "sys_choices";
    private final static String OP_SYS_RANDOM = "sys_random";
    private final static String OP_SYS_TIMEOUT = "sys_timeout";
    public final static String OP_STYLE_CHECK = "style_check";
    private final static String OP_DIALECT = "dialect";

    private static final int FLAG_SYS_BODY_VARIABLE = 0;
    private static final int FLAG_SYS_STACK_FRAME = 1;
    private static final int FLAG_SYS_HEAD_VARIABLE = 2;
    private static final int FLAG_SYS_CLAUSE_EXPAND = 3;
    private static final int FLAG_SYS_CLAUSE_INDEX = 4;
    private static final int FLAG_DOUBLE_QUOTES = 5;
    private static final int FLAG_BACK_QUOTES = 6;
    private static final int FLAG_MAX_CODE = 7;
    private static final int FLAG_SYS_BREAK_LEVEL = 8;
    private static final int FLAG_SYS_PRINT_MAP = 9;
    private static final int FLAG_SYS_LAST_PRED = 10;
    private static final int FLAG_SYS_ACT_STATUS = 11;
    private static final int FLAG_SINGLE_QUOTES = 12;
    private static final int FLAG_SYS_VARIABLES = 13;
    private static final int FLAG_SYS_CHOICES = 14;
    private static final int FLAG_SYS_RANDOM = 15;
    private static final int FLAG_SYS_TIMEOUT = 16;
    private static final int FLAG_STYLE_CHECK = 17;
    private static final int FLAG_DIALECT = 18;

    static {
        DEFAULT.add(OP_SYS_BODY_VARIABLE, new Flag(FLAG_SYS_BODY_VARIABLE));
        DEFAULT.add(OP_SYS_STACK_FRAME, new Flag(FLAG_SYS_STACK_FRAME));
        DEFAULT.add(OP_SYS_HEAD_VARIABLE, new Flag(FLAG_SYS_HEAD_VARIABLE));
        DEFAULT.add(OP_SYS_CLAUSE_EXPAND, new Flag(FLAG_SYS_CLAUSE_EXPAND));
        DEFAULT.add(OP_SYS_CLAUSE_INDEX, new Flag(FLAG_SYS_CLAUSE_INDEX));
        DEFAULT.add(OP_DOUBLE_QUOTES, new Flag(FLAG_DOUBLE_QUOTES));
        DEFAULT.add(OP_BACK_QUOTES, new Flag(FLAG_BACK_QUOTES));
        DEFAULT.add(OP_MAX_CODE, new Flag(FLAG_MAX_CODE));
        DEFAULT.add(OP_SYS_BREAK_LEVEL, new Flag(FLAG_SYS_BREAK_LEVEL));
        DEFAULT.add(OP_SYS_PRINT_MAP, new Flag(FLAG_SYS_PRINT_MAP));
        DEFAULT.add(OP_SYS_LAST_PRED, new Flag(FLAG_SYS_LAST_PRED));
        DEFAULT.add(OP_SYS_ACT_STATUS, new Flag(FLAG_SYS_ACT_STATUS));
        DEFAULT.add(OP_SINGLE_QUOTES, new Flag(FLAG_SINGLE_QUOTES));
        DEFAULT.add(OP_SYS_VARIABLES, new Flag(FLAG_SYS_VARIABLES));
        DEFAULT.add(OP_SYS_CHOICES, new Flag(FLAG_SYS_CHOICES));
        DEFAULT.add(OP_SYS_RANDOM, new Flag(FLAG_SYS_RANDOM));
        DEFAULT.add(OP_SYS_TIMEOUT, new Flag(FLAG_SYS_TIMEOUT));
        DEFAULT.add(OP_STYLE_CHECK, new Flag(FLAG_STYLE_CHECK));
        DEFAULT.add(OP_DIALECT, new Flag(FLAG_DIALECT));

    }

    /**
     * <p>Create a Prolog flag.</p>
     *
     * @param i The id of the Prolog flag.
     */
    private Flag(int i) {
        super(i);
    }

    /**
     * <p>Retrieve the value of this Prolog flag.</p>
     *
     * @param en The engine.
     * @return The value.
     */
    public Object getObjFlag(Engine obj, Engine en) {
        switch (id) {
            case FLAG_SYS_BODY_VARIABLE:
                return AbstractFlag.switchToAtom((en.store.foyer.getBits() &
                        Foyer.MASK_FOYER_NBDY) == 0);
            case FLAG_SYS_STACK_FRAME:
                return AbstractFlag.switchToAtom((en.store.foyer.getBits() &
                        Foyer.MASK_FOYER_NSTK) == 0);
            case FLAG_SYS_HEAD_VARIABLE:
                return AbstractFlag.switchToAtom((en.store.foyer.getBits() &
                        Foyer.MASK_FOYER_NHED) == 0);
            case FLAG_SYS_CLAUSE_EXPAND:
                return AbstractFlag.switchToAtom((en.store.foyer.getBits() &
                        Foyer.MASK_FOYER_CEXP) != 0);
            case FLAG_SYS_CLAUSE_INDEX:
                return AbstractFlag.switchToAtom((en.store.foyer.getBits() &
                        Foyer.MASK_FOYER_NIDX) == 0);
            case FLAG_DOUBLE_QUOTES:
                return ReadOpts.utilToAtom(en.visor.peekStack().utildouble);
            case FLAG_BACK_QUOTES:
                return ReadOpts.utilToAtom(en.visor.peekStack().utilback);
            case FLAG_MAX_CODE:
                return Integer.valueOf(Character.MAX_CODE_POINT);
            case FLAG_SYS_BREAK_LEVEL:
                return Integer.valueOf(en.visor.breaklevel);
            case FLAG_SYS_PRINT_MAP:
                return en.visor.printmap;
            case FLAG_SYS_LAST_PRED:
                StoreKey sk = en.visor.lastsk;
                return (sk != null ? StoreKey.storeKeyToSkel(sk) : AbstractFlag.OP_NULL);
            case FLAG_SYS_ACT_STATUS:
                return new SkelAtom(en.store.foyer.getError());
            case FLAG_SINGLE_QUOTES:
                return ReadOpts.utilToAtom(en.visor.peekStack().utilsingle);
            case FLAG_SYS_VARIABLES:
                AbstractMap<BindUniv, Integer> map = en.visor.varmap;
                return Integer.valueOf(map.totalSize());
            case FLAG_SYS_CHOICES:
                return Integer.valueOf(en.number);
            case FLAG_SYS_RANDOM:
                return en.store.foyer.random;
            case FLAG_SYS_TIMEOUT:
                return TermAtomic.normBigInteger(en.store.foyer.timeout);
            case FLAG_STYLE_CHECK:
                return AbstractFlag.switchToAtom((en.visor.peekStack().getBits() &
                        AbstractSource.MASK_SRC_NSTY) == 0);
            case FLAG_DIALECT:
                return en.store.foyer.ATOM_JEKEJEKE;
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
                case FLAG_SYS_BODY_VARIABLE:
                    if (AbstractFlag.atomToSwitch(m, d)) {
                        en.store.foyer.resetBit(Foyer.MASK_FOYER_NBDY);
                    } else {
                        en.store.foyer.setBit(Foyer.MASK_FOYER_NBDY);
                    }
                    return true;
                case FLAG_SYS_STACK_FRAME:
                    if (AbstractFlag.atomToSwitch(m, d)) {
                        en.store.foyer.resetBit(Foyer.MASK_FOYER_NSTK);
                    } else {
                        en.store.foyer.setBit(Foyer.MASK_FOYER_NSTK);
                    }
                    return true;
                case FLAG_SYS_HEAD_VARIABLE:
                    if (AbstractFlag.atomToSwitch(m, d)) {
                        en.store.foyer.resetBit(Foyer.MASK_FOYER_NHED);
                    } else {
                        en.store.foyer.setBit(Foyer.MASK_FOYER_NHED);
                    }
                    return true;
                case FLAG_SYS_CLAUSE_EXPAND:
                    if (AbstractFlag.atomToSwitch(m, d)) {
                        en.store.foyer.setBit(Foyer.MASK_FOYER_CEXP);
                    } else {
                        en.store.foyer.resetBit(Foyer.MASK_FOYER_CEXP);
                    }
                    return true;
                case FLAG_SYS_CLAUSE_INDEX:
                    if (AbstractFlag.atomToSwitch(m, d)) {
                        en.store.foyer.resetBit(Foyer.MASK_FOYER_NIDX);
                    } else {
                        en.store.foyer.setBit(Foyer.MASK_FOYER_NIDX);
                    }
                    return true;
                case FLAG_DOUBLE_QUOTES:
                    en.visor.peekStack().utildouble = (byte) ReadOpts.atomToUtil(m, d);
                    return true;
                case FLAG_BACK_QUOTES:
                    en.visor.peekStack().utilback = (byte) ReadOpts.atomToUtil(m, d);
                    return true;
                case FLAG_MAX_CODE:
                    /* can't modify */
                    return false;
                case FLAG_SYS_BREAK_LEVEL:
                    Number num = SpecialEval.derefAndCastInteger(m, d);
                    en.visor.breaklevel = SpecialEval.castIntValue(num);
                    return true;
                case FLAG_SYS_PRINT_MAP:
                    en.skel = m;
                    en.display = d;
                    en.deref();
                    en.visor.printmap = AbstractTerm.createMolec(en.skel, en.display);
                    return true;
                case FLAG_SYS_LAST_PRED:
                    en.skel = m;
                    en.display = d;
                    en.deref();
                    if (en.skel instanceof SkelAtom &&
                            (AbstractFlag.OP_NULL.equals(((SkelAtom) en.skel).fun))) {
                        en.visor.lastsk = null;
                    } else {
                        en.visor.lastsk = StoreKey.propToStoreKey(m, d, en);
                    }
                    return true;
                case FLAG_SYS_ACT_STATUS:
                    /* can't modify */
                    return false;
                case FLAG_SINGLE_QUOTES:
                    en.visor.peekStack().utilsingle = (byte) ReadOpts.atomToUtil(m, d);
                    return true;
                case FLAG_SYS_VARIABLES:
                    /* can't modify */
                    return false;
                case FLAG_SYS_CHOICES:
                    /* can't modify */
                    return false;
                case FLAG_SYS_RANDOM:
                    m = SpecialUniv.derefAndCastRef(m, d);
                    if (m instanceof Random) {
                        en.store.foyer.random = (Random) m;
                    } else {
                        throw new EngineMessage(EngineMessage.domainError(
                                EngineMessage.OP_DOMAIN_FLAG_VALUE, m));
                    }
                    return true;
                case FLAG_SYS_TIMEOUT:
                    num = SpecialEval.derefAndCastInteger(m, d);
                    en.store.foyer.timeout = SpecialEval.castLongValue(num);
                    return true;
                case FLAG_STYLE_CHECK:
                    if (AbstractFlag.atomToSwitch(m, d)) {
                        en.visor.peekStack().resetBit(AbstractSource.MASK_SRC_NSTY);
                    } else {
                        en.visor.peekStack().setBit(AbstractSource.MASK_SRC_NSTY);
                    }
                    return true;
                case FLAG_DIALECT:
                    /* can't modify */
                    return false;
                default:
                    throw new IllegalArgumentException("illegal flag");
            }
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

}