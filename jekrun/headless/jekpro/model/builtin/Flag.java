package jekpro.model.builtin;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.ReadOpts;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;
import matula.util.data.ListArray;
import matula.util.data.MapHash;

import java.util.Properties;
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
public final class Flag extends AbstractFlag {
    public final static String OP_FLAG_DOUBLE_QUOTES = "double_quotes"; /* ISO */
    public final static String OP_FLAG_BACK_QUOTES = "back_quotes";
    public final static String OP_FLAG_SINGLE_QUOTES = "single_quotes";
    public final static String OP_FLAG_SYS_ACT_STATUS = "sys_act_status";

    private static final int FLAG_SYS_BODY_VARIABLE = 0;
    private static final int FLAG_SYS_STACK_FRAME = 1;
    private static final int FLAG_SYS_HEAD_VARIABLE = 2;
    private static final int FLAG_SYS_BODY_CONVERT = 3;
    private static final int FLAG_SYS_CLAUSE_EXPAND = 4;
    private static final int FLAG_SYS_CLAUSE_INDEX = 5;
    private static final int FLAG_BOUNDED = 6;
    private static final int FLAG_INTEGER_ROUNDING_FUNCTION = 7;
    private static final int FLAG_CHAR_CONVERSION = 8;
    private static final int FLAG_MAX_ARITY = 9;
    private static final int FLAG_DOUBLE_QUOTES = 10;
    private static final int FLAG_BACK_QUOTES = 11;
    private static final int FLAG_MAX_CODE = 12;
    private static final int FLAG_SYS_BREAK_LEVEL = 13;
    private static final int FLAG_SYS_LAST_PRED = 14;
    private static final int FLAG_SYS_ACT_STATUS = 15;
    private static final int FLAG_SINGLE_QUOTES = 16;
    private static final int FLAG_SYS_VARIABLES = 17;
    private static final int FLAG_SYS_CHOICES = 18;
    private static final int FLAG_DIALECT = 19;
    private static final int FLAG_VERSION_DATA = 20;
    private static final int FLAG_SYS_RANDOM = 21;
    private static final int FLAG_SYS_TIMEOUT = 22;

    /**
     * <p>Create a Prolog flag.</p>
     *
     * @param i The id of the Prolog flag.
     */
    private Flag(int i) {
        super(i);
    }

    /**
     * <p>Define the prolog flags.</p>
     *
     * @return The prolog flags.
     */
    static MapHash<String, AbstractFlag> defineFlags() {
        MapHash<String, AbstractFlag> prologflags = new MapHash<String, AbstractFlag>();
        prologflags.add("sys_body_variable", new Flag(FLAG_SYS_BODY_VARIABLE));
        prologflags.add("sys_stack_frame", new Flag(FLAG_SYS_STACK_FRAME));
        prologflags.add("sys_head_variable", new Flag(FLAG_SYS_HEAD_VARIABLE));
        prologflags.add("sys_body_convert", new Flag(FLAG_SYS_BODY_CONVERT));
        prologflags.add("sys_clause_expand", new Flag(FLAG_SYS_CLAUSE_EXPAND));
        prologflags.add("sys_clause_index", new Flag(FLAG_SYS_CLAUSE_INDEX));
        prologflags.add("bounded", new Flag(FLAG_BOUNDED));
        prologflags.add("integer_rounding_function", new Flag(FLAG_INTEGER_ROUNDING_FUNCTION));
        prologflags.add("char_conversion", new Flag(FLAG_CHAR_CONVERSION));
        prologflags.add("max_arity", new Flag(FLAG_MAX_ARITY));
        prologflags.add(OP_FLAG_DOUBLE_QUOTES, new Flag(FLAG_DOUBLE_QUOTES));
        prologflags.add(OP_FLAG_BACK_QUOTES, new Flag(FLAG_BACK_QUOTES));
        prologflags.add("max_code", new Flag(FLAG_MAX_CODE));
        prologflags.add("sys_break_level", new Flag(FLAG_SYS_BREAK_LEVEL));
        prologflags.add("sys_last_pred", new Flag(FLAG_SYS_LAST_PRED));
        prologflags.add(OP_FLAG_SYS_ACT_STATUS, new Flag(FLAG_SYS_ACT_STATUS));
        prologflags.add(OP_FLAG_SINGLE_QUOTES, new Flag(FLAG_SINGLE_QUOTES));
        prologflags.add("sys_variables", new Flag(FLAG_SYS_VARIABLES));
        prologflags.add("sys_choices", new Flag(FLAG_SYS_CHOICES));
        prologflags.add("dialect", new Flag(FLAG_DIALECT));
        prologflags.add("version_data", new Flag(FLAG_VERSION_DATA));
        prologflags.add("sys_random", new Flag(FLAG_SYS_RANDOM));
        prologflags.add("sys_timeout", new Flag(FLAG_SYS_TIMEOUT));
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
            case FLAG_SYS_BODY_VARIABLE:
                return AbstractFlag.switchToAtom((en.store.foyer.getBits() & Foyer.MASK_FOYER_NBDY) == 0);
            case FLAG_SYS_STACK_FRAME:
                return AbstractFlag.switchToAtom((en.store.foyer.getBits() & Foyer.MASK_FOYER_NLST) == 0);
            case FLAG_SYS_HEAD_VARIABLE:
                return AbstractFlag.switchToAtom((en.store.foyer.getBits() & Foyer.MASK_FOYER_NHED) == 0);
            case FLAG_SYS_BODY_CONVERT:
                return AbstractFlag.switchToAtom((en.store.foyer.getBits() & Foyer.MASK_FOYER_NBCV) == 0);
            case FLAG_SYS_CLAUSE_EXPAND:
                return AbstractFlag.switchToAtom((en.store.foyer.getBits() & Foyer.MASK_FOYER_CEXP) != 0);
            case FLAG_SYS_CLAUSE_INDEX:
                return AbstractFlag.switchToAtom((en.store.foyer.getBits() & Foyer.MASK_FOYER_NIDX) == 0);
            case FLAG_BOUNDED:
                return new SkelAtom(AbstractFlag.OP_FALSE);
            case FLAG_INTEGER_ROUNDING_FUNCTION:
                return new SkelAtom(Branch.OP_VALUE_TOWARD_ZERO);
            case FLAG_CHAR_CONVERSION:
                return AbstractFlag.switchToAtom(false);
            case FLAG_MAX_ARITY:
                return Integer.valueOf(Integer.MAX_VALUE);
            case FLAG_DOUBLE_QUOTES:
                return ReadOpts.utilToAtom(en.visor.peekStack().utildouble);
            case FLAG_BACK_QUOTES:
                return ReadOpts.utilToAtom(en.visor.peekStack().utilback);
            case FLAG_MAX_CODE:
                return Integer.valueOf(Character.MAX_CODE_POINT);
            case FLAG_SYS_BREAK_LEVEL:
                return Integer.valueOf(en.visor.breaklevel);
            case FLAG_SYS_LAST_PRED:
                StoreKey sk = en.visor.lastsk;
                if (sk == null) {
                    return AbstractFlag.OP_NULL;
                } else {
                    return StoreKey.storeKeyToSkel(sk);
                }
            case FLAG_SYS_ACT_STATUS:
                return new SkelAtom(en.store.foyer.getError());
            case FLAG_SINGLE_QUOTES:
                return ReadOpts.utilToAtom(en.visor.peekStack().utilsingle);
            case FLAG_SYS_VARIABLES:
                return Integer.valueOf(en.serno);
            case FLAG_SYS_CHOICES:
                return Integer.valueOf(en.number);
            case FLAG_DIALECT:
                AbstractBranch branch = en.store.foyer.getFactory().getBrandBranch();
                Properties descr = branch.getDescriptionLang(en.store.foyer.locale);
                String family = descr.getProperty("family");
                return new SkelAtom(family);
            case FLAG_VERSION_DATA:
                branch = en.store.foyer.getFactory().getBrandBranch();
                descr = branch.getDescriptionLang(en.store.foyer.locale);
                String release = descr.getProperty("release");
                ListArray<Object> list = new ListArray<Object>();
                int k1 = 0;
                int k2 = release.indexOf('.');
                while (k2 != -1) {
                    list.add(Integer.valueOf(release.substring(k1, k2)));
                    k1 = k2 + 1;
                    k2 = release.indexOf('.', k1);
                }
                list.add(Integer.valueOf(release.substring(k1)));
                String product = descr.getProperty("product");
                list.add(new SkelAtom(product));
                Object[] args = new Object[list.size()];
                list.toArray(args);
                family = descr.getProperty("family");
                return new SkelCompound(new SkelAtom(family), args);
            case FLAG_SYS_RANDOM:
                return en.store.foyer.random;
            case FLAG_SYS_TIMEOUT:
                return TermAtomic.normBigInteger(en.store.foyer.timeout);
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
                case FLAG_SYS_BODY_VARIABLE:
                    if (AbstractFlag.atomToSwitch(m, d)) {
                        en.store.foyer.resetBit(Foyer.MASK_FOYER_NBDY);
                    } else {
                        en.store.foyer.setBit(Foyer.MASK_FOYER_NBDY);
                    }
                    return true;
                case FLAG_SYS_STACK_FRAME:
                    if (AbstractFlag.atomToSwitch(m, d)) {
                        en.store.foyer.resetBit(Foyer.MASK_FOYER_NLST);
                    } else {
                        en.store.foyer.setBit(Foyer.MASK_FOYER_NLST);
                    }
                    return true;
                case FLAG_SYS_HEAD_VARIABLE:
                    if (AbstractFlag.atomToSwitch(m, d)) {
                        en.store.foyer.resetBit(Foyer.MASK_FOYER_NHED);
                    } else {
                        en.store.foyer.setBit(Foyer.MASK_FOYER_NHED);
                    }
                    return true;
                case FLAG_SYS_BODY_CONVERT:
                    if (AbstractFlag.atomToSwitch(m, d)) {
                        en.store.foyer.resetBit(Foyer.MASK_FOYER_NBCV);
                    } else {
                        en.store.foyer.setBit(Foyer.MASK_FOYER_NBCV);
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
                case FLAG_BOUNDED:
                    /* can't modify */
                    return false;
                case FLAG_INTEGER_ROUNDING_FUNCTION:
                    /* can't modify */
                    return false;
                case FLAG_CHAR_CONVERSION:
                    /* can't modify */
                    return false;
                case FLAG_MAX_ARITY:
                    /* can't modify */
                    return false;
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
                    /* can't modify */
                    return false;
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
                case FLAG_DIALECT:
                    /* can't modify */
                    return false;
                case FLAG_VERSION_DATA:
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
                    Number num = SpecialEval.derefAndCastInteger(m, d);
                    en.store.foyer.timeout = SpecialEval.castLongValue(num);
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