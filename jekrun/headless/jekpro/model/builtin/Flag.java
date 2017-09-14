package jekpro.model.builtin;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.PrologReader;
import jekpro.model.pretty.ReadOpts;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;
import matula.util.data.ListArray;
import matula.util.data.MapHash;

import java.util.Properties;
import java.util.Random;

/**
 * <p>Capability flags.</p>
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
public final class Flag extends AbstractFlag {
    public final static String OP_FLAG_DOUBLE_QUOTES = "double_quotes"; /* ISO */
    public final static String OP_FLAG_BACK_QUOTES = "back_quotes";
    public final static String OP_FLAG_SINGLE_QUOTES = "single_quotes";
    public final static String OP_FLAG_SYS_ACT_STATUS = "sys_act_status";

    private static final int FLAG_SYS_CHOICE_POINT = 0;
    private static final int FLAG_SYS_BODY_VARIABLE = 1;
    private static final int FLAG_SYS_STACK_FRAME = 2;
    private static final int FLAG_SYS_HEAD_VARIABLE = 3;
    private static final int FLAG_SYS_BODY_CONVERT = 4;
    private static final int FLAG_SYS_CLAUSE_EXPAND = 5;
    private static final int FLAG_SYS_CLAUSE_INDEX = 6;
    private static final int FLAG_BOUNDED = 7;
    private static final int FLAG_INTEGER_ROUNDING_FUNCTION = 8;
    private static final int FLAG_CHAR_CONVERSION = 9;
    private static final int FLAG_MAX_ARITY = 10;
    private static final int FLAG_DOUBLE_QUOTES = 11;
    private static final int FLAG_BACK_QUOTES = 12;
    private static final int FLAG_MAX_CODE = 13;
    private static final int FLAG_SYS_BREAK_LEVEL = 14;
    private static final int FLAG_SYS_LAST_PRED = 15;
    private static final int FLAG_SYS_ACT_STATUS = 16;
    private static final int FLAG_SINGLE_QUOTES = 17;
    private static final int FLAG_SYS_VARIABLES = 18;
    private static final int FLAG_SYS_CHOICES = 19;
    private static final int FLAG_DIALECT = 20;
    private static final int FLAG_VERSION_DATA = 21;
    private static final int FLAG_SYS_RANDOM = 22;
    private static final int FLAG_SYS_TIMEOUT = 23;

    /**
     * <p>Create a flag.</p>
     *
     * @param i The id of the flag.
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
        prologflags.put("sys_choice_point", new Flag(FLAG_SYS_CHOICE_POINT));
        prologflags.put("sys_body_variable", new Flag(FLAG_SYS_BODY_VARIABLE));
        prologflags.put("sys_stack_frame", new Flag(FLAG_SYS_STACK_FRAME));
        prologflags.put("sys_head_variable", new Flag(FLAG_SYS_HEAD_VARIABLE));
        prologflags.put("sys_body_convert", new Flag(FLAG_SYS_BODY_CONVERT));
        prologflags.put("sys_clause_expand", new Flag(FLAG_SYS_CLAUSE_EXPAND));
        prologflags.put("sys_clause_index", new Flag(FLAG_SYS_CLAUSE_INDEX));
        prologflags.put("bounded", new Flag(FLAG_BOUNDED));
        prologflags.put("integer_rounding_function", new Flag(FLAG_INTEGER_ROUNDING_FUNCTION));
        prologflags.put("char_conversion", new Flag(FLAG_CHAR_CONVERSION));
        prologflags.put("max_arity", new Flag(FLAG_MAX_ARITY));
        prologflags.put(OP_FLAG_DOUBLE_QUOTES, new Flag(FLAG_DOUBLE_QUOTES));
        prologflags.put(OP_FLAG_BACK_QUOTES, new Flag(FLAG_BACK_QUOTES));
        prologflags.put("max_code", new Flag(FLAG_MAX_CODE));
        prologflags.put("sys_break_level", new Flag(FLAG_SYS_BREAK_LEVEL));
        prologflags.put("sys_last_pred", new Flag(FLAG_SYS_LAST_PRED));
        prologflags.put(OP_FLAG_SYS_ACT_STATUS, new Flag(FLAG_SYS_ACT_STATUS));
        prologflags.put(OP_FLAG_SINGLE_QUOTES, new Flag(FLAG_SINGLE_QUOTES));
        prologflags.put("sys_variables", new Flag(FLAG_SYS_VARIABLES));
        prologflags.put("sys_choices", new Flag(FLAG_SYS_CHOICES));
        prologflags.put("dialect", new Flag(FLAG_DIALECT));
        prologflags.put("version_data", new Flag(FLAG_VERSION_DATA));
        prologflags.put("sys_random", new Flag(FLAG_SYS_RANDOM));
        prologflags.put("sys_timeout", new Flag(FLAG_SYS_TIMEOUT));
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
            case FLAG_SYS_CHOICE_POINT:
                return en.store.switchToAtom((en.store.flags & Store.MASK_STORE_NCHC) == 0);
            case FLAG_SYS_BODY_VARIABLE:
                return en.store.switchToAtom((en.store.flags & Store.MASK_STORE_NBDY) == 0);
            case FLAG_SYS_STACK_FRAME:
                return en.store.switchToAtom((en.store.flags & Store.MASK_STORE_NLST) == 0);
            case FLAG_SYS_HEAD_VARIABLE:
                return en.store.switchToAtom((en.store.flags & Store.MASK_STORE_NHED) == 0);
            case FLAG_SYS_BODY_CONVERT:
                return en.store.switchToAtom((en.store.flags & Store.MASK_STORE_NBCV) == 0);
            case FLAG_SYS_CLAUSE_EXPAND:
                return en.store.switchToAtom((en.store.flags & Store.MASK_STORE_CEXP) != 0);
            case FLAG_SYS_CLAUSE_INDEX:
                return en.store.switchToAtom((en.store.flags & Store.MASK_STORE_NIDX) == 0);
            case FLAG_BOUNDED:
                return new SkelAtom(Store.OP_FALSE);
            case FLAG_INTEGER_ROUNDING_FUNCTION:
                return new SkelAtom(Branch.OP_VALUE_TOWARD_ZERO);
            case FLAG_CHAR_CONVERSION:
                return en.store.switchToAtom(false);
            case FLAG_MAX_ARITY:
                return Integer.valueOf(Integer.MAX_VALUE);
            case FLAG_DOUBLE_QUOTES:
                return PrologReader.utilToAtom(en.store.getUtilDouble());
            case FLAG_BACK_QUOTES:
                return PrologReader.utilToAtom(en.store.getUtilBack());
            case FLAG_MAX_CODE:
                return Integer.valueOf(Character.MAX_CODE_POINT);
            case FLAG_SYS_BREAK_LEVEL:
                return Integer.valueOf(en.visor.breaklevel);
            case FLAG_SYS_LAST_PRED:
                StoreKey sk = en.visor.lastsk;
                return StoreKey.storeKeyToIndicatorSkel(sk.getFun(), sk.getArity());
            case FLAG_SYS_ACT_STATUS:
                return new SkelAtom(en.store.getError());
            case FLAG_SINGLE_QUOTES:
                return PrologReader.utilToAtom(en.store.getUtilSingle());
            case FLAG_SYS_VARIABLES:
                return Integer.valueOf(en.serno);
            case FLAG_SYS_CHOICES:
                return Integer.valueOf(en.number);
            case FLAG_DIALECT:
                AbstractBranch branch = en.store.getFactory().getBrandBranch();
                Properties descr = branch.getDescriptionLang(en.store.getLocale());
                String family = descr.getProperty("family");
                return new SkelAtom(family);
            case FLAG_VERSION_DATA:
                branch = en.store.getFactory().getBrandBranch();
                descr = branch.getDescriptionLang(en.store.getLocale());
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
                return en.store.random;
            case FLAG_SYS_TIMEOUT:
                return TermAtomic.normBigInteger(en.store.timeout);
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
    public boolean setFlag(Object m, Display d, Engine en) throws EngineMessage {
        switch (id) {
        case FLAG_SYS_CHOICE_POINT:
            if (Store.atomToSwitch(m, d)) {
                en.store.flags &= ~Store.MASK_STORE_NCHC;
            } else {
                en.store.flags |= Store.MASK_STORE_NCHC;
            }
            return true;
        case FLAG_SYS_BODY_VARIABLE:
            if (Store.atomToSwitch(m, d)) {
                en.store.flags &= ~Store.MASK_STORE_NBDY;
            } else {
                en.store.flags |= Store.MASK_STORE_NBDY;
            }
            return true;
        case FLAG_SYS_STACK_FRAME:
            if (Store.atomToSwitch(m, d)) {
                en.store.flags &= ~Store.MASK_STORE_NLST;
            } else {
                en.store.flags |= Store.MASK_STORE_NLST;
            }
            return true;
        case FLAG_SYS_HEAD_VARIABLE:
            if (Store.atomToSwitch(m, d)) {
                en.store.flags &= ~Store.MASK_STORE_NHED;
            } else {
                en.store.flags |= Store.MASK_STORE_NHED;
            }
            return true;
        case FLAG_SYS_BODY_CONVERT:
            if (Store.atomToSwitch(m, d)) {
                en.store.flags &= ~Store.MASK_STORE_NBCV;
            } else {
                en.store.flags |= Store.MASK_STORE_NBCV;
            }
            return true;
        case FLAG_SYS_CLAUSE_EXPAND:
            if (Store.atomToSwitch(m, d)) {
                en.store.flags |= Store.MASK_STORE_CEXP;
            } else {
                en.store.flags &= ~Store.MASK_STORE_CEXP;
            }
            return true;
        case FLAG_SYS_CLAUSE_INDEX:
            if (Store.atomToSwitch(m, d)) {
                en.store.flags &= ~Store.MASK_STORE_NIDX;
            } else {
                en.store.flags |= Store.MASK_STORE_NIDX;
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
            en.store.setUtilDouble(ReadOpts.atomToUtil(m, d, en));
            return true;
        case FLAG_BACK_QUOTES:
            en.store.setUtilBack(ReadOpts.atomToUtil(m, d, en));
            return true;
        case FLAG_MAX_CODE:
            /* can't modify */
            return false;
        case FLAG_SYS_BREAK_LEVEL:
            /* can't modify */
            return false;
        case FLAG_SYS_LAST_PRED:
            en.visor.lastsk = StoreKey.indicatorToStoreKey(m, d, en);
            return true;
        case FLAG_SYS_ACT_STATUS:
            /* can't modify */
            return false;
        case FLAG_SINGLE_QUOTES:
            en.store.setUtilSingle(ReadOpts.atomToUtil(m, d, en));
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
            en.skel = m;
            en.display = d;
            en.deref();
            if (en.skel instanceof Random) {
                en.store.random = (Random) en.skel;
            } else {
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkRef(en.skel, en.display);
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_FLAG_VALUE, en.skel));
            }
            return true;
        case FLAG_SYS_TIMEOUT:
            en.skel = m;
            en.display = d;
            en.deref();
            EngineMessage.checkInstantiated(en.skel);
            Number num = EngineMessage.castInteger(en.skel, en.display);
            en.store.timeout = EngineMessage.castLongValue(num);
            return true;
        default:
            throw new IllegalArgumentException("illegal flag");
        }
    }

}