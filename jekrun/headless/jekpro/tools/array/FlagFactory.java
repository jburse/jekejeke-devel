package jekpro.tools.array;

import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.rope.LoadOpts;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelAtom;
import matula.util.data.MapHash;
import matula.util.wire.AbstractLivestock;
import matula.util.wire.LangProperties;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;

/**
 * <p>Toolkit flags.</p>
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
public final class FlagFactory extends AbstractFlag<Engine> {
    public final static MapHash<String, AbstractFlag<Engine>> DEFAULT
            = new MapHash<String, AbstractFlag<Engine>>();

    private final static String OP_VALUE_TOWARD_ZERO = "toward_zero";
    private final static String OP_VALUE_HALF_EVEN = "half_even";

    public final static String OP_SYS_MASK = "sys_mask";
    public final static String OP_SYS_CUR_INPUT = "sys_cur_input";
    public final static String OP_SYS_CUR_OUTPUT = "sys_cur_output";
    public final static String OP_SYS_CUR_ERROR = "sys_cur_error";
    public final static String OP_SYS_ATTACHED_TO = "sys_attached_to";
    public final static String OP_SYS_GOOD_FOR = "sys_good_for";
    public final static String OP_SYS_CPU_COUNT = "sys_cpu_count";
    public final static String OP_SYS_RUNTIME_VERSION = "sys_runtime_version";
    public final static String OP_VERBOSE = "verbose";
    public final static String OP_SYS_HINT = "sys_hint";
    public final static String OP_SYS_TOOL_INPUT = "sys_tool_input";
    public final static String OP_SYS_TOOL_OUTPUT = "sys_tool_output";
    public final static String OP_SYS_TOOL_ERROR = "sys_tool_error";
    public final static String OP_SYS_BELONGS_TO = "sys_belongs_to";
    public final static String OP_BOUNDED = "bounded";
    public final static String OP_INTEGER_ROUNDING_FUNCTION = "integer_rounding_function";
    public final static String OP_CHAR_CONVERSION = "char_conversion";
    public final static String OP_MAX_ARITY = "max_arity";
    public final static String OP_FLOAT_ROUNDING_FUNCTION = "float_rounding_function";

    private static final int FLAG_SYS_MASK = 0;
    private static final int FLAG_SYS_CUR_INPUT = 1;
    private static final int FLAG_SYS_CUR_OUTPUT = 2;
    private static final int FLAG_SYS_CUR_ERROR = 3;
    private static final int FLAG_SYS_ATTACHED_TO = 4;
    private static final int FLAG_SYS_GOOD_FOR = 6;
    private static final int FLAG_SYS_CPU_COUNT = 7;
    private static final int FLAG_SYS_RUNTIME_VERSION = 8;
    private static final int FLAG_VERBOSE = 9;
    private static final int FLAG_SYS_HINT = 10;
    private static final int FLAG_SYS_TOOL_INPUT = 11;
    private static final int FLAG_SYS_TOOL_OUTPUT = 12;
    private static final int FLAG_SYS_TOOL_ERROR = 13;
    private static final int FLAG_SYS_BELONGS_TO = 14;
    private static final int FLAG_BOUNDED = 15;
    private static final int FLAG_INTEGER_ROUNDING_FUNCTION = 16;
    private static final int FLAG_CHAR_CONVERSION = 17;
    private static final int FLAG_MAX_ARITY = 18;
    private static final int FLAG_FLOAT_ROUNDING_FUNCTION = 19;

    /**
     * <p>Create a flag.</p>
     *
     * @param i The id of the flag.
     */
    private FlagFactory(int i) {
        super(i);
    }

    static {
        DEFAULT.add(OP_SYS_MASK, new FlagFactory(FLAG_SYS_MASK));
        DEFAULT.add(OP_SYS_CUR_INPUT, new FlagFactory(FLAG_SYS_CUR_INPUT));
        DEFAULT.add(OP_SYS_CUR_OUTPUT, new FlagFactory(FLAG_SYS_CUR_OUTPUT));
        DEFAULT.add(OP_SYS_CUR_ERROR, new FlagFactory(FLAG_SYS_CUR_ERROR));
        DEFAULT.add(OP_SYS_ATTACHED_TO, new FlagFactory(FLAG_SYS_ATTACHED_TO));
        DEFAULT.add(OP_SYS_GOOD_FOR, new FlagFactory(FLAG_SYS_GOOD_FOR));
        DEFAULT.add(OP_SYS_CPU_COUNT, new FlagFactory(FLAG_SYS_CPU_COUNT));
        DEFAULT.add(OP_SYS_RUNTIME_VERSION, new FlagFactory(FLAG_SYS_RUNTIME_VERSION));
        DEFAULT.add(OP_VERBOSE, new FlagFactory(FLAG_VERBOSE));
        DEFAULT.add(OP_SYS_HINT, new FlagFactory(FLAG_SYS_HINT));
        DEFAULT.add(OP_SYS_TOOL_INPUT, new FlagFactory(FLAG_SYS_TOOL_INPUT));
        DEFAULT.add(OP_SYS_TOOL_OUTPUT, new FlagFactory(FLAG_SYS_TOOL_OUTPUT));
        DEFAULT.add(OP_SYS_TOOL_ERROR, new FlagFactory(FLAG_SYS_TOOL_ERROR));
        DEFAULT.add(OP_SYS_BELONGS_TO, new FlagFactory(FLAG_SYS_BELONGS_TO));
        DEFAULT.add(OP_BOUNDED, new FlagFactory(FLAG_BOUNDED));
        DEFAULT.add(OP_INTEGER_ROUNDING_FUNCTION, new FlagFactory(FLAG_INTEGER_ROUNDING_FUNCTION));
        DEFAULT.add(OP_CHAR_CONVERSION, new FlagFactory(FLAG_CHAR_CONVERSION));
        DEFAULT.add(OP_MAX_ARITY, new FlagFactory(FLAG_MAX_ARITY));
        DEFAULT.add(OP_FLOAT_ROUNDING_FUNCTION, new FlagFactory(FLAG_FLOAT_ROUNDING_FUNCTION));
    }

    /**
     * <p>Retrieve the value of this flag.</p>
     *
     * @param en The engine.
     * @return The value.
     */
    public Object getObjFlag(Engine obj, Engine en) {
        switch (id) {
            case FLAG_SYS_MASK:
                return AbstractFlag.switchToAtom((en.visor.flags & AbstractLivestock.MASK_LIVESTOCK_NOSG) == 0);
            case FLAG_SYS_CUR_INPUT:
                return en.visor.curinput;
            case FLAG_SYS_CUR_OUTPUT:
                return en.visor.curoutput;
            case FLAG_SYS_CUR_ERROR:
                return en.visor.curerror;
            case FLAG_SYS_ATTACHED_TO:
                Object val = en.visor.attachedto;
                return val != null ? val : AbstractFlag.OP_NULL;
            case FLAG_SYS_GOOD_FOR:
                val = en.store.foyer.goodfor;
                return val != null ? val : AbstractFlag.OP_NULL;
            case FLAG_SYS_CPU_COUNT:
                return Integer.valueOf(Runtime.getRuntime().availableProcessors());
            case FLAG_SYS_RUNTIME_VERSION:
                String name = System.getProperty("java.vm.name");
                int k = (name != null ? name.indexOf(':') : -1);
                if (k != -1)
                    name = name.substring(0, k);
                return name + ", " + System.getProperty("java.version");
            case FLAG_VERBOSE:
                k = 0;
                int flags = en.store.foyer.getBits();
                if ((flags & Foyer.MASK_FOYER_SMRY) != 0)
                    k |= LoadOpts.VERBOSE_SUMMARY;
                if ((flags & Foyer.MASK_FOYER_DTLS) != 0)
                    k |= LoadOpts.VERBOSE_DETAILS;
                switch (k) {
                    case 0:
                        name = AbstractFlag.OP_OFF;
                        break;
                    case LoadOpts.VERBOSE_SUMMARY:
                        name = LoadOpts.OP_VERBOSE_SUMMARY;
                        break;
                    case LoadOpts.VERBOSE_DETAILS:
                        name = LoadOpts.OP_VERBOSE_DETAILS;
                        break;
                    case LoadOpts.VERBOSE_SUMMARY + LoadOpts.VERBOSE_DETAILS:
                        name = AbstractFlag.OP_ON;
                        break;
                    default:
                        throw new IllegalArgumentException("illegal verbosity");
                }
                return new SkelAtom(name);
            case FLAG_SYS_HINT:
                return Integer.valueOf(en.store.foyer.getHint());
            case FLAG_SYS_TOOL_INPUT:
                return en.store.foyer.getFactory().toolinput;
            case FLAG_SYS_TOOL_OUTPUT:
                return en.store.foyer.getFactory().tooloutput;
            case FLAG_SYS_TOOL_ERROR:
                return en.store.foyer.getFactory().toolerror;
            case FLAG_SYS_BELONGS_TO:
                val = en.store.belongsto;
                return val != null ? val : AbstractFlag.OP_NULL;
            case FLAG_BOUNDED:
                return new SkelAtom(AbstractFlag.OP_FALSE);
            case FLAG_INTEGER_ROUNDING_FUNCTION:
                return new SkelAtom(OP_VALUE_TOWARD_ZERO);
            case FLAG_CHAR_CONVERSION:
                return AbstractFlag.switchToAtom(false);
            case FLAG_MAX_ARITY:
                return Integer.valueOf(Integer.MAX_VALUE);
            case FLAG_FLOAT_ROUNDING_FUNCTION:
                return new SkelAtom(OP_VALUE_HALF_EVEN);
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
        switch (id) {
            case FLAG_SYS_MASK:
                en.visor.setMask(AbstractFlag.atomToSwitch(m, d));
                return true;
            case FLAG_SYS_CUR_INPUT:
                m = SpecialUniv.derefAndCastRef(m, d);
                checkRead(m);
                en.visor.curinput = m;
                return true;
            case FLAG_SYS_CUR_OUTPUT:
                m = SpecialUniv.derefAndCastRef(m, d);
                checkWrite(m);
                en.visor.curoutput = m;
                return true;
            case FLAG_SYS_CUR_ERROR:
                m = SpecialUniv.derefAndCastRef(m, d);
                checkWrite(m);
                en.visor.curerror = m;
                return true;
            case FLAG_SYS_ATTACHED_TO:
                en.visor.attachedto = SpecialUniv.derefAndCastRefOrNull(m, d);
                return true;
            case FLAG_SYS_GOOD_FOR:
                en.store.foyer.goodfor = SpecialUniv.derefAndCastRefOrNull(m, d);
                return true;
            case FLAG_SYS_CPU_COUNT:
                /* can't modify */
                return false;
            case FLAG_SYS_RUNTIME_VERSION:
                /* can't modify */
                return false;
            case FLAG_VERBOSE:
                int verb = LoadOpts.atomToVerbose(m, d);
                if ((verb & LoadOpts.VERBOSE_SUMMARY) != 0) {
                    en.store.foyer.setBit(Foyer.MASK_FOYER_SMRY);
                } else {
                    en.store.foyer.resetBit(Foyer.MASK_FOYER_SMRY);
                }
                if ((verb & LoadOpts.VERBOSE_DETAILS) != 0) {
                    en.store.foyer.setBit(Foyer.MASK_FOYER_DTLS);
                } else {
                    en.store.foyer.resetBit(Foyer.MASK_FOYER_DTLS);
                }
                return true;
            case FLAG_SYS_HINT:
                /* can't modify */
                return false;
            case FLAG_SYS_TOOL_INPUT:
                m = SpecialUniv.derefAndCastRef(m, d);
                checkRead(m);
                en.store.foyer.getFactory().toolinput = m;
                return true;
            case FLAG_SYS_TOOL_OUTPUT:
                m = SpecialUniv.derefAndCastRef(m, d);
                checkWrite(m);
                en.store.foyer.getFactory().tooloutput = m;
                return true;
            case FLAG_SYS_TOOL_ERROR:
                m = SpecialUniv.derefAndCastRef(m, d);
                checkWrite(m);
                en.store.foyer.getFactory().toolerror = m;
                return true;
            case FLAG_SYS_BELONGS_TO:
                en.store.belongsto = SpecialUniv.derefAndCastRefOrNull(m, d);
                return true;
            case FLAG_BOUNDED:
            case FLAG_INTEGER_ROUNDING_FUNCTION:
            case FLAG_CHAR_CONVERSION:
            case FLAG_MAX_ARITY:
            case FLAG_FLOAT_ROUNDING_FUNCTION:
                /* can't modify */
                return false;
            default:
                throw new IllegalArgumentException("illegal flag");
        }
    }

    /**
     * <p>Check the object is a text or binary output stream.</p>
     *
     * @param obj The object.
     * @throws EngineMessage Shit happens.
     */
    private static void checkWrite(Object obj) throws EngineMessage {
        if (!(obj instanceof Writer) && !(obj instanceof OutputStream)) {
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_OUTPUT,
                    EngineMessage.OP_PERMISSION_STREAM, obj));
        }
    }

    /**
     * <p>Check whether the object is a text or binary input stream.</p>
     *
     * @param obj The object.
     * @throws EngineMessage Shit happens.
     */
    private static void checkRead(Object obj) throws EngineMessage {
        if (!(obj instanceof Reader) && !(obj instanceof InputStream)) {
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_INPUT,
                    EngineMessage.OP_PERMISSION_STREAM, obj));
        }
    }

}