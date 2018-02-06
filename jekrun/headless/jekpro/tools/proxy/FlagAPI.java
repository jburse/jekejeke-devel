package jekpro.tools.proxy;

import jekpro.frequent.system.ForeignLocale;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.builtin.Flag;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Store;
import jekpro.model.rope.LoadOpts;
import jekpro.tools.term.SkelAtom;
import matula.util.data.MapHash;
import matula.util.wire.AbstractLivestock;

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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class FlagAPI extends AbstractFlag {
    public final static String OP_FLAG_SYS_MASK = "sys_mask";
    public final static String OP_FLAG_SYS_DISP_INPUT = "sys_disp_input";
    public final static String OP_FLAG_SYS_DISP_OUTPUT = "sys_disp_output";
    public final static String OP_FLAG_SYS_DISP_ERROR = "sys_disp_error";
    public final static String OP_FLAG_SYS_CUR_INPUT = "sys_cur_input";
    public final static String OP_FLAG_SYS_CUR_OUTPUT = "sys_cur_output";
    public final static String OP_FLAG_SYS_CUR_ERROR = "sys_cur_error";
    public final static String OP_FLAG_SYS_ATTACHED_TO = "sys_attached_to";
    public final static String OP_FLAG_BASE_URL = "base_url";
    public final static String OP_FLAG_SYS_LOCALE = "sys_locale";
    public final static String OP_FLAG_SYS_BELONGS_TO = "sys_belongs_to";
    public final static String OP_FLAG_SYS_CPU_COUNT = "sys_cpu_count";
    public final static String OP_FLAG_SYS_RUNTIME_VERSION = "sys_runtime_version";
    public final static String OP_FLAG_VERBOSE = "verbose";

    private static final int FLAG_SYS_MASK = 0;
    private static final int FLAG_SYS_DISP_INPUT = 1;
    private static final int FLAG_SYS_DISP_OUTPUT = 2;
    private static final int FLAG_SYS_DISP_ERROR = 3;
    private static final int FLAG_SYS_CUR_INPUT = 4;
    private static final int FLAG_SYS_CUR_OUTPUT = 5;
    private static final int FLAG_SYS_CUR_ERROR = 6;
    private static final int FLAG_SYS_ATTACHED_TO = 7;
    private static final int FLAG_BASE_URL = 8;
    private static final int FLAG_SYS_LOCALE = 9;
    private static final int FLAG_SYS_BELONGS_TO = 10;
    private static final int FLAG_SYS_CPU_COUNT = 11;
    private static final int FLAG_SYS_RUNTIME_VERSION = 12;
    private static final int FLAG_VERBOSE = 13;

    /**
     * <p>Create a flag.</p>
     *
     * @param i The id of the flag.
     */
    private FlagAPI(int i) {
        super(i);
    }

    /**
     * <p>Define the prolog flags.</p>
     *
     * @return The prolog flags.
     */
    static MapHash<String, AbstractFlag> defineFlags() {
        MapHash<String, AbstractFlag> prologflags = new MapHash<String, AbstractFlag>();
        prologflags.add(OP_FLAG_SYS_MASK, new FlagAPI(FLAG_SYS_MASK));
        prologflags.add(OP_FLAG_SYS_DISP_INPUT, new FlagAPI(FLAG_SYS_DISP_INPUT));
        prologflags.add(OP_FLAG_SYS_DISP_OUTPUT, new FlagAPI(FLAG_SYS_DISP_OUTPUT));
        prologflags.add(OP_FLAG_SYS_DISP_ERROR, new FlagAPI(FLAG_SYS_DISP_ERROR));
        prologflags.add(OP_FLAG_SYS_CUR_INPUT, new FlagAPI(FLAG_SYS_CUR_INPUT));
        prologflags.add(OP_FLAG_SYS_CUR_OUTPUT, new FlagAPI(FLAG_SYS_CUR_OUTPUT));
        prologflags.add(OP_FLAG_SYS_CUR_ERROR, new FlagAPI(FLAG_SYS_CUR_ERROR));
        prologflags.add(OP_FLAG_SYS_ATTACHED_TO, new FlagAPI(FLAG_SYS_ATTACHED_TO));
        prologflags.add(OP_FLAG_BASE_URL, new FlagAPI(FLAG_BASE_URL));
        prologflags.add(OP_FLAG_SYS_LOCALE, new FlagAPI(FLAG_SYS_LOCALE));
        prologflags.add(OP_FLAG_SYS_BELONGS_TO, new FlagAPI(FLAG_SYS_BELONGS_TO));
        prologflags.add(OP_FLAG_SYS_CPU_COUNT, new FlagAPI(FLAG_SYS_CPU_COUNT));
        prologflags.add(OP_FLAG_SYS_RUNTIME_VERSION, new FlagAPI(FLAG_SYS_RUNTIME_VERSION));
        prologflags.add("verbose", new FlagAPI(FLAG_VERBOSE));
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
            case FLAG_SYS_MASK:
                return en.store.switchToAtom((en.visor.flags & AbstractLivestock.MASK_LIVESTOCK_NOSG) == 0);
            case FLAG_SYS_DISP_INPUT:
                return en.visor.dispinput;
            case FLAG_SYS_DISP_OUTPUT:
                return en.visor.dispoutput;
            case FLAG_SYS_DISP_ERROR:
                return en.visor.disperror;
            case FLAG_SYS_CUR_INPUT:
                return en.visor.curinput;
            case FLAG_SYS_CUR_OUTPUT:
                return en.visor.curoutput;
            case FLAG_SYS_CUR_ERROR:
                return en.visor.curerror;
            case FLAG_SYS_ATTACHED_TO:
                Object val = en.visor.attachedto;
                return val != null ? val : en.store.ATOM_NULL;
            case FLAG_BASE_URL:
                String path = en.store.getBase();
                return new SkelAtom(path != null ? path : "");
            case FLAG_SYS_LOCALE:
                return new SkelAtom(en.store.getLocale().toString());
            case FLAG_SYS_BELONGS_TO:
                val = en.store.belongsto;
                return val != null ? val : en.store.ATOM_NULL;
            case FLAG_SYS_CPU_COUNT:
                return Integer.valueOf(Runtime.getRuntime().availableProcessors());
            case FLAG_SYS_RUNTIME_VERSION:
                return System.getProperty("java.vm.specification.version");
            case FLAG_VERBOSE:
                int verb = 0;
                int flags=en.store.getBits();
                if ((flags & Store.MASK_STORE_SMRY) != 0)
                    verb |= LoadOpts.VERBOSE_SUMMARY;
                if ((flags & Store.MASK_STORE_DTLS) != 0)
                    verb |= LoadOpts.VERBOSE_DETAILS;
                String name;
                switch (verb) {
                    case 0:
                        name = Store.OP_OFF;
                        break;
                    case LoadOpts.VERBOSE_SUMMARY:
                        name = LoadOpts.OP_VERBOSE_SUMMARY;
                        break;
                    case LoadOpts.VERBOSE_DETAILS:
                        name = LoadOpts.OP_VERBOSE_DETAILS;
                        break;
                    case LoadOpts.VERBOSE_SUMMARY + LoadOpts.VERBOSE_DETAILS:
                        name = Store.OP_ON;
                        break;
                    default:
                        throw new IllegalArgumentException("illegal verbosity");
                }
                return new SkelAtom(name);
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
            case FLAG_SYS_MASK:
                en.visor.setMask(Store.atomToSwitch(m, d));
                return true;
            case FLAG_SYS_DISP_INPUT:
                en.skel = m;
                en.display = d;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkRef(en.skel, en.display);
                checkRead(en.skel);
                en.visor.dispinput = en.skel;
                return true;
            case FLAG_SYS_DISP_OUTPUT:
                en.skel = m;
                en.display = d;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkRef(en.skel, en.display);
                checkWrite(en.skel);
                en.visor.dispoutput = en.skel;
                return true;
            case FLAG_SYS_DISP_ERROR:
                en.skel = m;
                en.display = d;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkRef(en.skel, en.display);
                checkWrite(en.skel);
                en.visor.disperror = en.skel;
                return true;
            case FLAG_SYS_CUR_INPUT:
                en.skel = m;
                en.display = d;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkRef(en.skel, en.display);
                checkRead(en.skel);
                en.visor.curinput = en.skel;
                return true;
            case FLAG_SYS_CUR_OUTPUT:
                en.skel = m;
                en.display = d;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkRef(en.skel, en.display);
                checkWrite(en.skel);
                en.visor.curoutput = en.skel;
                return true;
            case FLAG_SYS_CUR_ERROR:
                en.skel = m;
                en.display = d;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkRef(en.skel, en.display);
                checkWrite(en.skel);
                en.visor.curerror = en.skel;
                return true;
            case FLAG_SYS_ATTACHED_TO:
                en.visor.attachedto = castRefOrNull(m, d, en);
                return true;
            case FLAG_BASE_URL:
                en.skel = m;
                en.display = d;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                String fun = EngineMessage.castString(en.skel, en.display);
                en.store.setBase(!"".equals(fun) ? fun : null);
                return true;
            case FLAG_SYS_LOCALE:
                en.skel = m;
                en.display = d;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                fun = EngineMessage.castString(en.skel, en.display);
                en.store.setLocale(ForeignLocale.stringToLocale(fun));
                return true;
            case FLAG_SYS_BELONGS_TO:
                en.store.belongsto = castRefOrNull(m, d, en);
                return true;
            case FLAG_SYS_CPU_COUNT:
                /* can't modify */
                return false;
            case FLAG_SYS_RUNTIME_VERSION:
                /* can't modify */
                return false;
            case FLAG_VERBOSE:
                int verb = LoadOpts.atomToVerbose(m, d, en);
                if ((verb & LoadOpts.VERBOSE_SUMMARY) != 0) {
                    en.store.setBit(Store.MASK_STORE_SMRY);
                } else {
                    en.store.resetBit(Store.MASK_STORE_SMRY);
                }
                if ((verb & LoadOpts.VERBOSE_DETAILS) != 0) {
                    en.store.setBit(Store.MASK_STORE_DTLS);
                } else {
                    en.store.resetBit(Store.MASK_STORE_DTLS);
                }
                return true;
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

    /**
     * <p>Cast a frame or null.</p>
     *
     * @param m The skel.
     * @return The frame.
     * @throws EngineMessage Shit happens.
     */
    private static Object castRefOrNull(Object m, Display d, Engine en)
            throws EngineMessage {
        en.skel = m;
        en.display = d;
        en.deref();
        if (en.skel instanceof SkelAtom &&
                ((SkelAtom) en.skel).fun.equals(Store.OP_NULL)) {
            return null;
        } else {
            EngineMessage.checkInstantiated(en.skel);
            EngineMessage.checkRef(en.skel,en.display);
            return en.skel;
        }
    }

}