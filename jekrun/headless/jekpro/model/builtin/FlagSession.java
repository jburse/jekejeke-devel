package jekpro.model.builtin;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import jekpro.model.rope.LoadOpts;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.array.FlagFactory;
import jekpro.tools.array.Types;
import jekpro.tools.term.SkelAtom;
import matula.comp.sharik.AbstractActivator;
import matula.util.data.MapHash;
import matula.util.wire.LangProperties;

import java.io.File;

/**
 * <p>Session flags on runtime library level.</p>
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
public final class FlagSession extends AbstractFlag<Store> {
    public final static MapHash<String, AbstractFlag<Store>> DEFAULT
            = new MapHash<>();

    public final static String OP_USER_PREFS = "user_prefs";
    public final static String OP_BASE_URL = "base_url";
    public final static String OP_SYS_LOCALE = "sys_locale";
    public final static String OP_SYS_HINT = "sys_hint";
    public final static String OP_SYS_APPLICATION = "sys_application";
    public final static String OP_SYS_BELONGS_TO = "sys_belongs_to";
    public final static String OP_VERBOSE = "verbose";
    public final static String OP_SYS_GOOD_FOR = "sys_good_for";

    private static final int FLAG_USER_PREFS = 0;
    private static final int FLAG_BASE_URL = 1;
    private static final int FLAG_SYS_LOCALE = 2;
    private static final int FLAG_SYS_HINT = 3;
    private static final int FLAG_SYS_APPLICATION = 4;
    private static final int FLAG_SYS_BELONGS_TO = 5;
    private static final int FLAG_VERBOSE = 6;
    private static final int FLAG_SYS_GOOD_FOR = 7;

    static {
        DEFAULT.add(OP_USER_PREFS, new FlagSession(FLAG_USER_PREFS));
        DEFAULT.add(OP_BASE_URL, new FlagSession(FLAG_BASE_URL));
        DEFAULT.add(OP_SYS_LOCALE, new FlagSession(FLAG_SYS_LOCALE));
        DEFAULT.add(OP_SYS_HINT, new FlagSession(FLAG_SYS_HINT));
        DEFAULT.add(OP_SYS_APPLICATION, new FlagSession(FLAG_SYS_APPLICATION));
        DEFAULT.add(OP_SYS_BELONGS_TO, new FlagSession(FLAG_SYS_BELONGS_TO));
        DEFAULT.add(OP_VERBOSE, new FlagSession(FLAG_VERBOSE));
        DEFAULT.add(OP_SYS_GOOD_FOR, new FlagSession(FLAG_SYS_GOOD_FOR));
    }

    /**
     * <p>Create a session flag.</p>
     *
     * @param i The id of the session flag.
     */
    private FlagSession(int i) {
        super(i);
    }

    /**
     * <p>Retrieve the value of this flag.</p>
     *
     * @return The value.
     */
    public Object getObjFlag(Store obj) {
        switch (id) {
            case FLAG_USER_PREFS:
                AbstractActivator activator = obj.foyer.getFramework().getActivator();
                File dir = activator.getUserDir(obj.foyer.getApplication());
                return new SkelAtom(dir.toString());
            case FLAG_BASE_URL:
                String path = obj.getBase();
                return new SkelAtom(path != null ? path : "");
            case FLAG_SYS_LOCALE:
                return new SkelAtom(obj.foyer.locale.toString());
            case FLAG_SYS_HINT:
                return Integer.valueOf(obj.foyer.getHint());
            case FLAG_SYS_APPLICATION:
                Object val = obj.foyer.getApplication();
                return val != null ? val : AbstractFlag.OP_NULL;
            case FLAG_SYS_BELONGS_TO:
                val = obj.belongsto;
                return (val != null ? val : AbstractFlag.OP_NULL);
            case FLAG_VERBOSE:
                int k = 0;
                int flags = obj.foyer.getBits();
                if ((flags & Foyer.MASK_FOYER_SMRY) != 0)
                    k |= LoadOpts.VERBOSE_SUMMARY;
                if ((flags & Foyer.MASK_FOYER_DTLS) != 0)
                    k |= LoadOpts.VERBOSE_DETAILS;
                return FlagSession.verboseToAtom(k);
            case FLAG_SYS_GOOD_FOR:
                val = obj.foyer.goodfor;
                return (val != null ? val : AbstractFlag.OP_NULL);
            default:
                throw new IllegalArgumentException("illegal flag");
        }
    }

    /**
     * <p>Set the value of a this flag.</p>
     *
     * @param m  The value skel.
     * @param d  The value display.
     * @return True if flag could be changed, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public boolean setObjFlag(Store obj, Object m, Display d)
            throws EngineMessage {
        try {
            switch (id) {
                case FLAG_USER_PREFS:
                    /* can't modify */
                    return false;
                case FLAG_BASE_URL:
                    String fun = SpecialUniv.derefAndCastString(m, d);
                    obj.setBase(!"".equals(fun) ? fun : null);
                    return true;
                case FLAG_SYS_LOCALE:
                    fun = SpecialUniv.derefAndCastString(m, d);
                    obj.foyer.locale = LangProperties.stringToLocale(fun);
                    return true;
                case FLAG_SYS_HINT:
                    Number num = SpecialEval.derefAndCastInteger(m, d);
                    AbstractFactory factory = obj.foyer.getFactory();
                    factory.setHint(obj.foyer, SpecialEval.castIntValue(num));
                    return true;
                case FLAG_SYS_APPLICATION:
                    Object val = SpecialUniv.derefAndCastRefOrNull(m, d);
                    obj.foyer.setApplication(val);
                    return true;
                case FLAG_SYS_BELONGS_TO:
                    obj.belongsto = SpecialUniv.derefAndCastRefOrNull(m, d);
                    return true;
                case FLAG_VERBOSE:
                    int k = FlagSession.atomToVerbose(m, d);
                    if ((k & LoadOpts.VERBOSE_SUMMARY) != 0) {
                        obj.foyer.setBit(Foyer.MASK_FOYER_SMRY);
                    } else {
                        obj.foyer.resetBit(Foyer.MASK_FOYER_SMRY);
                    }
                    if ((k & LoadOpts.VERBOSE_DETAILS) != 0) {
                        obj.foyer.setBit(Foyer.MASK_FOYER_DTLS);
                    } else {
                        obj.foyer.resetBit(Foyer.MASK_FOYER_DTLS);
                    }
                    return true;
                case FLAG_SYS_GOOD_FOR:
                    obj.foyer.goodfor = SpecialUniv.derefAndCastRefOrNull(m, d);
                    return true;
                default:
                    throw new IllegalArgumentException("illegal flag");
            }
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
        }
    }

    /**************************************************************/
    /* Conversion Helper                                          */
    /**************************************************************/

    /**
     * <p>Convert an atom to a verbose. Will throw exception
     * when the atom is not well formed.</p>
     *
     * @param m The verbose skeleton.
     * @param d The verbose display.
     * @return The verbose.
     * @throws EngineMessage Shit happens.
     */
    public static int atomToVerbose(Object m, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        if (fun.equals(OP_OFF)) {
            return 0;
        } else if (fun.equals(LoadOpts.OP_VERBOSE_DETAILS)) {
            return LoadOpts.VERBOSE_DETAILS;
        } else if (fun.equals(LoadOpts.OP_VERBOSE_SUMMARY)) {
            return LoadOpts.VERBOSE_SUMMARY;
        } else if (fun.equals(OP_ON)) {
            return LoadOpts.VERBOSE_DETAILS + LoadOpts.VERBOSE_SUMMARY;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_VERBOSE_OPTION, m), d);
        }
    }

    /**
     * <p>Convert verbose to an atom.</p>
     * @param k The verbose.
     * @return The atom.
     */
    public static SkelAtom verboseToAtom(int k) {
        String name;
        switch (k) {
            case 0:
                name = OP_OFF;
                break;
            case LoadOpts.VERBOSE_SUMMARY:
                name = LoadOpts.OP_VERBOSE_SUMMARY;
                break;
            case LoadOpts.VERBOSE_DETAILS:
                name = LoadOpts.OP_VERBOSE_DETAILS;
                break;
            case LoadOpts.VERBOSE_SUMMARY + LoadOpts.VERBOSE_DETAILS:
                name = OP_ON;
                break;
            default:
                throw new IllegalArgumentException("illegal verbosity");
        }
        return new SkelAtom(name);
    }

}