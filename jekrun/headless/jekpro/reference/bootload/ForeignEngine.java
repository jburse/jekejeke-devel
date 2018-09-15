package jekpro.reference.bootload;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractStore;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.call.Interpreter;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.proxy.FactoryAPI;
import jekpro.tools.term.Lobby;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.TermCompound;
import matula.comp.sharik.AbstractBundle;
import matula.comp.sharik.AbstractTracking;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Locale;
import java.util.Properties;

/**
 * <p>The foreign predicates for the module engine.</p>
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
public final class ForeignEngine {
    private final static int MAX_UNSIGNED_BYTE = 255;

    /**
     * <p>Retrieve the list of flags.</p>
     *
     * @param inter The call-in.
     * @return The list of flags.
     */
    public static Object sysListFlags(Interpreter inter) {
        Lobby lobby = inter.getKnowledgebase().getLobby();
        ArrayList<String> flags = inter.getProperties();
        Object res = lobby.ATOM_NIL;
        for (int i = flags.size() - 1; i >= 0; i--)
            res = new TermCompound(lobby.ATOM_CONS,
                    flags.get(i), res);
        return res;
    }

    /**
     * <p>Retrieve a flag.</p>
     *
     * @param inter The call-in.
     * @param flag  The flag.
     * @return The value.
     * @throws InterpreterMessage Flag undefined.
     */
    public static Object sysGetFlag(Interpreter inter, String flag)
            throws InterpreterMessage {
        Object val = inter.getProperty(flag);
        if (val == null)
            throw new InterpreterMessage(InterpreterMessage.domainError(
                    "prolog_flag", flag));
        return val;
    }

    /**
     * <p>Set a flag.</p>
     *
     * @param inter The call-in.
     * @param flag  The flag.
     * @param val   The value.
     * @throws InterpreterMessage Flag undefined.
     */
    public static void sysSetFlag(Interpreter inter, String flag, Object val)
            throws InterpreterMessage {
        inter.setProperty(flag, val);
    }

    /**
     * <p>Halt the system.</p>
     *
     * @param val The exit code.
     * @throws ClassCastException Illegal exit code.
     */
    public static void sysHalt(Integer val)
            throws ClassCastException {
        int k = SpecialEval.castOctet(val);
        System.exit(k);
    }

    /*************************************************************/
    /* Prolog Flags                                              */
    /*************************************************************/

    /**
     * <p>Change the value of a prolog flag.</p>
     * <p>Throws a domain error for undefined flags.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param flag The name of the flag.
     * @param m    The value skel.
     * @param d    The value display.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void setFlag(String flag, Object m, Display d, Engine en)
            throws EngineMessage {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            MapHash<String, AbstractFlag> pfs = branch.getPrologFlags();
            AbstractFlag af = (pfs != null ? pfs.get(flag) : null);
            if (af != null) {
                if (af.setFlag(m, d, en)) {
                    return;
                } else {
                    throw new EngineMessage(EngineMessage.permissionError(
                            EngineMessage.OP_PERMISSION_MODIFY,
                            EngineMessage.OP_PERMISSION_FLAG, new SkelAtom(flag)));
                }
            }
        }
        AbstractFactory factory = en.store.foyer.getFactory();
        MapHash<String, AbstractFlag> pfs = factory.getPrologFlags();
        AbstractFlag af = (pfs != null ? pfs.get(flag) : null);
        if (af != null) {
            if (af.setFlag(m, d, en)) {
                return;
            } else {
                throw new EngineMessage(EngineMessage.permissionError(
                        EngineMessage.OP_PERMISSION_MODIFY,
                        EngineMessage.OP_PERMISSION_FLAG, new SkelAtom(flag)));
            }
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_FLAG,
                new SkelAtom(flag)));
    }

    /**
     * <p>Retrieve the value of the given flag.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param flag The flag.
     * @param en   The engine.
     * @return The value or null.
     */
    public static Object getFlag(String flag, Engine en) {
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            MapHash<String, AbstractFlag> pfs = branch.getPrologFlags();
            AbstractFlag af = (pfs != null ? pfs.get(flag) : null);
            if (af != null)
                return af.getFlag(en);
        }
        AbstractFactory factory = en.store.foyer.getFactory();
        MapHash<String, AbstractFlag> pfs = factory.getPrologFlags();
        AbstractFlag af = (pfs != null ? pfs.get(flag) : null);
        if (af != null)
            return af.getFlag(en);
        return null;
    }

    /**
     * <p>Retrieve the list of flags.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param en The engine.
     * @return The list of flags.
     */
    public static ArrayList<String> listFlags(Engine en) {
        ArrayList<String> res = new ArrayList<String>();
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            MapHash<String, AbstractFlag> pfs = branch.getPrologFlags();
            for (MapEntry<String, AbstractFlag> entry2 = (pfs != null ? pfs.getFirstEntry() : null);
                 entry2 != null; entry2 = pfs.successor(entry2)) {
                res.add(entry2.key);
            }
        }
        AbstractFactory factory = en.store.foyer.getFactory();
        MapHash<String, AbstractFlag> pfs = factory.getPrologFlags();
        for (MapEntry<String, AbstractFlag> entry2 = (pfs != null ? pfs.getFirstEntry() : null);
             entry2 != null; entry2 = pfs.successor(entry2)) {
            res.add(entry2.key);
        }
        return res;
    }

    /**********************************************************/
    /* Hierarchical Knowledgebases                            */
    /**********************************************************/

    /**
     * <p>Show the knowledge base stack.</p>
     *
     * @param inter The interpreter.
     * @throws EngineMessage Shit happens.
     */
    public static void sysShowKnowledgebaseStack(Interpreter inter)
            throws EngineMessage {
        Engine en = (Engine) inter.getEngine();
        Object obj = en.visor.dispoutput;
        FactoryAPI.checkTextWrite(obj);
        Writer wr = (Writer) obj;
        try {
            AbstractStore store = en.store;
            while (store != null) {
                wr.write("store ");
                wr.write(store.toString());
                wr.write('\n');
                wr.flush();
                store = store.parent;
            }
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /*************************************************************/
    /* Prolog Data                                               */
    /*************************************************************/

    /**
     * <p>Retrieve the Prolog version.</p>
     *
     * @param inter The interpreter.
     * @return The Prolog version.
     */
    public static String sysPrologVersion(Interpreter inter) {
        Engine en = (Engine) inter.getEngine();
        AbstractBranch brand = en.store.foyer.getFactory().getBrandBranch();
        Locale locale = en.store.foyer.locale;
        Properties descr = brand.getDescriptionLang(locale);
        String family = descr.getProperty("family");
        String product = descr.getProperty("product") + " " + descr.getProperty("release");
        return family + ", " + product;
    }

    /**
     * <p>Retrieve the Prolog vendor.</p>
     *
     * @param inter The interpreter.
     * @return The Prolog vendor.
     */
    public static String sysPrologVendor(Interpreter inter) {
        Engine en = (Engine) inter.getEngine();
        AbstractBranch brand = en.store.foyer.getFactory().getBrandBranch();
        Locale locale = en.store.foyer.locale;
        Properties descr = brand.getDescriptionLang(locale);
        return descr.getProperty("company");
    }

}
