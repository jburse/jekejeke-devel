package jekpro.reference.bootload;

import derek.util.protect.LicenseError;
import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Store;
import jekpro.model.rope.LoadForce;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.call.*;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.Knowledgebase;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.TermAtomic;
import matula.comp.sharik.AbstractTracking;
import matula.util.config.AbstractBundle;
import matula.util.config.AbstractFramework;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;

import java.util.Enumeration;
import java.util.Locale;

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

    /**
     * <p>Retrieve the list of Prolog flags.</p>
     *
     * @param inter The interpreter.
     * @param co    The call out.
     * @return The Prolog flags.
     */
    public static String sysCurrentFlag(Interpreter inter, CallOut co) {
        Enumeration<String> dc;
        if (co.getFirst()) {
            Engine en = inter.getEngine();
            ListArray<String> list = ForeignEngine.listPrologFlags(en);
            ListArray<String> list2 = ForeignEngine.listSessionFlags(en.store);
            for (int i = 0; i < list2.size(); i++)
                list.add(list2.get(i));
            dc = list.elements();
            if (!dc.hasMoreElements())
                return null;
            co.setData(dc);
        } else {
            dc = (Enumeration<String>) co.getData();
        }
        String res = dc.nextElement();
        co.setRetry(dc.hasMoreElements());
        return res;
    }

    /**
     * <p>Retrieve a Prolog flag.</p>
     *
     * @param inter The interpreter.
     * @param flag  The Prolog flag.
     * @return The value.
     * @throws InterpreterMessage Flag undefined.
     */
    public static Object sysGetFlag(Interpreter inter, String flag)
            throws InterpreterMessage {
        Engine en = inter.getEngine();
        AbstractFlag<Engine> af = findPrologFlag(flag, en);
        if (af != null)
            return af.getObjFlag(en, en);
        AbstractFlag<Store> af2 = findSessionFlag(flag, en.store);
        if (af2 != null)
            return af2.getObjFlag(en.store, null);
        throw new InterpreterMessage(InterpreterMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_FLAG, flag));
    }

    /**
     * <p>Set a Prolog flag.</p>
     *
     * @param inter The interpreter.
     * @param flag  The Prolog flag.
     * @param val   The value.
     * @throws InterpreterMessage Flag undefined.
     */
    public static void sysSetFlag(Interpreter inter, String flag, Object val)
            throws InterpreterMessage {
        Engine en = inter.getEngine();
        try {
            AbstractFlag<Engine> af = findPrologFlag(flag, en);
            if (af != null) {
                if (!af.setObjFlag(en, AbstractTerm.getSkel(val), AbstractTerm.getDisplay(val), en))
                    throw new EngineMessage(EngineMessage.permissionError(
                            EngineMessage.OP_PERMISSION_MODIFY,
                            EngineMessage.OP_PERMISSION_FLAG, new SkelAtom(flag)));
                return;
            }
            AbstractFlag<Store> af2 = findSessionFlag(flag, en.store);
            if (af2 != null) {
                if (!af2.setObjFlag(en.store, AbstractTerm.getSkel(val), AbstractTerm.getDisplay(val), null))
                    throw new EngineMessage(EngineMessage.permissionError(
                            EngineMessage.OP_PERMISSION_MODIFY,
                            EngineMessage.OP_PERMISSION_FLAG, new SkelAtom(flag)));
                return;
            }
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_PROLOG_FLAG,
                    new SkelAtom(flag)));
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        }
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
     * <p>Retrieve the list of Prolog flags.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param en The engine.
     * @return The list of flags.
     */
    public static ListArray<String> listPrologFlags(Engine en) {
        ListArray<String> res = new ListArray<>();
        AbstractFactory factory = en.store.foyer.getFactory();
        ListArray<MapHash<String, AbstractFlag<Engine>>> flags = factory.getPrologFlags();
        for (int i = 0; i < flags.size(); i++)
            listPrologFlags(flags.get(i), res);
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot
                = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            MapHash<String, AbstractFlag<Engine>> pfs = branch.getPrologFlags();
            if (pfs == null)
                continue;
            listPrologFlags(pfs, res);
        }
        return res;
    }

    /**
     * <p>List the flag names from the hash table.</p>
     *
     * @param pfs The hash table.
     * @param res The flag names.
     */
    private static void listPrologFlags(MapHash<String, AbstractFlag<Engine>> pfs,
                                        ListArray<String> res) {
        for (MapEntry<String, AbstractFlag<Engine>> entry2 = pfs.getFirstEntry();
             entry2 != null; entry2 = pfs.successor(entry2)) {
            res.add(entry2.key);
        }
    }

    /**
     * <p>Retrieve the value of the given Prolog flag.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param flag The flag.
     * @param en   The engine.
     * @return The value or null.
     */
    public static Object getPrologFlag(String flag, Engine en) {
        AbstractFlag<Engine> af = findPrologFlag(flag, en);
        if (af != null)
            return af.getObjFlag(en, en);
        return null;
    }

    /**
     * <p>Change the value of a prolog Prolog flag.</p>
     * <p>Throws a domain error for undefined flags.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param flag The name of the flag.
     * @param m    The value skel.
     * @param d    The value display.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void setPrologFlag(String flag, Object m, Display d, Engine en)
            throws EngineMessage {
        AbstractFlag<Engine> af = findPrologFlag(flag, en);
        if (af != null) {
            if (!af.setObjFlag(en, m, d, en))
                throw new EngineMessage(EngineMessage.permissionError(
                        EngineMessage.OP_PERMISSION_MODIFY,
                        EngineMessage.OP_PERMISSION_FLAG, new SkelAtom(flag)));
            return;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_FLAG,
                new SkelAtom(flag)));
    }

    /**
     * <p>Find a Prolog flag.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param flag The Prolog flag name.
     * @param en   The engine.
     * @return The Prolog flag.
     */
    private static AbstractFlag<Engine> findPrologFlag(String flag, Engine en) {
        AbstractFactory factory = en.store.foyer.getFactory();
        ListArray<MapHash<String, AbstractFlag<Engine>>> flags = factory.getPrologFlags();
        for (int i = 0; i < flags.size(); i++) {
            MapHash<String, AbstractFlag<Engine>> pfs = flags.get(i);
            AbstractFlag<Engine> af = pfs.get(flag);
            if (af != null)
                return af;
        }
        MapEntry<AbstractBundle, AbstractTracking>[] snapshot
                = en.store.foyer.snapshotTrackings();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractBundle, AbstractTracking> entry = snapshot[i];
            AbstractTracking tracking = entry.value;
            if (!LicenseError.ERROR_LICENSE_OK.equals(tracking.getError()))
                continue;
            AbstractBranch branch = (AbstractBranch) entry.key;
            MapHash<String, AbstractFlag<Engine>> pfs = branch.getPrologFlags();
            if (pfs == null)
                continue;
            AbstractFlag<Engine> af = pfs.get(flag);
            if (af != null)
                return af;
        }
        return null;
    }

    /*************************************************************/
    /* Session Flags                                             */
    /*************************************************************/

    /**
     * <p>Retrieve the list of session flags.</p>
     *
     * @param store The store.
     * @return The list of flags.
     */
    public static ListArray<String> listSessionFlags(Store store) {
        ListArray<String> res = new ListArray<>();
        AbstractFactory factory = store.foyer.getFactory();
        ListArray<MapHash<String, AbstractFlag<Store>>> flags = factory.getSessionFlags();
        for (int i = 0; i < flags.size(); i++)
            listSessionFlags(flags.get(i), res);
        return res;
    }

    /**
     * <p>List the flag names from the hash table.</p>
     *
     * @param pfs The hash table.
     * @param res The flag names.
     */
    private static void listSessionFlags(MapHash<String, AbstractFlag<Store>> pfs,
                                         ListArray<String> res) {
        for (MapEntry<String, AbstractFlag<Store>> entry2 = pfs.getFirstEntry();
             entry2 != null; entry2 = pfs.successor(entry2)) {
            res.add(entry2.key);
        }
    }

    /**
     * <p>Retrieve the value of the given session flag.</p>
     *
     * @param flag  The flag.
     * @param store The store.
     * @return The value or null.
     */
    public static Object getSessionFlag(String flag, Store store) {
        AbstractFlag<Store> af = findSessionFlag(flag, store);
        if (af != null)
            return af.getObjFlag(store, null);
        return null;
    }

    /**
     * <p>Change the value of a prolog Prolog flag.</p>
     * <p>Throws a domain error for undefined flags.</p>
     * <p>Only capabilities that are ok are considered.</p>
     *
     * @param flag  The name of the flag.
     * @param m     The value skel.
     * @param d     The value display.
     * @param store The store.
     * @throws EngineMessage Shit happens.
     */
    public static void setSessionFlag(String flag, Object m, Display d, Store store)
            throws EngineMessage {
        AbstractFlag<Store> af = findSessionFlag(flag, store);
        if (af != null) {
            if (!af.setObjFlag(store, m, d, null))
                throw new EngineMessage(EngineMessage.permissionError(
                        EngineMessage.OP_PERMISSION_MODIFY,
                        EngineMessage.OP_PERMISSION_FLAG, new SkelAtom(flag)));
            return;
        }
        throw new EngineMessage(EngineMessage.domainError(
                EngineMessage.OP_DOMAIN_PROLOG_FLAG,
                new SkelAtom(flag)));
    }

    /**
     * <p>Find a session flag.</p>
     *
     * @param flag  The session flag name.
     * @param store The store.
     * @return The Prolog flag.
     */
    private static AbstractFlag<Store> findSessionFlag(String flag, Store store) {
        AbstractFactory factory = store.foyer.getFactory();
        ListArray<MapHash<String, AbstractFlag<Store>>> flags = factory.getSessionFlags();
        for (int i = 0; i < flags.size(); i++) {
            MapHash<String, AbstractFlag<Store>> pfs = flags.get(i);
            AbstractFlag<Store> af = pfs.get(flag);
            if (af != null)
                return af;
        }
        return null;
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
        Knowledgebase know = inter.getKnowledgebase();
        Capability brand = know.getToolkit().getBrandCapability();
        Locale locale = know.getFoyer().locale;
        ClassLoader loader = know.getRoot().getLoader();
        AbstractFramework framework = know.getFoyer().getFramework();
        return brand.getFamily(locale, loader) + ", " +
                brand.getProductReleaseDate(locale, loader, framework);
    }

    /**
     * <p>Retrieve the Prolog vendor.</p>
     *
     * @param inter The interpreter.
     * @return The Prolog vendor.
     */
    public static String sysPrologVendor(Interpreter inter) {
        Knowledgebase know = inter.getKnowledgebase();
        Capability brand = know.getToolkit().getBrandCapability();
        Locale locale = know.getFoyer().locale;
        ClassLoader loader = know.getRoot().getLoader();
        return brand.getCompany(locale, loader);
    }

    /*************************************************************/
    /* Locale Modules                                            */
    /*************************************************************/

    /**
     * <p>Performe a module action.</p>
     *
     * @param inter The interpreter.
     * @param obj   The source name.
     * @param opt   The option list.
     * @throws InterpreterMessage   Shit happens.
     * @throws InterpreterException Shit happens.
     */
    public static void sysModuleAction(Interpreter inter,
                                       TermAtomic obj, Object opt)
            throws InterpreterMessage, InterpreterException {
        try {
            Engine engine = inter.getEngine();
            LoadForce opts2 = new LoadForce();
            opts2.decodeLoadForce(
                    AbstractTerm.getSkel(opt),
                    AbstractTerm.getDisplay(opt), engine);
            SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(
                    AbstractTerm.getSkel(obj),
                    AbstractTerm.getDisplay(obj));
            AbstractSource source = (sa.scope != null ? sa.scope : engine.store.user);
            opts2.makeForce(source, sa.fun, engine);
        } catch (EngineMessage x) {
            throw new InterpreterMessage(x);
        } catch (EngineException x) {
            throw new InterpreterException(x);
        }
    }

    /**
     * <p>Retrieve the top of the module stack.</p>
     *
     * @param inter The interpreter.
     * @return The top of the module stack, or null.
     */
    public static TermAtomic sysPeekStack(Interpreter inter) {
        Engine engine = inter.getEngine();
        AbstractSource src = engine.visor.peekStack();
        return (src != null ? new TermAtomic(src.getPathAtom()) : null);
    }

    /**
     * <p>Retrieve the size of the module stack.</p>
     *
     * @param inter The interpreter.
     * @return The size of the module stack.
     */
    public static int sysCountStack(Interpreter inter) {
        Engine engine = inter.getEngine();
        return engine.visor.countStack();
    }

}
