package jekpro.model.rope;

import jekpro.frequent.system.ForeignThread;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.builtin.InterfaceInit;
import jekpro.model.inter.Engine;
import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.reference.reflect.PropertySource;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.array.Types;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;
import matula.util.data.ListArray;
import matula.util.data.MapEntry;
import matula.util.system.OpenOpts;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.Locale;
import java.util.Properties;
import java.util.concurrent.TimeUnit;

/**
 * <p>File load and unload options.</p>
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
public final class LoadOpts extends LoadForce {
    public final static String OP_MASK = "mask";

    private final static String OP_COND_ON = "on";
    private final static String OP_COND = "condition";

    public final static String OP_PREFIX_LIBRARY = "library";
    public final static String OP_PREFIX_FOREIGN = "foreign";
    public final static String OP_PREFIX_VERBATIM = "verbatim";
    public final static String OP_PREFIX_RESOURCE = "resource";

    public final static String OP_PACKAGE = "package";
    public final static String OP_USE_PACKAGE = "use_package";

    public final static String OP_VERBOSE_DETAILS = "details";
    public final static String OP_VERBOSE_SUMMARY = "summary";
    private final static String OP_VERBOSE = "verbose";

    private final static int COND_ON = 1;

    public final static int VERBOSE_SUMMARY = 1;
    public final static int VERBOSE_DETAILS = 2;

    public static final int MASK_LOAD_MASK = 0x00002000;

    public static final int MASK_LOAD_SMRY = 0x00010000;
    public static final int MASK_LOAD_DTLS = 0x00020000;
    public static final int MASK_LOAD_COND = 0x00040000;

    private InterfaceInit init;

    /**
     * <p>Retrieve the init.</p>
     *
     * @return The init.
     */
    public InterfaceInit getInit() {
        return init;
    }

    /**
     * <p>Set the init.</p>
     *
     * @param i The init.
     */
    public void setInit(InterfaceInit i) {
        init = i;
    }

    /*******************************************************************/
    /* Source Load                                                     */
    /*******************************************************************/

    /**
     * <p>Make the load.</p>
     *
     * @param scope The call-site, non-null.
     * @param key   The source key.
     * @param en    The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final AbstractSource makeLoad(AbstractSource scope, String key,
                                         Engine en)
            throws EngineMessage, EngineException {
        scope.setTiming(scope.getTiming() + System.currentTimeMillis());
        LoadOpts backcond = en.visor.cond;
        StoreKey backlastsk = en.visor.lastsk;
        if (en.visor.cond == null) {
            setVisited(new ListArray<>());
            en.visor.cond = this;
        }
        boolean rscs = (getFlags() & LoadForce.MASK_LOAD_RSCS) != 0;
        AbstractSource source;
        try {
            source = scope.getStore().getSourceDefined(key, rscs);
            int f = addDeps(source, scope);
            CachePredicate.notifyImportvers(scope, f);
            if (!en.visor.cond.getVisited().contains(source)) {
                en.visor.cond.getVisited().add(source);
                boolean cond = ((en.visor.cond.getFlags() & LoadOpts.MASK_LOAD_COND) != 0);
                Reader reader = source.openReader(cond, this);
                if (reader != null) {
                    performEnsureLoaded(reader, source, en);
                    if ((en.visor.cond.getFlags() & LoadOpts.MASK_LOAD_MASK) != 0)
                        source.setBit(AbstractSource.MASK_SRC_PREL);
                    source.closeReader(reader);
                } else {
                    performReplay(source, en);
                }
            }
            if (backcond == null) {
                purgeSources(en);
                verboseSummary(en);
            }
        } catch (EngineMessage x) {
            en.visor.cond = backcond;
            en.visor.lastsk = backlastsk;
            scope.setTiming(scope.getTiming() - System.currentTimeMillis());
            throw x;
        } catch (EngineException x) {
            en.visor.cond = backcond;
            en.visor.lastsk = backlastsk;
            scope.setTiming(scope.getTiming() - System.currentTimeMillis());
            throw x;
        }
        en.visor.cond = backcond;
        en.visor.lastsk = backlastsk;
        scope.setTiming(scope.getTiming() - System.currentTimeMillis());
        return source;
    }

    /**
     * <p>Perform a reload.</p>
     *
     * @param reader The reader.
     * @param source The source.
     * @param en     The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private void performEnsureLoaded(Reader reader,
                                     AbstractSource source,
                                     Engine en)
            throws EngineMessage, EngineException {
        try {
            /* wait for complete source */
            if (!source.getWrite().tryLock(source.getStore().foyer.timeout, TimeUnit.MILLISECONDS))
                throw new EngineMessage(EngineMessage.limitError(
                        EngineMessage.OP_LIMIT_DEADLOCK_TIMEOUT));
        } catch (InterruptedException x) {
            throw (EngineMessage) ForeignThread.sysThreadClear();
        }
        ListArray<AbstractSource> backmodstack = en.visor.modstack;
        en.visor.modstack = null;
        en.visor.pushStack(source);
        try {
            source.setTiming(-System.currentTimeMillis());
            if ((source.getBits() & AbstractSource.MASK_SRC_SCND) != 0)
                source.clearModule();
            InterfaceInit init = getInit();
            if (init != null)
                init.init(source, en);
            source.loadModule(reader, en);
            source.checkModule(reader, en);
            LoadForce.checkModuleEnd(en);
            source.setBit(AbstractSource.MASK_SRC_SCND);
            source.setTiming(source.getTiming() + System.currentTimeMillis());
            en.visor.cond.verboseConsult(reader, source, en);
        } catch (EngineMessage x) {
            LoadForce.undoNonEmptyStack(en);
            source.getWrite().unlock();
            en.visor.modstack = backmodstack;
            throw x;
        } catch (EngineException x) {
            LoadForce.undoNonEmptyStack(en);
            source.getWrite().unlock();
            en.visor.modstack = backmodstack;
            throw x;
        }
        LoadForce.undoNonEmptyStack(en);
        source.getWrite().unlock();
        en.visor.modstack = backmodstack;
    }

    /**
     * <p>Peform the deps.</p>
     *
     * @param source The source.
     * @param en     The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private void performReplay(AbstractSource source,
                               Engine en)
            throws EngineException, EngineMessage {
        MapEntry<AbstractSource, Integer>[] deps;
        try {
            /* wait for complete source */
            if (!source.getRead().tryLock(source.getStore().foyer.timeout, TimeUnit.MILLISECONDS))
                throw new EngineMessage(EngineMessage.limitError(
                        EngineMessage.OP_LIMIT_DEADLOCK_TIMEOUT));
            try {
                deps = source.snapshotDeps();
            } finally {
                source.getRead().unlock();
            }
        } catch (InterruptedException x) {
            throw (EngineMessage) ForeignThread.sysThreadClear();
        }
        LoadOpts opts = null;
        for (int i = 0; i < deps.length; i++) {
            MapEntry<AbstractSource, Integer> dep = deps[i];
            int flags = dep.value.intValue();
            int f = 0;
            if ((flags & AbstractSource.MASK_IMPT_AUTO) != 0)
                f |= LoadForce.MASK_LOAD_AUTO;
            if ((flags & AbstractSource.MASK_IMPT_MODL) != 0)
                f |= LoadForce.MASK_LOAD_MODL;
            if ((flags & AbstractSource.MASK_IMPT_REEX) != 0)
                f |= LoadForce.MASK_LOAD_REEX;
            if ((flags & AbstractSource.MASK_IMPT_RSCS) != 0)
                f |= LoadForce.MASK_LOAD_RSCS;
            if ((flags & AbstractSource.MASK_IMPT_HOFL) != 0)
                f |= LoadForce.MASK_LOAD_HOFL;
            if ((flags & AbstractSource.MASK_IMPT_PAIM) != 0)
                f |= LoadForce.MASK_LOAD_PAIM;
            if (f == 0)
                continue;
            if (opts == null)
                opts = new LoadOpts();
            opts.setFlags(f);
            try {
                opts.makeLoad(source, dep.key.getPath(), en);
            } catch (EngineMessage x) {
                EngineException y = new EngineException(x,
                        EngineException.fetchStack(en));
                SpecialLoad.systemConsultBreak(y, en);
            } catch (EngineException x) {
                SpecialLoad.systemConsultBreak(x, en);
            }
        }
    }

    /************************************************************/
    /* Make Unload                                              */
    /************************************************************/

    /**
     * <p>Make the unload.</p>
     *
     * @param scope The scope.
     * @param key   The source key.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    public void makeUnload(AbstractSource scope, String key,
                           Engine en)
            throws EngineMessage, EngineException {
        if (scope == null)
            scope = en.store.user;
        LoadOpts backcond = en.visor.cond;
        try {
            if (en.visor.cond == null) {
                setVisited(new ListArray<>());
                en.visor.cond = this;
            }
            AbstractSource source = scope.getStore().getSource(key);
            if (source != null) {
                int f = removeDeps(source, scope);
                CachePredicate.notifyImportvers(scope, f);
            }
            if (backcond == null) {
                purgeSources(en);
                verboseSummary(en);
            }
        } catch (EngineMessage x) {
            en.visor.cond = backcond;
            throw x;
        }
        en.visor.cond = backcond;
    }

    /**
     * <p>Register the dependencies.</p>
     *
     * @param source The source.
     * @param scope  The scope-.
     */
    private int removeDeps(AbstractSource source, AbstractSource scope) {
        int f = 0;
        if ((getFlags() & LoadForce.MASK_LOAD_AUTO) != 0)
            f |= AbstractSource.MASK_IMPT_AUTO;
        if ((getFlags() & LoadForce.MASK_LOAD_MODL) != 0)
            f |= AbstractSource.MASK_IMPT_MODL;
        if ((getFlags() & LoadForce.MASK_LOAD_REEX) != 0)
            f |= AbstractSource.MASK_IMPT_REEX;
        if ((getFlags() & LoadForce.MASK_LOAD_RSCS) != 0)
            f |= AbstractSource.MASK_IMPT_RSCS;
        if ((getFlags() & LoadForce.MASK_LOAD_HOFL) != 0)
            f |= AbstractSource.MASK_IMPT_HOFL;
        if ((getFlags() & LoadForce.MASK_LOAD_PAIM) != 0)
            f |= AbstractSource.MASK_IMPT_PAIM;
        f = scope.removeDep(source, f);
        return f;
    }

    /************************************************************/
    /* Purge Sources                                            */
    /************************************************************/

    /**
     * <p>Purge unused files by mark sweep algorithm.</p>
     * <p>Engine stamp should be set to a value different of -1.</p>
     *
     * @param en The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    protected final void purgeSources(Engine en)
            throws EngineMessage, EngineException {

        Store[] stores = en.store.foyer.snapshotStores();
        for (int i = 0; i < stores.length; i++) {
            Store store = stores[i];
            AbstractSource[] roots = store.snapshotRoots();
            for (int j = 0; j < roots.length; j++) {
                AbstractSource source = roots[j];
                performMark(source);
            }
        }

        for (int i = 0; i < stores.length; i++) {
            Store store = stores[i];
            MapEntry<String, AbstractSource>[] sources = store.snapshotSources();
            for (int j = 0; j < sources.length; j++) {
                AbstractSource src = sources[j].value;
                if (getVisited().contains(src))
                    continue;
                performClear(src, en.visor.curoutput);
                store.removeSource(src.getPath());
            }
        }

        setVisited(null);
    }

    /**
     * <p>Mark a path and its dependent paths.</p>
     * <p>Engine stamp should be set to a value different of -1.</p>
     *
     * @param source The source.
     * @throws EngineMessage Shit happens.
     */
    private void performMark(AbstractSource source)
            throws EngineMessage {
        if (getVisited().contains(source))
            return;
        getVisited().add(source);
        MapEntry<AbstractSource, Integer>[] deps;
        try {
            /* wait for complete source */
            if (!source.getRead().tryLock(source.getStore().foyer.timeout, TimeUnit.MILLISECONDS))
                throw new EngineMessage(EngineMessage.limitError(
                        EngineMessage.OP_LIMIT_DEADLOCK_TIMEOUT));
            try {
                deps = source.snapshotDeps();
            } finally {
                source.getRead().unlock();
            }
        } catch (InterruptedException x) {
            throw (EngineMessage) ForeignThread.sysThreadClear();
        }
        for (int i = 0; i < deps.length; i++) {
            MapEntry<AbstractSource, Integer> dep = deps[i];
            performMark(dep.key);
        }
    }

    /**
     * <p>Perform the unload.</p>
     *
     * @param source The source.
     * @param out    The output stream.
     * @throws EngineMessage Shit happens.
     */
    public final void performClear(AbstractSource source,
                                   Object out)
            throws EngineMessage, EngineException {
        try {
            /* unload with locking */
            if (!source.getWrite().tryLock(source.getStore().foyer.timeout, TimeUnit.MILLISECONDS))
                throw new EngineMessage(EngineMessage.limitError(
                        EngineMessage.OP_LIMIT_DEADLOCK_TIMEOUT));
            try {
                source.setTiming(-System.currentTimeMillis());
                if ((source.getBits() & AbstractSource.MASK_SRC_SCND) != 0)
                    source.clearModule();
                source.setTiming(source.getTiming() + System.currentTimeMillis());
                verboseUnload(source, out);
            } finally {
                source.getWrite().unlock();
            }
        } catch (InterruptedException x) {
            throw (EngineMessage) ForeignThread.sysThreadClear();
        }
    }

    /************************************************************/
    /* Load Options                                             */
    /************************************************************/

    /**
     * <p>Decode the consult options.</p>
     *
     * @param t  The options skel.
     * @param d  The options display.
     * @param en The engine.
     * @throws EngineMessage Type Error.
     */
    public final void decodeLoadOpts(Object t, Display d, Engine en)
            throws EngineMessage {
        try {
            int flags = en.store.foyer.getBits();
            if ((flags & Foyer.MASK_FOYER_SMRY) != 0) {
                setFlags(getFlags() | LoadOpts.MASK_LOAD_SMRY);
            } else {
                setFlags(getFlags() & ~LoadOpts.MASK_LOAD_SMRY);
            }
            if ((flags & Foyer.MASK_FOYER_DTLS) != 0) {
                setFlags(getFlags() | LoadOpts.MASK_LOAD_DTLS);
            } else {
                setFlags(getFlags() & ~LoadOpts.MASK_LOAD_DTLS);
            }
            en.skel = t;
            en.display = d;
            en.deref();
            while (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 2 &&
                    ((SkelCompound) en.skel).sym.fun.equals(Foyer.OP_CONS)) {
                Object[] mc = ((SkelCompound) en.skel).args;
                d = en.display;
                en.skel = mc[0];
                en.deref();
                if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(LoadOpts.OP_MASK)) {
                    if (AbstractFlag.atomToSwitch(((SkelCompound) en.skel).args[0], en.display)) {
                        setFlags(getFlags() | LoadOpts.MASK_LOAD_MASK);
                    } else {
                        setFlags(getFlags() & ~LoadOpts.MASK_LOAD_MASK);
                    }
                } else if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(LoadOpts.OP_COND)) {
                    int cond = LoadOpts.atomToCond(((SkelCompound) en.skel).args[0], en.display);
                    if ((cond & LoadOpts.COND_ON) != 0) {
                        setFlags(getFlags() | LoadOpts.MASK_LOAD_COND);
                    } else {
                        setFlags(getFlags() & ~LoadOpts.MASK_LOAD_COND);
                    }
                } else if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(LoadOpts.OP_VERBOSE)) {
                    int verb = LoadOpts.atomToVerbose(((SkelCompound) en.skel).args[0], en.display);
                    if ((verb & LoadOpts.VERBOSE_DETAILS) != 0) {
                        setFlags(getFlags() | LoadOpts.MASK_LOAD_DTLS);
                    } else {
                        setFlags(getFlags() & ~LoadOpts.MASK_LOAD_DTLS);
                    }
                    if ((verb & LoadOpts.VERBOSE_SUMMARY) != 0) {
                        setFlags(getFlags() | LoadOpts.MASK_LOAD_SMRY);
                    } else {
                        setFlags(getFlags() & ~LoadOpts.MASK_LOAD_SMRY);
                    }
                } else if (en.skel instanceof SkelCompound &&
                        ((SkelCompound) en.skel).args.length == 1 &&
                        ((SkelCompound) en.skel).sym.fun.equals(PropertySource.OP_SYS_LINK)) {
                    int link = LoadOpts.atomToLink(((SkelCompound) en.skel).args[0], en.display);
                    if ((link & LINK_SYS_AUTO_LOAD) != 0) {
                        setFlags(getFlags() | LoadForce.MASK_LOAD_AUTO);
                    } else {
                        setFlags(getFlags() & ~LoadForce.MASK_LOAD_AUTO);
                    }
                    if ((link & LINK_USE_MODULE) != 0) {
                        setFlags(getFlags() | LoadForce.MASK_LOAD_MODL);
                    } else {
                        setFlags(getFlags() & ~LoadForce.MASK_LOAD_MODL);
                    }
                    if ((link & LINK_REEXPORT) != 0) {
                        setFlags(getFlags() | LoadForce.MASK_LOAD_REEX);
                    } else {
                        setFlags(getFlags() & ~LoadForce.MASK_LOAD_REEX);
                    }
                    if ((link & LINK_SYS_LOAD_RESOURCE) != 0) {
                        setFlags(getFlags() | LoadForce.MASK_LOAD_RSCS);
                    } else {
                        setFlags(getFlags() & ~LoadForce.MASK_LOAD_RSCS);
                    }
                } else {
                    EngineMessage.checkInstantiated(en.skel);
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_CONSULT_OPTION,
                            en.skel), en.display);
                }
                en.skel = mc[1];
                en.display = d;
                en.deref();
            }
            if (en.skel instanceof SkelAtom &&
                    ((SkelAtom) en.skel).fun.equals(Foyer.OP_NIL)) {
                /* */
            } else {
                EngineMessage.checkInstantiated(en.skel);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_LIST,
                        en.skel), en.display);
            }
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
        }
    }

    /**
     * <p>Convert an atom to a condition. Will throw exception
     * when the atom is not well formed.</p>
     *
     * @param m The link skeleton.
     * @param d The link display.
     * @return The link.
     * @throws EngineMessage Shit happens.
     */
    private static int atomToCond(Object m, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        if (fun.equals(OP_COND_ON)) {
            return COND_ON;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_ACTION_OPTION, m), d);
        }
    }

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
        if (fun.equals(AbstractFlag.OP_OFF)) {
            return 0;
        } else if (fun.equals(OP_VERBOSE_DETAILS)) {
            return VERBOSE_DETAILS;
        } else if (fun.equals(OP_VERBOSE_SUMMARY)) {
            return VERBOSE_SUMMARY;
        } else if (fun.equals(AbstractFlag.OP_ON)) {
            return VERBOSE_DETAILS + VERBOSE_SUMMARY;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_VERBOSE_OPTION, m), d);
        }
    }

    /************************************************************/
    /* Logging Routines                                         */
    /************************************************************/

    /**
     * <p>Prose after consulting a stream.</p>
     *
     * @param reader The reader.
     * @param source The source.
     * @param en     The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final void verboseConsult(Reader reader, AbstractSource source,
                                     Engine en)
            throws EngineMessage, EngineException {
        try {
            if ((getFlags() & LoadOpts.MASK_LOAD_DTLS) != 0) {
                SkelCompound sk = new SkelCompound(new SkelAtom("file_lines_ms"),
                        new SkelAtom(OpenOpts.getPath(reader)),
                        Integer.valueOf(OpenOpts.getLineNumber(reader)),
                        TermAtomic.normBigInteger(source.getTiming()));
                Locale locale = en.store.foyer.locale;
                Properties error = EngineMessage.getErrorLang(locale, en.store);
                String text = EngineMessage.messageMake(sk, Display.DISPLAY_CONST, locale, error, en);
                Object obj = en.visor.curoutput;
                checkTextWrite(obj);
                Writer wr = (Writer) obj;
                wr.write(text);
                wr.write("\n");
                wr.flush();
            }
            setTiming(getTiming() + source.getTiming());
            setConsultCount(getConsultCount() + 1);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Prose after consulting a stream.</p>
     *
     * @param source The source.
     * @param out    The output stream.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final void verboseUnload(AbstractSource source,
                                    Object out)
            throws EngineMessage, EngineException {
        try {
            if ((getFlags() & LoadOpts.MASK_LOAD_DTLS) != 0) {
                SkelCompound sk = new SkelCompound(new SkelAtom("file_ms"),
                        source.getPathAtom(),
                        TermAtomic.normBigInteger(source.getTiming()));
                Locale locale = source.getStore().foyer.locale;
                Properties error = EngineMessage.getErrorLang(locale, source.getStore());
                String text = EngineMessage.messageMake(sk, Display.DISPLAY_CONST, locale, error, null);
                checkTextWrite(out);
                Writer wr = (Writer) out;
                wr.write(text);
                wr.write("\n");
                wr.flush();
            }
            setTiming(getTiming() + source.getTiming());
            setUnloadCount(getUnloadCount() + 1);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Prose after a load or unload directive.</p>
     *
     * @param en The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public void verboseSummary(Engine en)
            throws EngineMessage, EngineException {
        try {
            if ((getFlags() & LoadOpts.MASK_LOAD_SMRY) != 0) {
                SkelCompound sk = new SkelCompound(new SkelAtom("load_unload_ms"),
                        Integer.valueOf(getConsultCount()),
                        Integer.valueOf(getUnloadCount()),
                        TermAtomic.normBigInteger(getTiming()));
                Locale locale = en.store.foyer.locale;
                Properties error = EngineMessage.getErrorLang(locale, en.store);
                String text = EngineMessage.messageMake(sk, Display.DISPLAY_CONST, locale, error, en);
                Object obj = en.visor.curoutput;
                checkTextWrite(obj);
                Writer wr = (Writer) obj;
                wr.write(text);
                wr.write("\n");
                wr.flush();
            }
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /*************************************************************/
    /* Check Stream Type                                         */
    /*************************************************************/

    /**
     * <p>Check whether the object is a text output stream.</p>
     *
     * @param obj The object.
     * @throws EngineMessage Shit happens.
     */
    public static void checkTextWrite(Object obj) throws EngineMessage {
        if (!(obj instanceof Writer)) {
            if (!(obj instanceof OutputStream)) {
                throw new EngineMessage(EngineMessage.permissionError(
                        EngineMessage.OP_PERMISSION_OUTPUT,
                        EngineMessage.OP_PERMISSION_STREAM,
                        (obj != null ? obj : new SkelAtom(AbstractFlag.OP_NULL))));
            } else {
                throw new EngineMessage(EngineMessage.permissionError(
                        EngineMessage.OP_PERMISSION_OUTPUT,
                        EngineMessage.OP_PERMISSION_BINARY_STREAM,
                        (obj != null ? obj : new SkelAtom(AbstractFlag.OP_NULL))));
            }
        }
    }

    /**
     * <p>Check whether the object is a binary output stream.</p>
     *
     * @param obj The object.
     * @throws EngineMessage Shit happens.
     */
    public static void checkBinaryWrite(Object obj) throws EngineMessage {
        if (!(obj instanceof OutputStream)) {
            if (!(obj instanceof Writer)) {
                throw new EngineMessage(EngineMessage.permissionError(
                        EngineMessage.OP_PERMISSION_OUTPUT,
                        EngineMessage.OP_PERMISSION_STREAM, obj));
            } else {
                throw new EngineMessage(EngineMessage.permissionError(
                        EngineMessage.OP_PERMISSION_OUTPUT,
                        EngineMessage.OP_PERMISSION_TEXT_STREAM, obj));
            }
        }
    }

}
