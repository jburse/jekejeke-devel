package jekpro.model.rope;

import jekpro.frequent.system.ForeignThread;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.reference.reflect.PropertySource;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.ListArray;

import java.util.concurrent.TimeUnit;

/**
 * <p>Module begin and end options.</p>
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
public class LoadForce {
    public final static String OP_ACTION_BEGIN_MODULE = "begin_module";
    public final static String OP_ACTION_END_MODULE = "end_module";
    public final static String OP_ACTION = "action";

    public final static String OP_SYS_LINK_USE_MODULE = "use_module";
    public final static String OP_SYS_LINK_REEXPORT = "reexport";
    public final static String OP_SYS_LINK_SYS_AUTO_LOAD = "sys_auto_load";
    public final static String OP_SYS_LINK_SYS_LOAD_RESOURCE = "sys_load_resource";
    public final static String OP_SYS_LINK_SYS_HOME_FILE = "sys_home_file";
    public final static String OP_SYS_LINK_SYS_PARENT_IMPORT = "sys_parent_import";

    public static final int MASK_LOAD_BEGM = 0x00000001;

    public static final int MASK_LOAD_AUTO = 0x00000010;
    public static final int MASK_LOAD_MODL = 0x00000020;
    public static final int MASK_LOAD_REEX = 0x00000040;

    public static final int MASK_LOAD_RSCS = 0x00000100;
    public static final int MASK_LOAD_HOFL = 0x00000200;
    public static final int MASK_LOAD_PAIM = 0x00000400;

    protected final static int LINK_SYS_AUTO_LOAD = 1;
    protected final static int LINK_USE_MODULE = 2;
    protected final static int LINK_REEXPORT = 4;
    protected final static int LINK_SYS_LOAD_RESOURCE = 16;

    private final static int ACTION_BEGIN_MODULE = 1;

    private int flags;
    private ListArray<AbstractSource> visited = new ListArray<>();
    private long timing;
    private int consultcount;
    private int unloadcount;

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The flags.
     */
    public int getFlags() {
        return flags;
    }

    /**
     * <p>Set the flags.</p>
     *
     * @param f The flags.
     */
    public void setFlags(int f) {
        flags = f;
    }

    /**
     * <p>Retrieve the visited list.</p>
     *
     * @return The visited list.
     */
    public ListArray<AbstractSource> getVisited() {
        return visited;
    }

    /**
     * <p>Set the visited list.</p>
     *
     * @param v The visited list.
     */
    public void setVisited(ListArray<AbstractSource> v) {
        visited = v;
    }

    /**
     * <p>Retrieve the timing.</p>
     *
     * @return The timing.
     */
    public long getTiming() {
        return timing;
    }

    /**
     * <p>Set the timing.</p>
     *
     * @param t The timing
     */

    public void setTiming(long t) {
        timing = t;
    }

    /**
     * <p>Retrieve the load count.</p>
     *
     * @return The load count.
     */
    public int getConsultCount() {
        return consultcount;
    }

    /**
     * <p>Set the load count.</p>
     *
     * @param c The load count.
     */
    public void setConsultCount(int c) {
        consultcount = c;
    }

    /**
     * <p>Retrieve the unload count.</p>
     *
     * @return The unload count.
     */
    public int getUnloadCount() {
        return unloadcount;
    }

    /**
     * <p>Set the unload count.</p>
     *
     * @param c The unload count.
     */
    public void setUnloadCount(int c) {
        unloadcount = c;
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
    public final AbstractSource makeForce(AbstractSource scope, String key,
                                          Engine en)
            throws EngineMessage, EngineException {
        boolean rscs = (getFlags() & LoadForce.MASK_LOAD_RSCS) != 0;
        AbstractSource source = scope.getStore().getSourceDefined(key, rscs);
        if ((getFlags() & LoadForce.MASK_LOAD_BEGM) != 0) {
            int f = addDeps(source, scope);
            CachePredicate.notifyImportvers(scope, f);
            if (en.visor.cond != null) {
                if (!en.visor.cond.getVisited().contains(source))
                    en.visor.cond.getVisited().add(source);
            }
            LoadForce.performBeginModule(source, en);
        } else {
            LoadForce.performEndModule(source, en);
        }
        return source;
    }

    /**
     * <p>Register the dependencies.</p>
     *
     * @param source The source.
     * @param scope  The scope.
     */
    protected final int addDeps(AbstractSource source, AbstractSource scope) {
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
        if (f == 0)
            return 0;
        f = scope.addDep(source, f);
        return f;
    }

    /**
     * <p>Perform a reload.</p>
     *
     * @param source The source.
     * @param en     The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void performBeginModule(AbstractSource source,
                                           Engine en)
            throws EngineMessage {
        AbstractSource parent = LoadForce.getParent(source);
        if (parent == null)
            throw new EngineMessage(EngineMessage.syntaxError(
                    EngineMessage.OP_SYNTAX_NOT_LOCALE));

        AbstractSource peek = en.visor.peekStack();
        if (peek != parent)
            throw new EngineMessage(EngineMessage.syntaxError(
                    EngineMessage.OP_SYNTAX_WRONG_PARENT));

        try {
            /* wait for complete source */
            if (!source.getWrite().tryLock(source.getStore().foyer.timeout, TimeUnit.MILLISECONDS))
                throw new EngineMessage(EngineMessage.limitError(
                        EngineMessage.OP_LIMIT_DEADLOCK_TIMEOUT));
        } catch (InterruptedException x) {
            throw (EngineMessage) ForeignThread.sysThreadClear();
        }
        en.visor.pushStack(source);
        if (en.visor.cond != null) {
            if ((source.getBits() & AbstractSource.MASK_SRC_SCND) != 0)
                source.clearModule();
        }

        int f = source.addDep(peek, AbstractSource.MASK_IMPT_PAIM);
        CachePredicate.notifyImportvers(source, f);
        source.utilsingle = parent.utilsingle;
        source.utildouble = parent.utildouble;
        source.utilback = parent.utilback;
    }

    /**
     * <p>Retrieve the direct parent.</p>
     *
     * @param source The source.
     * @return The direct parent.
     */
    private static AbstractSource getParent(AbstractSource source) {
        String path = source.getPath();
        int k = path.lastIndexOf(CacheSubclass.OP_CHAR_SYN);
        if (k == -1)
            return null;

        path = path.substring(0, k);
        return source.getStore().getSource(path);
    }

    /**
     * <p>Perform a reload.</p>
     *
     * @param source The source.
     * @param en     The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static void performEndModule(AbstractSource source,
                                        Engine en)
            throws EngineMessage, EngineException {
        if (en.visor.modstack.size() < 2)
            throw new EngineMessage(EngineMessage.syntaxError(
                    EngineMessage.OP_SYNTAX_SUPERFLOUS_END));

        try {
            if (en.visor.cond != null) {
                source.checkModule(null, en);
                source.setBit(AbstractSource.MASK_SRC_SCND);
            }
        } catch (EngineMessage x) {
            en.visor.popStack();
            source.getWrite().unlock();
            throw x;
        } catch (EngineException x) {
            en.visor.popStack();
            source.getWrite().unlock();
            throw x;
        }
        en.visor.popStack();
        source.getWrite().unlock();
    }

    /************************************************************/
    /* Check & Undo                                             */
    /************************************************************/

    /**
     * <p>Perform a style check on the given module.</p>
     * <p>This check is performed at the end of loading a module.</p>
     *
     * @param en The engine.
     * @throws EngineMessage Printing error.
     */
    public static void checkModuleEnd(Engine en)
            throws EngineMessage, EngineException {
        try {
            checkModuleStackEmpty(en);
        } catch (EngineMessage x) {
            EngineException y = new EngineException(x,
                    EngineException.fetchStack(en),
                    EngineException.OP_WARNING);
            y.printStackTrace(en);
        }
    }

    /**
     * <p>Perform the empty check.</p>
     *
     * @param en The engine.
     * @throws EngineMessage The warning.
     */
    private static void checkModuleStackEmpty(Engine en)
            throws EngineMessage {
        if (en.visor.modstack.size() >= 2)
            throw new EngineMessage(EngineMessage.syntaxError(
                    EngineMessage.OP_SYNTAX_END_MISSING));
    }

    /**
     * <p>Undo superflous stack elements.</p>
     *
     * @param en The engine.
     */
    public static void undoNonEmptyStack(Engine en)
            throws EngineException, EngineMessage {
        while (en.visor.modstack.size() >= 2) {
            AbstractSource src = en.visor.peekStack();
            performEndModule(src, en);
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
     * @throws EngineMessage Shit happens.
     */
    public final void decodeLoadForce(Object t, Display d, Engine en)
            throws EngineMessage {
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
                    ((SkelCompound) en.skel).sym.fun.equals(OP_ACTION)) {
                int action = atomToAction(((SkelCompound) en.skel).args[0], en.display, en);
                if ((action & ACTION_BEGIN_MODULE) != 0) {
                    setFlags(getFlags() | MASK_LOAD_BEGM);
                } else {
                    setFlags(getFlags() & ~MASK_LOAD_BEGM);
                }
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(PropertySource.OP_SYS_LINK)) {
                int link = atomToLink(((SkelCompound) en.skel).args[0], en.display);
                if ((link & LINK_SYS_AUTO_LOAD) != 0) {
                    setFlags(getFlags() | MASK_LOAD_AUTO);
                } else {
                    setFlags(getFlags() & ~MASK_LOAD_AUTO);
                }
                if ((link & LINK_USE_MODULE) != 0) {
                    setFlags(getFlags() | MASK_LOAD_MODL);
                } else {
                    setFlags(getFlags() & ~MASK_LOAD_MODL);
                }
                if ((link & LINK_REEXPORT) != 0) {
                    setFlags(getFlags() | MASK_LOAD_REEX);
                } else {
                    setFlags(getFlags() & ~MASK_LOAD_REEX);
                }
                if ((link & LINK_SYS_LOAD_RESOURCE) != 0) {
                    setFlags(getFlags() | MASK_LOAD_RSCS);
                } else {
                    setFlags(getFlags() & ~MASK_LOAD_RSCS);
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
    }

    /**
     * <p>Convert an atom to a link. Will throw exception
     * when the atom is not well formed.</p>
     *
     * @param m The link skeleton.
     * @param d The link display.
     * @return The link.
     * @throws EngineMessage Shit happens.
     */
    protected static int atomToLink(Object m, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        if (fun.equals(OP_SYS_LINK_SYS_AUTO_LOAD)) {
            return LINK_SYS_AUTO_LOAD;
        } else if (fun.equals(OP_SYS_LINK_USE_MODULE)) {
            return LINK_SYS_AUTO_LOAD | LINK_USE_MODULE;
        } else if (fun.equals(OP_SYS_LINK_REEXPORT)) {
            return LINK_SYS_AUTO_LOAD | LINK_USE_MODULE | LINK_REEXPORT;
        } else if (fun.equals(OP_SYS_LINK_SYS_LOAD_RESOURCE)) {
            return LINK_SYS_LOAD_RESOURCE;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_LINK_OPTION, m), d);
        }
    }

    /**
     * <p>Convert an atom to an action. Will throw exception
     * when the atom is not well formed.</p>
     *
     * @param m  The link skeleton.
     * @param d  The link display.
     * @param en The engine.
     * @return The link.
     * @throws EngineMessage Shit happens.
     */
    private static int atomToAction(Object m, Display d, Engine en)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        if (fun.equals(OP_ACTION_BEGIN_MODULE)) {
            return ACTION_BEGIN_MODULE;
        } else if (fun.equals(OP_ACTION_END_MODULE)) {
            return 0;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_ACTION_OPTION, en.skel));
        }
    }

}