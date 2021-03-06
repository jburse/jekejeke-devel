package jekpro.tools.call;

import jekpro.frequent.system.ForeignThread;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.rope.LoadForce;
import jekpro.model.rope.LoadOpts;
import jekpro.tools.foreign.AutoClass;
import jekpro.tools.foreign.LookupBinary;
import jekpro.tools.term.SkelAtom;
import matula.util.config.AbstractRuntime;

import java.io.Reader;
import java.io.StringReader;

/**
 * <p>The synthetic source class for Java classes.</p>
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
public abstract class AbstractAuto extends AbstractSource {
    private Class auto;

    /**
     * <p>Create a source from path.</p>
     *
     * @param p The path.
     */
    public AbstractAuto(String p) {
        super(p);
    }

    /**
     * <p>Retrieve the auto loaded class..</p>
     *
     * @return The class, or null.
     */
    public Class getAuto() {
        return auto;
    }

    /**
     * <p>Set the auto loaded class.</p>
     *
     * @param a The class, or null.
     */
    public void setAuto(Class a) {
        auto = a;
    }

    /**************************************************************/
    /* Init & Clear Module                                        */
    /**************************************************************/

    /**
     * <p>Compute default package, name and parent.</p>
     */
    public void initSource() {
        String path = LookupBinary.removeClassExt(getPath());
        if (path == null)
            throw new RuntimeException("illegal path");

        /* add package and name */
        if (CacheModule.isOs(path)) {
            addFix(CacheModule.sepDirectory(path), MASK_PCKG_FRGN);
            setName(CacheModule.sepFile(path));
        } else {
            setName(path);
        }

        /* set the full name */
        path = path.replace(CacheModule.OP_CHAR_OS, CachePackage.OP_CHAR_SEG);
        setFullName(path);

        /* change default visibility */
        resetBit(MASK_SRC_VSPU);
    }

    /**
     * <p>Unload the module before a reconsult or purge.</p>
     *
     * @throws EngineMessage Shit happens.
     */
    public void clearModule()
            throws EngineMessage {
        super.clearModule();
        setAuto(null);

        /* clear deps without notify */
        clearDeps(-1);

        /* clear fixes without notify */
        setName(null);
        clearFixes(-1);
    }

    /**************************************************************/
    /* Open & Close Reader                                        */
    /**************************************************************/

    /**
     * <p>Open a read stream.</p>
     *
     * @param if_modified The if modified flag.
     * @param opts        The options.
     * @return The reader or null.
     */
    public Reader openReader(boolean if_modified,
                             LoadOpts opts) {
        if ((getBits() & AbstractSource.MASK_SRC_PREL) != 0) {
            if (if_modified)
                return null;
            if_modified = true;
        }

        if (!if_modified || (getBits() & AbstractSource.MASK_SRC_SCND) == 0) {
            return new StringReader("");
        } else {
            return null;
        }
    }

    /**************************************************************/
    /* Clear, Load & Check Module                                 */
    /**************************************************************/

    /**
     * <p>Consult a verbatim module.</p>
     *
     * @param lr The buffered reader.
     * @param en The interpreter.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public void loadModule(Reader lr, Engine en)
            throws EngineMessage, EngineException {
        Class<?> clazz = LookupBinary.keyToClass(getPath(), en.store);
        if (clazz == null)
            throw new RuntimeException("class missing");
        setAuto(clazz);
    }

    /***************************************************************/
    /* Superclass & Interfaces                                     */
    /***************************************************************/

    /**
     * <p>Reexport the super class of a class.</p>
     *
     * @param en The interpreter.
     * @return The corresponding source.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    protected AutoClass reexportSuperclass(Engine en)
            throws EngineException, EngineMessage {
        Class superclazz = getAuto().getSuperclass();
        if (superclazz == null)
            return null;

        LoadOpts opts = new LoadOpts();
        opts.setFlags(opts.getFlags() | LoadOpts.MASK_LOAD_COND);
        opts.setFlags(opts.getFlags() | LoadForce.MASK_LOAD_AUTO);
        opts.setFlags(opts.getFlags() | LoadForce.MASK_LOAD_MODL);
        opts.setFlags(opts.getFlags() | LoadForce.MASK_LOAD_REEX);
        String key = AbstractRuntime.classToString(superclazz);
        key = key.replace(CachePackage.OP_CHAR_SEG, CacheModule.OP_CHAR_OS);
        key = LookupBinary.addClassExt(key);
        return (AutoClass) opts.makeLoad(this, key, en);
    }

    /**
     * <p>Reexport the interfaces of the class.</p>
     *
     * @param en The interpreter.
     * @return The corresponding sources.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    protected AutoClass[] reexportInterfaces(Engine en)
            throws EngineException, EngineMessage {
        Class[] interfaces = getAuto().getInterfaces();
        AutoClass[] res = new AutoClass[interfaces.length];

        LoadOpts opts = new LoadOpts();
        opts.setFlags(opts.getFlags() | LoadOpts.MASK_LOAD_COND);
        opts.setFlags(opts.getFlags() | LoadForce.MASK_LOAD_AUTO);
        opts.setFlags(opts.getFlags() | LoadForce.MASK_LOAD_MODL);
        opts.setFlags(opts.getFlags() | LoadForce.MASK_LOAD_REEX);
        for (int i = 0; i < interfaces.length; i++) {
            String key = AbstractRuntime.classToString(interfaces[i]);
            key = key.replace(CachePackage.OP_CHAR_SEG, CacheModule.OP_CHAR_OS);
            key = LookupBinary.addClassExt(key);
            res[i] = (AutoClass) opts.makeLoad(this, key, en);
        }
        return res;
    }

    /**************************************************************/
    /* Define preds & Evaluables                                  */
    /**************************************************************/

    /**
     * <p>Make predicate or evaluable function public and override.</p>
     * <p>Will set the predicate also to automatic.</p>
     *
     * @param sa    The name and call-site.
     * @param arity The length.
     * @param virt  The virtual flag.
     * @param en    The interpreter.
     */
    public static Predicate makePublic(SkelAtom sa, int arity,
                                       boolean virt, Engine en)
            throws EngineException, EngineMessage {
        AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
        CachePredicate cp = CachePredicate.getPredicateDefined(sa,
                arity, en, CachePredicate.MASK_CACH_CRTE);
        Predicate pick = cp.pick;
        pick.setBit(Predicate.MASK_PRED_VSPU);
        pick.addDef(src, Predicate.MASK_TRCK_VSPU);
        pick.setBit(Predicate.MASK_PRED_AUTO);
        if (virt)
            pick.setBit(Predicate.MASK_PRED_VIRT);
        return pick;
    }

    /**
     * <p>Make predicate or evaluable function overriden.</p>
     *
     * @param sa   The functor.
     * @param pick The predicate.
     * @param en   The engine.
     * @return The overriden predicate or evaluable function.
     * @throws EngineMessage Shit happens.
     */
    public static Predicate makeOverride(SkelAtom sa,
                                         Predicate pick,
                                         Engine en)
            throws EngineMessage, EngineException {
        AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
        AbstractSource base = CachePredicate.performBase(sa, src, en);
        Predicate over;
        try {
            over = CachePredicate.performOverrides(sa, pick.getArity(), base);
        } catch (InterruptedException x) {
            throw (EngineMessage) ForeignThread.sysThreadClear();
        }
        if (over == null || !CachePredicate.visiblePred(over, src))
            return null;
        pick.addDef(src, Predicate.MASK_TRCK_OVRD);
        return over;
    }

}