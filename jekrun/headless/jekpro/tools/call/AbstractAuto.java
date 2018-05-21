package jekpro.tools.call;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.inter.Usage;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.AbstractStore;
import jekpro.model.pretty.SourceLocal;
import jekpro.model.rope.LoadForce;
import jekpro.model.rope.LoadOpts;
import jekpro.tools.foreign.AutoClass;
import jekpro.tools.foreign.LookupBinary;
import jekpro.tools.proxy.InterfaceHandler;
import jekpro.tools.term.SkelAtom;
import matula.util.wire.AbstractLivestock;
import matula.util.system.AbstractRuntime;

import java.io.Reader;
import java.io.StringReader;

/**
 * <p>The synthetic source class for Java classes.</p>
 *
 * @author Copyright 2015-2017, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.0.9 (a fast and small prolog interpreter)
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
    /* Variation Points                                           */
    /**************************************************************/

    /**
     * <p>Compute default package, name and parent.</p>
     */
    public void initSource() {
        String path = LookupBinary.removeClassExt(getPath());
        if (path == null)
            throw new RuntimeException("illegal path");

        /* add package */
        if (SourceLocal.isOs(path)) {
            addFix(SourceLocal.sepDirectory(path), MASK_PCKG_FRGN);
            setName(SourceLocal.sepFile(path));
        } else {
            setName(path);
        }
        resetBit(MASK_SRC_VSPU);
    }

    /**
     * Remove default package, name and parent.
     */
    public void finiSource() {
        String path = LookupBinary.removeClassExt(getPath());
        if (path == null)
            throw new RuntimeException("illegal path");

        /* remove package */
        if (SourceLocal.isOs(path))
            removeFix(SourceLocal.sepDirectory(path), MASK_PCKG_FRGN);
        setName(null);
    }

    /**
     * <p>Retrieve the invocation handler.</p>
     *
     * @return The invocation handler.
     * @throws EngineMessage Shit happens.
     */
    public InterfaceHandler getHandler() throws EngineMessage {
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_CREATE,
                EngineMessage.OP_PERMISSION_PROXY,
                new SkelAtom(getPath())));
    }

    /**
     * <p>Set the invocation handler.</p>
     *
     * @param h The invocation handler.
     */
    public void setHandler(InterfaceHandler h) {
        /* do nothing */
    }

    /**************************************************************/
    /* Open & Close Reader                                        */
    /**************************************************************/

    /**
     * <p>Open a read stream.</p>
     *
     * @param if_modified The if modified flag.
     * @param opts        The options.
     * @param en          The engine.
     * @return The reader or null.
     */
    public Reader openReader(boolean if_modified,
                             LoadOpts opts, Engine en) {
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
     * <p>Unload the module before a reconsult or purge.</p>
     *
     * @throws EngineMessage Shit happens.
     */
    public void clearModule()
            throws EngineMessage {
        super.clearModule();
        setAuto(null);
    }

    /**
     * <p>Consult a verbatim module.</p>
     *
     * @param en  The interpreter.
     * @param rec The recursion flag.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public void loadModule(Reader lr, Engine en, boolean rec)
            throws EngineMessage, EngineException {
        Class<?> clazz = en.store.foyer.getFactory().keyToClass(getPath(), en.store);
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
    public AutoClass reexportSuperclass(Engine en)
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
        key = key.replace(CachePackage.OP_CHAR_SEG, SourceLocal.OP_CHAR_OS);
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
    public AutoClass[] reexportInterfaces(Engine en)
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
            key = key.replace(CachePackage.OP_CHAR_SEG, SourceLocal.OP_CHAR_OS);
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
    public Predicate makePublic(SkelAtom sa, int arity,
                                boolean virt, Engine en)
            throws EngineException, EngineMessage {
        CachePredicate cp = CachePredicate.getPredicateDefined(sa,
                arity, en, true);
        Predicate pick = cp.pick;
        pick.setBit(Predicate.MASK_PRED_VSPU);
        Usage loc = pick.getUsage(this);
        if (loc != null)
            loc.setBit(Usage.MASK_USE_VSPU);
        pick.setBit(Predicate.MASK_PRED_AUTO);
        if (virt)
            pick.setBit(Predicate.MASK_PRED_VIRT);
        return pick;
    }

    /**
     * <p>Make predicate or evaluable function overriden.</p>
     *
     * @param pick The predicate.
     * @param en   The engine.
     * @return The overriden predicate or evaluable function.
     * @throws EngineMessage Shit happens.
     */
    public Predicate makeOverride(Predicate pick,
                                  Engine en)
            throws EngineMessage, EngineException {
        String fun = pick.getFun();
        AbstractSource base = (CacheFunctor.isQuali(fun) ? CachePredicate.lookupBase(
                CacheFunctor.sepModule(fun), this, en) : this);
        Predicate over;
        try {
            over = CachePredicate.performOverrides(fun, pick.getArity(), this,
                    base);
        } catch (InterruptedException x) {
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        }
        if (over == null || !over.visiblePred(this))
            return null;
        Usage loc = pick.getUsage(this);
        if (loc != null)
            loc.setBit(Usage.MASK_USE_OVRD);
        return over;
    }

}