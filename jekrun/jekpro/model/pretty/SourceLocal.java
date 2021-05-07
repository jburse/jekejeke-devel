package jekpro.model.pretty;

import jekpro.frequent.basic.InterfaceProxyable;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.rope.LoadOpts;
import jekpro.reference.bootload.ForeignPath;
import jekpro.tools.proxy.ProxyHandler;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.MapEntry;

import java.io.IOException;
import java.io.Reader;

/**
 * <p>Specialization of the abstract source class to locale modules.</p>
 *
 * @author Copyright 2015-2018, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.0.9 (a fast and small prolog interpreter)
 */
public final class SourceLocal extends AbstractSource
        implements InterfaceProxyable {
    private ProxyHandler handler;

    /***************************************************************/
    /* Variation Points                                            */
    /***************************************************************/

    /**
     * <p>Retrieve the invocation handler.</p>
     *
     * @return The invocation handler, or null.
     */
    public ProxyHandler getHandler() {
        return handler;
    }

    /**
     * <p>Set the invocation handler.</p>
     *
     * @param h The invocation handler.
     */
    public void setHandler(ProxyHandler h) {
        handler = h;
    }

    /**************************************************************/
    /* Init & Clear Module                                        */
    /**************************************************************/

    /**
     * <p>Compute default package, name and parent.</p>
     *
     * @throws EngineMessage Shit happens.
     */
    public void initSource() throws EngineMessage {
        String path = getPath();
        if (!CacheSubclass.isLocal(path))
            return;

        /* add package and name */
        String res = SourceLocal.unfindLibrary(path, getStore().user);
        if (CacheModule.isOs(res)) {
            addFix(CacheModule.sepDirectory(res), MASK_PCKG_LIBR);
            setName(CacheModule.sepFile(res));
        } else {
            setName(res);
        }

        /* set the full text name */
        res = res.replace(CacheModule.OP_CHAR_OS, CachePackage.OP_CHAR_SEG);
        setFullName(res);

        /* set the home link */
        res = CacheSubclass.sepHome(path);
        AbstractSource src = getStore().getSourceDefined(res, false);
        addDep(src, AbstractSource.MASK_IMPT_HOFL);

        /* change default visibility */
        resetBit(MASK_SRC_VSPU);
    }

    /**
     * <p>Calculate the full name.</p>
     *
     * @param key   The key.
     * @param scope The scope.
     * @return The full name.
     * @throws EngineMessage Shit happens.
     */
    private static String unfindLibrary(String key, AbstractSource scope)
            throws EngineMessage {
        try {
            int mask = ForeignPath.MASK_MODL_BASE;
            Object temp = CacheSubclass.unfindKey(key, scope, mask, null);
            if (!(temp instanceof SkelCompound))
                throw new EngineMessage(EngineMessage.existenceError(
                        EngineMessage.OP_EXISTENCE_CLASS_PATH, new SkelAtom(key)));

            temp = ((SkelCompound) temp).args[0];
            String res = ((SkelAtom) temp).fun;
            if (res.indexOf(CachePackage.OP_CHAR_SEG) != -1)
                throw new EngineMessage(EngineMessage.existenceError(
                        EngineMessage.OP_EXISTENCE_EXTENSION, new SkelAtom(key)));

            return res;
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Clear the module before a reconsult or purge.</p>
     *
     * @throws EngineMessage Shit happens.
     */
    public void clearModule()
            throws EngineMessage {
        super.clearModule();
        setHandler(null);

        /* clear deps with notify */
        int f = clearDeps(~AbstractSource.MASK_IMPT_HOFL);
        CachePredicate.notifyImportvers(this, f);
    }

    /**
     * <p>Check the module.</p>
     *
     * @param lr The reader.
     * @param en The interpreter.
     * @return True if module is empty, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public boolean checkModule(Reader lr, Engine en)
            throws EngineException, EngineMessage {
        boolean empty = super.checkModule(lr, en);
        if (empty)
            AbstractSource.checkModuleInit(this, en);
        return empty;
    }

    /**************************************************************/
    /* Open & Close Reader                                        */
    /**************************************************************/

    /**
     * <p>Open a read stream.</p>
     *
     * @param if_modified The if modified flag.
     * @param lopts       The options.
     * @return The reader or null.
     */
    public Reader openReader(boolean if_modified,
                             LoadOpts lopts) {
        return null;
    }

    /**
     * <p>Create a source from path.</p>
     *
     * @param p The path.
     */
    public SourceLocal(String p) {
        super(p);
    }

    /*******************************************************************/
    /* Import Link                                                     */
    /*******************************************************************/

    /**
     * <p>Retrieve the primordial parent.</p>
     *
     * @param src The source.
     * @return The primordial parent.
     */
    public static AbstractSource derefParentImport(AbstractSource src) {
        AbstractSource src2 = getParentImport(src);
        while (src2 != null) {
            src = src2;
            src2 = getParentImport(src);
        }
        return src;
    }

    /**
     * <p>Retrieve the parent module.</p>
     *
     * @return The parent module.
     */
    private static AbstractSource getParentImport(AbstractSource src) {
        MapEntry<AbstractSource, Integer>[] deps = src.snapshotDeps();
        for (int i = 0; i < deps.length; i++) {
            MapEntry<AbstractSource, Integer> dep = deps[i];
            if ((dep.value.intValue() & MASK_IMPT_PAIM) != 0)
                return dep.key;
        }
        return null;
    }

}
