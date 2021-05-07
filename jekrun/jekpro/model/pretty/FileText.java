package jekpro.model.pretty;

import jekpro.frequent.basic.InterfaceProxyable;
import jekpro.model.builtin.Branch;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.tools.proxy.ProxyHandler;
import matula.util.data.MapEntry;

import java.io.Reader;

/**
 * <p>Specialization of the file source class for Prolog texts.</p>
 *
 * @author Copyright 2015, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.0.9 (a fast and small prolog interpreter)
 */
public final class FileText extends AbstractFile
        implements InterfaceProxyable {
    private ProxyHandler handler;

    /**
     * <p>Create a text source from path.</p>
     *
     * @param p The path.
     */
    FileText(String p) {
        super(p);
    }

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
    /* Source Name                                                */
    /**************************************************************/

    /**
     * <p>Set the module name.</p>
     *
     * @param n The module name.
     * @return True if the module name was not changed, otherwise false.
     */
    public boolean setName(String n) {
        boolean f = super.setName(n);
        if (!f)
            setFullName(calcFullName());
        return f;
    }

    /**
     * <p>Calculate the full name.</p>
     *
     * @return The full name.
     */
    private String calcFullName() {
        String temp = FileText.getPackName(this);
        String res = FileText.getStoreName(this);
        if (temp != null) {
            return CachePackage.composeStruct(temp, res);
        } else {
            return res;
        }
    }

    /**
     * <p>Retrieve the package name.</p>
     *
     * @param src The source.
     * @return The package name.
     */
    private static String getPackName(AbstractSource src) {
        MapEntry<String, Integer>[] fixes = src.snapshotFixes();
        for (int i = 0; i < fixes.length; i++) {
            MapEntry<String, Integer> fix = fixes[i];
            if ((fix.value.intValue() & AbstractSource.MASK_PCKG_LIBR) != 0) {
                String temp = fix.key;
                temp = temp.replace(CacheModule.OP_CHAR_OS, CachePackage.OP_CHAR_SEG);
                return temp;
            }
        }
        return null;
    }

    /**
     * <p>Retrieve the store name.</p>
     *
     * @param src The source.
     * @return The store name.
     */
    private static String getStoreName(AbstractSource src) {
        String name = src.getName();
        if (name != null)
            return name;
        return Branch.OP_USER;
    }

    /**************************************************************/
    /* Init & Clear Module                                        */
    /**************************************************************/

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
        int f = clearDeps(-1);
        CachePredicate.notifyImportvers(this, f);

        /* clear fixes with notify */
        f = (setName(null) ? 0 : AbstractSource.MASK_PCKG_LIBR);
        f |= clearFixes(-1);
        CacheModule.notifyFixvers(this, f);

        setBit(AbstractSource.MASK_SRC_VSPU);
        resetBit(AbstractSource.MASK_SRC_VSPR);
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

}
