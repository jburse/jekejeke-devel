package jekpro.model.pretty;

import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineMessage;

import java.io.Reader;
import java.util.HashMap;
import java.util.Properties;

/**
 * <p>Specialization of the file source class for resource bundles.</p>
 *
 * @author Copyright 2015-2017, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.0.9 (a fast and small prolog interpreter)
 */
public final class FileResource extends AbstractFile {
    private final HashMap<String, Properties> cache = new HashMap<>();

    /**
     * <p>Create a resource source from path and capa.</p>
     *
     * @param p The path.
     */
    FileResource(String p) {
        super(p);
    }

    /**
     * <p>Retrieve the properties cache.</p>
     *
     * @return The properties cache.
     */
    public HashMap<String, Properties> getCache() {
        return cache;
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

        synchronized (this) {
            cache.clear();
        }
    }

    /**************************************************************/
    /* Load & Check Module                                        */
    /**************************************************************/

    /**
     * <p>Consult a stream.</p>
     *
     * @param lr The buffered reader.
     * @param en The interpreter.
     */
    public void loadModule(Reader lr, Engine en) {
        /* do nothing */
    }

    /**
     * <p>Check the module.</p>
     *
     * @param en The interpreter.
     * @return True if module is empty, otherwise false.
     */
    public boolean checkModule(Engine en) {
        /* do nothing */
        return false;
    }

}
