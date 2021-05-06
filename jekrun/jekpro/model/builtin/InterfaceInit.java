package jekpro.model.builtin;

import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;

/**
 * <p>The interface of an init routine.</p>
 *
 * @author Copyright 2015, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.0.6 (a fast and small prolog interpreter)
 */
public interface InterfaceInit {

    /**
     * <p>Perform a Java sequence initialization during load.</p>
     *
     * @param scope The scope.
     * @param en    The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    void init(AbstractSource scope,
              Engine en)
            throws EngineMessage, EngineException;

}
