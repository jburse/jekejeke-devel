package jekpro.frequent.experiment;

import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineMessage;

/**
 * <p>Base class for internal database references.</p>
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
public interface InterfaceReference {

    /**
     * <p>Assert this reference.</p>
     *
     * @param flags The flags.
     * @param en    The engine.
     * @return True if the reference was asserted, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    boolean assertRef(int flags, Engine en)
            throws EngineMessage;

    /**
     * <p>Retract this reference.</p>
     *
     * @param en The engine.
     * @return True if the reference was retracted, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    boolean retractRef(Engine en)
            throws EngineMessage;

    /**
     * <p>Clause this reference.</p>
     * <p>The result is returned in the skeleton and display.</p>
     *
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     * @return True if new display is returned, otherwise false.
     */
    boolean clauseRef(Engine en)
            throws EngineMessage;

}
