package jekpro.tools.proxy;

import jekpro.model.builtin.Branch;
import jekpro.model.builtin.InterfaceInit;
import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;

/**
 * <p>The internal implementation of the capability API.</p>
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
final class BranchAPI extends Branch {
    public final static BranchAPI DEFAULT = new BranchAPI();

    /**
     * <p>Init the store with this branch.</p>
     *
     * @param en     The engine.
     * @param prompt The prompt flag.
     * @param system The system flag.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public void initBranch(Engine en,
                           boolean prompt, boolean system)
            throws EngineMessage, EngineException {
        super.initBranch(en, prompt, system);

        loadSystem("jekpro/reference/reflect/foreign.p", en, new InterfaceInit() {
            public void init(AbstractSource scope, Engine en)
                    throws EngineMessage, EngineException {
                addResource("jekpro/reference/reflect/api.properties", scope, en);
                addResource("jekpro/frequent/stream/foreign.properties", scope, en);
            }
        });
        loadSystem("jekpro/reference/reflect/member.p", en);
        loadSystem("jekpro/reference/reflect/pred.p", en);
        loadSystem("jekpro/reference/reflect/oper.p", en);
        loadSystem("jekpro/reference/reflect/source.p", en);

        loadSystem("jekpro/reference/bootload/concat.p", en);
        loadSystem("jekpro/reference/bootload/path.p", en);
        loadSystem("jekpro/reference/bootload/load.p", en);
        loadSystem("jekpro/reference/bootload/module.p", en);

        String aspect=en.store.foyer.getFactory().getRuntime().getAspect();
        loadSystem("jekpro/platform/"+aspect+"/stats.p", en);
    }

}
