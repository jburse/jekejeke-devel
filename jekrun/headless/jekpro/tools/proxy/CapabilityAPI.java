package jekpro.tools.proxy;

import jekpro.model.builtin.Flag;
import jekpro.tools.call.Capability;

/**
 * <p>The runtime library capability provides the system predicates
 * as defined in the language reference of the runtime library. It
 * does not provide the system predicates of the language reference
 * of the development environment. This class implements the singleton
 * pattern. There is only one instance per Java class loader.
 * </p>
 * <p>This capability is already predefined by a toolkit and it
 * need not be added to the knowledge base after its initialization.
 * This capability does not need activation. It will be useable without
 * activation. The capability also declares some well-known interpreter
 * properties.</p>
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
public final class CapabilityAPI extends Capability {
    public final static CapabilityAPI DEFAULT = new CapabilityAPI();

    public final static String PROP_SYS_ACT_STATUS = Flag.OP_SYS_ACT_STATUS;

    /**
     * <p>Create the capability headless.</p>
     */
    private CapabilityAPI() {
        super(BranchAPI.DEFAULT);
    }

}