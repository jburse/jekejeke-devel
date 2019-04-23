package jekpro.tools.call;

import jekpro.model.builtin.AbstractBranch;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.array.FlagFactory;

/**
 * This class represents the base for all toolkits. Each toolkit predefines
 * a set of capabilities for a knowledge base. The list of predefined
 * capabilities can be retrieved via the method getInitCapabilities(). The
 * brand capability among the predefined capabilities can be retrieved via
 * the method getBrandCapability(). The lookup name of a capability
 * can be retrieve by the method capabilityToString().
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
public abstract class Toolkit {
    private final AbstractFactory factory;

    /* interpreter */
    public final static String PROP_SYS_CUR_INPUT = FlagFactory.OP_SYS_CUR_INPUT;
    public final static String PROP_SYS_CUR_OUTPUT = FlagFactory.OP_SYS_CUR_OUTPUT;
    public final static String PROP_SYS_CUR_ERROR = FlagFactory.OP_SYS_CUR_ERROR;
    public final static String PROP_SYS_ATTACHED_TO = FlagFactory.OP_SYS_ATTACHED_TO;

    /**
     * <p>Create a new toolkit.</p>
     *
     * @param f The factory.
     */
    protected Toolkit(AbstractFactory f) {
        factory = f;
        factory.proxy = this;
    }

    /**
     * <p>Retrieve the init capabilities.</p>
     *
     * @return The capabilities.
     */
    public final Capability[] getInitCapabilities() {
        AbstractBranch[] branches = factory.getInitBranches();
        Capability[] res = new Capability[branches.length];
        for (int i = 0; i < branches.length; i++) {
            Capability capa = (Capability) branches[i].proxy;
            if (capa == null)
                throw new NullPointerException("capability missing");
            res[i] = capa;
        }
        return res;
    }

    /**
     * <p>Retrieve the brand capability.</p>
     *
     * @return Then capability.
     */
    public final Capability getBrandCapability() {
        Capability capa = (Capability) factory.getBrandBranch().proxy;
        if (capa == null)
            throw new NullPointerException("capability missing");
        return capa;
    }

    /**
     * <p>Convert a capabilty to a string.</p>
     *
     * @param cap The capability.
     * @return The name.
     */
    public final String capabilityToString(Capability cap) {
        return factory.getReflection().branchToString((AbstractBranch) cap.getBranch());
    }

    /***********************************************************/
    /* For Internal Use Only                                   */
    /***********************************************************/

    /**
     * <p>Retrieve the factory.</p>
     *
     * @return The factory.
     */
    public final Object getFactory() {
        return factory;
    }

}
