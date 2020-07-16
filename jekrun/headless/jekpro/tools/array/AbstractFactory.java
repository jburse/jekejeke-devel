package jekpro.tools.array;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.builtin.FlagSession;
import jekpro.model.inter.Engine;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import jekpro.tools.call.Capability;
import jekpro.tools.proxy.AbstractReflection;
import jekpro.tools.proxy.CapabilityAPI;
import jekpro.tools.proxy.Reflection;
import matula.util.data.ListArray;
import matula.util.data.MapHash;
import matula.util.system.ConnectionReader;
import matula.util.system.ConnectionWriter;

import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

/**
 * <p>This class provides an abstract factory.</p>
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
public abstract class AbstractFactory {
    public static final String OP_DOMAIN_FOREIGN_VISIBILITY = "foreign_visibility";
    public static final String OP_DOMAIN_FOREIGN_RECEIVER = "foreign_receiver";
    public static final String OP_DOMAIN_FOREIGN_PARAMETER = "foreign_parameter";
    public static final String OP_DOMAIN_FOREIGN_RETURN = "foreign_return";
    public static final String OP_DOMAIN_FOREIGN_EXCEPTION = "foreign_exception";
    public static final String OP_DOMAIN_FOREIGN_ACCESS = "foreign_access";
    public static final String OP_DOMAIN_FOREIGN_ARRAY = "foreign_array";
    public static final String OP_DOMAIN_FOREIGN_NOTYET = "foreign_notyet";

    public static final String OP_PERMISSION_LOOKUP = "lookup";
    public static final String OP_PERMISSION_APPLY = "apply";
    public static final String OP_PERMISSION_METHOD = "method";
    public static final String OP_PERMISSION_CONSTRUCTOR = "constructor";
    public static final String OP_PERMISSION_FIELD = "field";
    public static final String OP_PERMISSION_INDEX = "index";
    public static final String OP_PERMISSION_NEW = "new";

    public static final String OP_REPRESENTATION_NULL = "null";

    public Object proxy;
    public Object toolinput;
    public Object tooloutput;
    public Object toolerror;
    private ListArray<MapHash<String, AbstractFlag<Engine>>> prologflags
            = new ListArray<MapHash<String, AbstractFlag<Engine>>>();
    private ListArray<MapHash<String, AbstractFlag<Store>>> sessionflags
            = new ListArray<MapHash<String, AbstractFlag<Store>>>();
    private AbstractReflection reflection;

    /**
     * <p>Create an abstract factor.</p>
     */
    public AbstractFactory() {
        prepareToolConnections();
        addPrologFlags(FlagFactory.DEFAULT);
        addSessionFlags(FlagSession.DEFAULT);
        setReflection(Reflection.DEFAULT);
    }

    /**
     * <p>Prepare the tool connections.</p>
     * <p>Can be overridden by subclasses.</p>
     */
    protected void prepareToolConnections() {
        toolinput = new ConnectionReader(new InputStreamReader(System.in));
        tooloutput = new ConnectionWriter(new OutputStreamWriter(System.out));
        toolerror = new ConnectionWriter(new OutputStreamWriter(System.err));
    }

    /**
     * <p>Retrieve the reflection.</p>
     *
     * @return The reflection.
     */
    public AbstractReflection getReflection() {
        return reflection;
    }

    /**
     * <p>Set the reflection.</p>
     *
     * @param r The reflection.
     */
    public void setReflection(AbstractReflection r) {
        reflection = r;
    }

    /**
     * <p>Retrieve the prolog flags.</p>
     *
     * @return The prolog flags.
     */
    public final ListArray<MapHash<String, AbstractFlag<Engine>>> getPrologFlags() {
        return prologflags;
    }

    /**
     * <p>Set the prolog flags.</p>
     *
     * @param f The prolog flags.
     */
    public final void addPrologFlags(MapHash<String, AbstractFlag<Engine>> f) {
        if (f == null)
            throw new NullPointerException("flags missing");
        prologflags.add(f);
    }

    /**
     * <p>Retrieve the prolog flags.</p>
     *
     * @return The prolog flags.
     */
    public final ListArray<MapHash<String, AbstractFlag<Store>>> getSessionFlags() {
        return sessionflags;
    }

    /**
     * <p>Set the prolog flags.</p>
     *
     * @param f The prolog flags.
     */
    public final void addSessionFlags(MapHash<String, AbstractFlag<Store>> f) {
        if (f == null)
            throw new NullPointerException("flags missing");
        sessionflags.add(f);
    }

    /*******************************************************************/
    /* Bootstrap Capabilities                                          */
    /*******************************************************************/

    /**
     * <p>Retrieve the init branches.</p>
     * <p>Can be overridden by subclasses.</p>
     *
     * @return The branches.
     */
    public AbstractBranch[] getInitBranches() {
        Capability cap = new CapabilityAPI();
        return new AbstractBranch[]{(AbstractBranch) cap.getBranch()};
    }

    /**
     * <p>Retrieve the brand branch.</p>
     * <p>Can be overridden by subclasses.</p>
     *
     * @return The branch.
     */
    public AbstractBranch getBrandBranch() {
        Capability cap = new CapabilityAPI();
        return (AbstractBranch) cap.getBranch();
    }

    /*******************************************************************/
    /* Factory Methods                                                 */
    /*******************************************************************/

    /**
     * <p>Create a fioyer.</p>
     * <p>Can be overridden by subclasses.</p>
     *
     * @return The foyer.
     */
    public Foyer createFoyer() {
        Foyer foyer = new Foyer();
        foyer.setFactory(this);
        return foyer;
    }

    /*******************************************************************/
    /* Plaztform Specific                                              */
    /*******************************************************************/

    /**
     * <p>Add a mem listener to a foyer.</p>
     *
     * @param f The foyer.
     */
    public abstract void addMemListener(Foyer f);

    /**
     * <p>Remove a mem listener from  a foyer.</p>
     *
     * @param f The foyer.
     */
    public abstract void removeMemListener(Foyer f);

    /**
     * <p>Set the hint.</p>
     *
     * @param f The foyer.
     * @param h The hint.
     */
    public abstract void setHint(Foyer f, int h);

}
