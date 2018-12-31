package jekpro.tools.array;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.Store;
import matula.comp.sharik.AbstractFramework;
import matula.util.data.MapHash;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

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
public abstract class AbstractFactory extends AbstractFramework {
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

    public static final String OP_REPRESENTATION_NULL = "null";

    public static final int FIELD_GET_PRED = 0;
    public static final int FIELD_GET_EVAL = 1;
    public static final int FIELD_SET = 2;

    public static final int ARRAY_LENGTH = 0;
    public static final int ARRAY_GET_EVAL = 1;
    public static final int ARRAY_NEW = 2;
    public static final int ARRAY_GET_PRED = 3;
    public static final int ARRAY_SET = 4;

    public Object proxy;
    public Object toolinput;
    public Object tooloutput;
    public Object toolerror;
    private MapHash<String, AbstractFlag> prologflags;
    private MapHash<String, AbstractFlag> threadflags;

    /****************************************************************/
    /* Prolog Flags                                                 */
    /****************************************************************/

    /**
     * <p>Retrieve the prolog flags.</p>
     *
     * @return The prolog flags.
     */
    public final MapHash<String, AbstractFlag> getPrologFlags() {
        return prologflags;
    }

    /**
     * <p>Set the prolog flags.</p>
     *
     * @param f The prolog flags.
     */
    public final void setPrologFlags(MapHash<String, AbstractFlag> f) {
        prologflags = f;
    }

    /****************************************************************/
    /* Thread Flags                                                 */
    /****************************************************************/

    /**
     * <p>Retrieve the thread flags.</p>
     *
     * @return The thread flags.
     */
    public final MapHash<String, AbstractFlag> getThreadFlags() {
        return threadflags;
    }

    /**
     * <p>Set the thread flags.</p>
     *
     * @param f The thread flags.
     */
    public final void setThreadFlags(MapHash<String, AbstractFlag> f) {
        threadflags = f;
    }

    /*******************************************************************/
    /* Life Cycle                                                      */
    /*******************************************************************/

    /**
     * <p>Create a fioyer.</p>
     *
     * @return The foyer.
     */
    public abstract Foyer createFoyer();

    /**
     * <p>Retrieve the init branches.</p>
     *
     * @return The branches.
     */
    public abstract AbstractBranch[] getInitBranches();

    /**
     * <p>Retrieve the brand branch.</p>
     *
     * @return The branch.
     */
    public abstract AbstractBranch getBrandBranch();

    /*******************************************************************/
    /* Delegate Factory                                                */
    /*******************************************************************/

    /**
     * <p>Create a foreign delegate.</p>
     * <p>Result or culprit is returned in the engine skel.</p>
     *
     * @param m  The method.
     * @param en The engine.
     * @param k  The predicate flag.
     * @return True if creation of the delegate succeeded, otherwise false.
     */
    public abstract boolean createMethod(Method m, Engine en, boolean k);

    /**
     * <p>Create a foreign constructor delegate.</p>
     * <p>Result or culprit is returned in the engine skel.</p>
     *
     * @param c  The constructor.
     * @param en The engine.
     * @return True if creation of the delegate succeeded, otherwise false.
     */
    public abstract boolean createConstructor(Constructor c, Engine en);

    /**
     * <p>Create a foreign getter or setter delegate.</p>
     * <p>Result or culprit is returned in the engine skel.</p>
     *
     * @param f  The field.
     * @param en The engine.
     * @param k  The desired delegate.
     * @return True if creation of the delegate succeeded, otherwise false.
     */
    public abstract boolean createField(Field f, Engine en, int k);

    /**
     * <p>Create a foreign array delegate.</p>
     * <p>Result or culprit is returned in the engine skel.</p>
     *
     * @param c  The class.
     * @param en The engine.
     * @param k  The desired delegate.
     * @return True if creation of the delegate succeeded, otherwise false.
     */
    public abstract boolean createArray(Class c, Engine en, int k);

    /***********************************************************************/
    /* Foreign Functions                                                   */
    /***********************************************************************/

    /**
     * <p>Find a branch.</p>
     *
     * @param name   The name.
     * @param loader The loader.
     * @throws EngineMessage Shit happens.
     */
    public abstract AbstractBranch stringToBranch(String name,
                                                  ClassLoader loader)
            throws EngineMessage;

    /**
     * <p>Convert a branch to a string.</p>
     *
     * @param branch The branch.
     * @return The name.
     */
    public abstract String branchToString(AbstractBranch branch);

    /**
     * <p>Validate the exception types.</p>
     *
     * @param exces The exception types.
     * @param en    The engine.
     */
    public abstract boolean validateExceptionTypes(Class[] exces, Engine en);

    /**
     * <p>Create a special.</p>
     *
     * @param con  The constructor.
     * @param args The constructor arguments.
     * @param en   The engine.
     * @return The special.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public abstract Object newInstance(Constructor con,
                                       Object[] args, Engine en)
            throws EngineException, EngineMessage;

    /*******************************************************************/
    /* Auto Loader Hook                                                */
    /*******************************************************************/

    /**
     * <p>Create a foreign or verbatim source.</p>
     *
     * @param key   The source key.
     * @param store The store.
     * @return The foreign source.
     */
    public abstract AbstractSource createSynth(String key, Store store);

}
