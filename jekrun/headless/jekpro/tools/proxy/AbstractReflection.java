package jekpro.tools.proxy;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Store;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * <p>The base class for our reflection.</p>
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
public abstract class AbstractReflection {
    public static final int FIELD_GET = 0;
    public static final int FIELD_SET = 2;

    public static final int ARRAY_LENGTH = 0;
    public static final int ARRAY_NEW = 2;
    public static final int ARRAY_GET = 3;
    public static final int ARRAY_SET = 4;

    /*******************************************************************/
    /* Special & Proxy Factory                                         */
    /*******************************************************************/

    /**
     * <p>Validate the exception types.</p>
     *
     * @param exces The exception types.
     * @param en    The engine.
     */
    public abstract boolean validateExceptionTypes(Class[] exces, Engine en);

    /*******************************************************************/
    /* Array & Foreign Factory                                         */
    /*******************************************************************/

    /**
     * <p>Create a foreign delegate.</p>
     * <p>Result or culprit is returned in the engine skel.</p>
     *
     * @param m  The method.
     * @param en The engine.
     * @return True if creation of the delegate succeeded, otherwise false.
     */
    public abstract boolean createMethod(Method m, Engine en);

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

    /*******************************************************************/
    /* Branch & Synth Objects                                          */
    /*******************************************************************/

    /**
     * <p>Find a branch.</p>
     *
     * @param name   The name.
     * @param loader The loader.
     * @throws EngineMessage Shit happens.
     */
    public abstract AbstractBranch stringToBranch(String name,
                                                  ClassLoader loader)
            throws EngineMessage, EngineException;

    /**
     * <p>Convert a branch to a string.</p>
     *
     * @param branch The branch.
     * @return The name.
     */
    public abstract String branchToString(AbstractBranch branch);

    /**
     * <p>Create a foreign or verbatim source.</p>
     *
     * @param key   The source key.
     * @param store The store.
     * @return The foreign source.
     */
    public abstract AbstractSource createSynth(String key, Store store);

}