package jekpro.tools.proxy;

import jekpro.model.builtin.AbstractBranch;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.builtin.Branch;
import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.SourceLocal;
import jekpro.model.pretty.Store;
import jekpro.reference.reflect.SpecialForeign;
import jekpro.tools.array.AutoArray;
import jekpro.tools.call.Capability;
import jekpro.tools.foreign.AutoClass;
import jekpro.tools.foreign.LookupBinary;
import jekpro.tools.foreign.LookupResource;
import jekpro.tools.term.SkelAtom;
import matula.util.system.AbstractRuntime;
import matula.util.system.ForeignUri;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * <p>The ordinary Java reflection.</p>
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
public final class Reflection extends AbstractReflection {
    public final static String OP_DEFAULT = "DEFAULT";

    public final static AbstractReflection DEFAULT = new Reflection();

    /**
     * <p>Create a reflection.</p>
     */
    private Reflection() {
    }

    /*******************************************************************/
    /* Special Factory                                                 */
    /*******************************************************************/

    /**
     * <p>Validate the exception types.</p>
     * <p>Culprit is returned in engine skel.</p>
     *
     * @param exces The exception types.
     * @param en    The engine.
     * @return True if the exeception types are ok, otherwise false.
     */
    public boolean validateExceptionTypes(Class[] exces, Engine en) {
        return AutoClass.validateExceptionTypes(exces, en);
    }

    /**
     * <p>Create an instance.</p>
     *
     * @param con  The constructor.
     * @param args The arguments.
     * @param en   The engine.
     * @return The special delegate.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public Object newInstance(Constructor con, Object[] args, Engine en)
            throws EngineMessage, EngineException {
        return AutoClass.invokeNew(con, args, en);
    }

    /*******************************************************************/
    /* Array & Foreign Factory                                         */
    /*******************************************************************/

    /**
     * <p>Create a method delegate.</p>
     * <p>Result or culprit is returned in the engine skel.</p>
     *
     * @param m  The method.
     * @param en The engine.
     * @param k  The predicate flag.
     * @return True if creation of the delegate succeeded, otherwise false.
     */
    public boolean createMethod(Method m, Engine en, boolean k) {
        return AutoClass.createMethod(m, en, k);
    }

    /**
     * <p>Create a constructor delegate.</p>
     * <p>Result or culprit is returned in the engine skel.</p>
     *
     * @param c  The constructor.
     * @param en The engine.
     * @return True if creation of the delegate succeeded, otherwise false.
     */
    public boolean createConstructor(Constructor c, Engine en) {
        return AutoClass.createConstructor(c, en);
    }

    /**
     * <p>Create a getter or setter delegate.</p>
     * <p>Result or culprit is returned in the engine skel.</p>
     *
     * @param f  The field.
     * @param en The engine.
     * @param k  The desired delegate.
     * @return True if creation of the delegate succeeded, otherwise false.
     */
    public boolean createField(Field f, Engine en, int k) {
        return AutoClass.createField(f, en, k);
    }

    /**
     * <p>Create an array delegate.</p>
     * <p>Result or culprit is returned in the engine skel.</p>
     *
     * @param c  The class.
     * @param en The engine.
     * @param k  The desired delegate.
     * @return True if creation of the delegate succeeded, otherwise false.
     */
    public boolean createArray(Class c, Engine en, int k) {
        return AutoArray.createArray(c, en, k);
    }

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
    public AbstractBranch stringToBranch(String name, ClassLoader loader)
            throws EngineMessage {
        Class clazz = AbstractRuntime.stringToClass(name, loader);
        if (clazz == null)
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_CLASS, new SkelAtom(name)));
        Field field = SpecialForeign.getDeclaredField(clazz, OP_DEFAULT);
        Object value = AutoClass.invokeGetter(field, null);
        if (!(value instanceof Capability))
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_CAPABILITY,
                    new SkelAtom(value != null ? AbstractRuntime.classToString(
                            value.getClass()) : AbstractFlag.OP_NULL)));
        return (AbstractBranch) ((Capability) value).getBranch();
    }

    /**
     * <p>Convert a branch to a string.</p>
     *
     * @param branch The branch.
     * @return The name.
     */
    public String branchToString(AbstractBranch branch) {
        Object proxy = branch.proxy;
        if (!(proxy instanceof Capability))
            throw new NullPointerException("capability missing");
        return AbstractRuntime.classToString(proxy.getClass());
    }

    /**
     * <p>Create a foreign or verbatim source.</p>
     *
     * @param key   The source key.
     * @param store The store.
     * @return The foreign source.
     */
    public AbstractSource createSynth(String key, Store store) {
        if (ForeignUri.sysUriIsRelative(key)) {
            String keyrel = LookupBinary.removeClassExt(key);
            if (keyrel != null) {
                AbstractSource src;
                if (keyrel.endsWith(AbstractRuntime.JAVA_ARRAY)) {
                    src = new AutoArray(key);
                } else {
                    src = new AutoClass(key);
                }
                src.setBranch(LookupResource.RelativeURIstoRoots(key, store));
                return src;
            }
            AbstractSource src = new SourceLocal(key);
            if (Branch.OP_SYSTEM.equals(key))
                src.setBranch(store.foyer.getFactory().getBrandBranch());
            return src;
        }
        AbstractSource src = new SourceLocal(key);
        src.setBranch(LookupResource.AbsoluteURIstoRoots(key, store));
        return src;
    }

}