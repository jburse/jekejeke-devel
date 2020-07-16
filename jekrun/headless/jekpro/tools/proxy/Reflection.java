package jekpro.tools.proxy;

import jekpro.model.builtin.AbstractBranch;
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
import jekpro.tools.term.SkelAtom;
import matula.util.config.AbstractBundle;
import matula.util.config.AbstractRuntime;
import matula.util.data.ListArray;
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
     * @return The special delegate.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public Object newInstance(Constructor con, Object[] args)
            throws EngineMessage, EngineException {
        return AutoClass.invokeNew(con, args);
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
     * @return True if creation of the delegate succeeded, otherwise false.
     */
    public boolean createMethod(Method m, Engine en) {
        return AutoClass.createMethod(m, en);
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
            throws EngineMessage, EngineException {
        Class clazz = AbstractRuntime.stringToClass(extractClass(name), loader);
        if (clazz == null)
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_CLASS, new SkelAtom(name)));
        String[] params = extractParams(name);
        Class[] types;
        if (params.length != 0) {
            types = new Class[params.length];
            for (int i = 0; i < params.length; i++)
                types[i] = String.class;
        } else {
            types = SpecialForeign.VOID_TYPES;
        }
        Constructor constr = SpecialForeign.getDeclaredConstructor(clazz, types);
        Object value = AutoClass.invokeNew(constr, params);
        if (!(value instanceof Capability))
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_CAPABILITY, value));
        return (AbstractBranch) ((Capability) value).getBranch();
    }

    /**
     * <p>Convert a branch to a string.</p>
     *
     * @param branch The branch.
     * @return The name.
     */
    public String branchToString(AbstractBranch branch) {
        Capability capa = (Capability) branch.proxy;
        if (capa == null)
            throw new NullPointerException("capability missing");
        String[] params = branch.getParams();
        if (params.length > 0) {
            StringBuilder buf = new StringBuilder();
            buf.append(AbstractRuntime.classToString(capa.getClass()));
            buf.append('(');
            buf.append(params[0]);
            for (int i = 1; i < params.length; i++) {
                buf.append(',');
                buf.append(params[i]);
            }
            buf.append(')');
            return buf.toString();
        } else {
            return AbstractRuntime.classToString(capa.getClass());
        }
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
                src.setBranch(store.pathToDecoder(key));
                return src;
            }
            AbstractSource src = new SourceLocal(key);
            if (Branch.OP_SYSTEM.equals(key))
                src.setBranch(store.foyer.getFactory().getBrandBranch());
            return src;
        } else {
            AbstractSource src = new SourceLocal(key);
            src.setBranch(store.pathToDecoder(key));
            return src;
        }
    }

    /**************************************************************/
    /* Parameter Utilities                                        */
    /**************************************************************/

    /**
     * <p>Extract the class.</p>
     *
     * @param name The capability.
     * @return The class.
     */
    private static String extractClass(String name) {
        int i = name.indexOf('(');
        if (i != -1) {
            return name.substring(0, i);
        } else {
            return name;
        }
    }

    /**
     * <p>Extract the parameters.</p>
     *
     * @param name The capability.
     * @return The parameters.
     * @throws EngineMessage Shit happens.
     */
    private static String[] extractParams(String name)
            throws EngineMessage {
        int i = name.indexOf('(');
        if (i != -1) {
            ListArray<String> list = new ListArray<String>();

            int k = name.indexOf(',', i + 1);
            while (k != -1) {
                list.add(name.substring(i + 1, k));
                i = k;
                k = name.indexOf(',', i + 1);
            }
            k = name.indexOf(')', i + 1);
            if (k != name.length() - 1)
                throw new EngineMessage(EngineMessage.syntaxError(
                        EngineMessage.OP_SYNTAX_PARAMETER_ERROR));
            list.add(name.substring(i + 1, k));

            String[] res = new String[list.size()];
            list.toArray(res);
            return res;
        } else {
            return AbstractBundle.VOID_LIST;
        }
    }

    /**
     * <p>Some testing.</p>
     *
     * @param args Not used.
     * @throws EngineMessage Shit happens.
     */
    /*
    public static void main(String[] args)
            throws EngineMessage {
        String name = "jekpro.tools.bundle.CapabilitySWI(chat80-1.0/)";
        System.out.println("name=" + name);

        String clazz = extractClass(name);
        System.out.println("class=" + clazz);

        String[] params = extractParams(name);
        System.out.println("param.length=" + params.length);
        for (int i = 0; i < params.length; i++)
            System.out.println("param[" + i + "]=" + params[i]);
    }
    */

}