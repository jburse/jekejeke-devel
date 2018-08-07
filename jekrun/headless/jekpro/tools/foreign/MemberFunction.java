package jekpro.tools.foreign;

import jekpro.model.builtin.SpecialSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.reference.reflect.SpecialForeign;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.array.Types;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

import java.lang.reflect.Method;

/**
 * <p>Specialization of a delegate for a number function.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
final class MemberFunction extends AbstractMember {
    private static final String OP_FOREIGN_FUN = "foreign_fun";

    private final Method method;

    /**
     * <p>Create function evaluable function.</p>
     *
     * @param m The method.
     */
    MemberFunction(Method m) {
        method = m;
        subflags |= MASK_DELE_ARIT;
        subflags |= MASK_METH_FUNC;
    }

    /************************************************************************************/
    /* Accessible Protocol                                                              */
    /************************************************************************************/

    /**
     * <p>Retrieve the modifier flags as an int.</p>
     *
     * @return The modifier flags as an int.
     */
    public int getModifiers() {
        return method.getModifiers();
    }

    /**
     * <p>Retrieve the return type as Java class.</p>
     *
     * @return The return type as Java class.
     */
    public Class getReturnType() {
        return method.getReturnType();
    }

    /**
     * <p>Retrieve the parameter types as Java classes.</p>
     *
     * @return The parameter types as Java classes.
     */
    public Class[] getParameterTypes() {
        return method.getParameterTypes();
    }

    /**
     * <p>Retrieve the declaring Java class.</p>
     *
     * @return The declaring Java class.
     */
    public Class getDeclaringClass() {
        return method.getDeclaringClass();
    }

    /************************************************************************************/
    /* Execution Service                                                                */
    /************************************************************************************/

    /**
     * <p>Arithmetically evaluate a compound.</p>
     * <p>The evaluable is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The result is passed via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @throws EngineMessage   FFI error.
     * @throws EngineException FFI error.
     */
    public final boolean moniEvaluate(Engine en)
            throws EngineMessage, EngineException {
        Object temp = en.skel;
        Display ref = en.display;
        Object obj;
        if ((subflags & AbstractDelegate.MASK_DELE_VIRT) != 0) {
            obj = Types.denormProlog(encodeobj, ((SkelCompound) temp).args[0], ref);
        } else {
            obj = null;
        }
        Object[] args = computeAndConvertArgs(temp, ref, en);
        Object res = invokeMethod(method, obj, args);
        res = Types.normJava(encoderet, res);
        if (res == null)
            throw new EngineMessage(EngineMessage.representationError(
                    AbstractFactory.OP_REPRESENTATION_NULL));
        Display d = AbstractTerm.getDisplay(res);
        en.skel = AbstractTerm.getSkel(res);
        en.display = d;
        return (d.flags & Display.MASK_DISP_MLTI) != 0;
    }

    /***************************************************************/
    /* Special Predicates                                          */
    /***************************************************************/

    /**
     * <p>Compute a hash code.</p>
     *
     * @return The hash code of this delegate.
     */
    public int hashCode() {
        return method.hashCode();
    }

    /**
     * <p>Compare with another delegate.</p>
     *
     * @param o The other delegate.
     * @return True if this delegate equals the other delegate, otherwise false.
     */
    public boolean equals(Object o) {
        if (!(o instanceof MemberFunction))
            return false;
        MemberFunction def = (MemberFunction) o;
        return method.equals(def.method);
    }

    /**
     * <p>Generate the spec of this delegate.</p>
     *
     * @param source The source.
     * @param en     The engine.
     * @return The spec.
     * @throws EngineMessage FFI error.
     */
    public Object toSpec(AbstractSource source, Engine en)
            throws EngineMessage {
        return new SkelCompound(new SkelAtom(OP_FOREIGN_FUN),
                SpecialSpecial.classToName(method.getDeclaringClass(), source, en),
                SpecialForeign.methodToCallable(method.getName(),
                        method.getParameterTypes(), source, en));
    }

    /***************************************************************/
    /* Foreign Predicates                                          */
    /***************************************************************/

    /**
     * <p>Retrieve a name guess.</p>
     *
     * @return The name guess, or null.
     */
    public String getFun() {
        return method.getName();
    }

    /**
     * <p>Convert the delegate to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        return method.toString() + " (evaluable)";
    }

}
