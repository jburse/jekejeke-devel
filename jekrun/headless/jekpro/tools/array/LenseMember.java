package jekpro.tools.array;

import jekpro.model.builtin.SpecialSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.DisplayClause;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.rope.Goal;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

import java.lang.reflect.Array;
import java.lang.reflect.Modifier;

/**
 * <p>Specialization of an array delegate for an array number element access.</p>
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
final class LenseMember extends AbstractLense {
    public static final String OP_NAME_GETTER = "at";

    private final Class clazz;

    /**
     * <p>Create an array number element access.</p>
     *
     * @param c The class.
     */
    LenseMember(Class c) {
        clazz = c;
        subflags |= MASK_DELE_ARIT;
        subflags |= MASK_METH_FUNC;
    }

    /******************************************************************/
    /* Accessible Protocol                                            */
    /******************************************************************/

    /**
     * <p>Retrieve the modifier flags as an int.</p>
     *
     * @return The modifier flags as an int.
     */
    public int getModifiers() {
        return Modifier.PUBLIC;
    }

    /**
     * <p>Retrieve the return type as Java class.</p>
     *
     * @return The return type as Java class.
     */
    public Class getReturnType() {
        return clazz.getComponentType();
    }

    /**
     * <p>Retrieve the parameter types as Java classes.</p>
     *
     * @return The parameter types as Java classes.
     */
    public Class[] getParameterTypes() {
        return SpecialSpecial.SIG_INT;
    }

    /**
     * <p>Retrieve the declaring Java class.</p>
     *
     * @return The declaring Java class.
     */
    public Class getDeclaringClass() {
        return clazz;
    }

    /******************************************************************/
    /* Variation Points Predicate                                     */
    /******************************************************************/

    /**
     * <p>Arithmetically evaluate a compound.</p>
     * <p>The evaluable is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The result is passed via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @throws EngineMessage FFI error.
     */
    public final void moniEvaluate(Engine en)
            throws EngineMessage {
        Object temp = en.skel;
        Display ref = en.display;
        Object obj = convertObj(temp, ref, en);
        en.skel = ((SkelCompound) temp).args[1];
        en.display = ref;
        en.deref();
        EngineMessage.checkInstantiated(en.skel);
        Number num = EngineMessage.castInteger(en.skel, en.display);
        EngineMessage.checkNotLessThanZero(num);
        int idx = EngineMessage.castIntValue(num);
        Object res = get(obj, idx, en);
        res = Types.normJava(encoderet, res);
        if (res == null)
            throw new EngineMessage(EngineMessage.representationError(
                    AbstractFactory.OP_REPRESENTATION_NULL));
        en.skel = AbstractTerm.getSkel(res);
        en.display = AbstractTerm.getDisplay(res);
    }

    /**
     * <p>Retrieve the element at the specified index.</p>
     *
     * @param o  The array.
     * @param i  The index.
     * @param en The engine.
     * @return The element.
     * @throws EngineMessage FFI error.
     */
    private Object get(Object o, int i, Engine en)
            throws EngineMessage {
        try {
            return Array.get(o, i);
        } catch (IllegalArgumentException x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    AbstractFactory.OP_PERMISSION_APPLY,
                    AbstractFactory.OP_PERMISSION_GETTER,
                    SpecialSpecial.classToName(clazz, en.store.SOURCE_SYSTEM, en)));
        } catch (ArrayIndexOutOfBoundsException x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    AbstractFactory.OP_PERMISSION_APPLY,
                    AbstractFactory.OP_PERMISSION_INDEX,
                    SpecialSpecial.classToName(clazz, en.store.SOURCE_SYSTEM, en)));
        }
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
        return clazz.hashCode();
    }

    /**
     * <p>Compare with another delegate.</p>
     *
     * @param o The other delegate.
     * @return True if this delegate equals the other delegate, otherwise false.
     */
    public boolean equals(Object o) {
        if (!(o instanceof LenseMember))
            return false;
        LenseMember fem = (LenseMember) o;
        return clazz.equals(fem.clazz);
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
        return new SkelCompound(new SkelAtom("foreign_member"),
                SpecialSpecial.classToName(clazz, source, en));
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
        return OP_NAME_GETTER;
    }

    /**
     * <p>Convert the delegate to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        return clazz.toString() + " (at, evaluable)";
    }

}
