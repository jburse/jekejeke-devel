package jekpro.tools.array;

import jekpro.model.builtin.SpecialSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.DisplayClause;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.rope.Goal;
import jekpro.tools.proxy.BranchAPI;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

import java.lang.reflect.Array;
import java.lang.reflect.Modifier;

/**
 * <p>Specialization of an array delegate for an array constructor.</p>
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
final class LenseDimension extends AbstractLense {
    private final Class clazz;

    /**
     * <p>Create an array construction.</p>
     *
     * @param c The class.
     */
    LenseDimension(Class c) {
        clazz = c;
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
        return Modifier.PUBLIC + Modifier.STATIC;
    }

    /**
     * <p>Retrieve the return type as Java class.</p>
     *
     * @return The return type as Java class.
     */
    public Class getReturnType() {
        return clazz;
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
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param r  The continuation skel.
     * @param u  The continuation display.
     * @param en The interpreter.
     * @return True if the goal succeeded, otherwise false.
     * @throws EngineException FFI error.
     * @throws EngineMessage   FFI error.
     */
    public final boolean findFirst(Goal r, DisplayClause u,
                                   Engine en)
            throws EngineException, EngineMessage {
        Object[] temp = ((SkelCompound) en.skel).args;
        Display ref = en.display;
        en.skel = temp[0];
        en.display = ref;
        en.deref();
        EngineMessage.checkInstantiated(en.skel);
        Number num = EngineMessage.castInteger(en.skel, en.display);
        EngineMessage.checkNotLessThanZero(num);
        int size = EngineMessage.castIntValue(num);
        Object val = newInstance(size, en);
        if (!en.unifyTerm(temp[1], ref, val, Display.DISPLAY_CONST, r, u))
            return false;
        return r.getNext(u, en);
    }

    /**
     * <p>Create a new array instance.</p>
     *
     * @param s  The size.
     * @param en The engine.
     * @return The new array.
     * @throws EngineMessage FFI error.
     */
    private Object newInstance(int s, Engine en)
            throws EngineMessage {
        try {
            return Array.newInstance(clazz.getComponentType(), s);
        } catch (IllegalArgumentException x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    BranchAPI.OP_PERMISSION_APPLY,
                    BranchAPI.OP_PERMISSION_CONSTRUCTOR,
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
        if (!(o instanceof LenseDimension))
            return false;
        LenseDimension fpd = (LenseDimension) o;
        return clazz.equals(fpd.clazz);
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
        return new SkelCompound(new SkelAtom("foreign_dimension"),
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
        return SpecialSpecial.OP_NAME_CONSTRUCTOR;
    }

    /**
     * <p>Convert the delegate to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        return clazz.toString() + " (constructor)";
    }

}
