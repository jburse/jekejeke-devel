package jekpro.tools.array;

import jekpro.model.builtin.SpecialSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.rope.Goal;
import jekpro.tools.call.InterpreterMessage;
import jekpro.tools.proxy.BranchAPI;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

import java.lang.reflect.Array;
import java.lang.reflect.Modifier;

/**
 * <p>Specialization of an array delegate for an array element update.</p>
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
final class LenseUpdate extends Lense {
    private static final String OP_NAME_SETTER = "set_at";

    private final Class clazz;
    private final Class[] fastpartys;

    /**
     * <p>Create an array element modification.</p>
     *
     * @param c The class.
     */
    LenseUpdate(Class c) {
        clazz = c;
        fastpartys = new Class[]{Integer.TYPE, clazz.getComponentType()};
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
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public final boolean findFirst(Goal r, DisplayClause u,
                                   Engine en)
            throws EngineException, EngineMessage {
        Object[] temp = ((SkelCompound) en.skel).args;
        Display ref = en.display;
        Object obj = Types.castRef(temp[0], ref, en);
        en.skel = temp[1];
        en.display = ref;
        en.deref();
        EngineMessage.checkInstantiated(en.skel);
        Number num = EngineMessage.castInteger(en.skel, en.display);
        EngineMessage.checkNotLessThanZero(num);
        int idx = EngineMessage.castIntValue(num);
        Object res = convertArg(temp, ref, en);
        set(obj, idx, res, en);
        return r.getNextRaw(u, en);
    }

    /**
     * <p>Build the argument. The argument of the goal is
     * checked and converted if necessary.</p>
     *
     * @param temp The skeleton.
     * @param ref  The display.
     * @param en   The engine.
     * @return The argument.
     * @throws EngineMessage Shit happens.
     */
    private Object convertArg(Object[] temp, Display ref, Engine en)
            throws EngineMessage {
        try {
            int typ = encodeparas[1];
            en.skel = temp[2];
            en.display = ref;
            en.deref();
            Object res;
            if (typ == Types.TYPE_TERM) {
                res = AbstractTerm.createTermWrapped(en.skel, en.display);
            } else {
                res = AbstractTerm.createTerm(en.skel, en.display);
            }
            return Types.denormProlog(typ, res);
        } catch (InterpreterMessage x) {
            throw (EngineMessage) x.getException();
        }
    }

    /**
     * <p>Set the element at the specified index.</p>
     *
     * @param o  The array.
     * @param i  The index.
     * @param v  The element.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    private void set(Object o, int i, Object v, Engine en)
            throws EngineMessage {
        try {
            Array.set(o, i, v);
        } catch (IllegalArgumentException x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    BranchAPI.OP_PERMISSION_APPLY,
                    BranchAPI.OP_PERMISSION_SETTER,
                    SpecialSpecial.classToName(clazz, en.store.SOURCE_SYSTEM, en)));
        } catch (ArrayIndexOutOfBoundsException x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    BranchAPI.OP_PERMISSION_APPLY,
                    BranchAPI.OP_PERMISSION_INDEX,
                    SpecialSpecial.classToName(clazz, en.store.SOURCE_SYSTEM, en)));
        }
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
        return Void.TYPE;
    }

    /**
     * <p>Retrieve the parameter types as Java classes.</p>
     *
     * @return The parameter types as Java classes.
     */
    public Class[] getParameterTypes() {
        return fastpartys;
    }

    /**
     * <p>Retrieve the declaring Java class.</p>
     *
     * @return The declaring Java class.
     */
    public Class getDeclaringClass() {
        return clazz;
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
        if (!(o instanceof LenseUpdate))
            return false;
        LenseUpdate fpu = (LenseUpdate) o;
        return clazz.equals(fpu.clazz);
    }

    /**
     * <p>Generate the spec of this delegate.</p>
     *
     * @param source The source.
     * @param en     The engine.
     * @return The spec.
     * @throws EngineMessage Shit happens.
     */
    public Object toSpec(AbstractSource source, Engine en)
            throws EngineMessage {
        return new SkelCompound(new SkelAtom("foreign_update"),
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
        return OP_NAME_SETTER;
    }

    /**
     * <p>Convert the delegate to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        return clazz.toString() + " (setter)";
    }

}
