package jekpro.tools.foreign;

import jekpro.model.builtin.SpecialSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.DisplayClause;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.rope.Goal;
import jekpro.tools.proxy.BranchAPI;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

import java.lang.reflect.Field;

/**
 * <p>Specialization of a delegate for a field setter.</p>
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
final class MemberFieldSet extends Member {
    private final Field field;
    private final String fastname;
    private final Class[] fastpartys;

    /**
     * <p>Create field predicate.</p>
     *
     * @param f The field.
     */
    MemberFieldSet(Field f) {
        field = f;
        fastname = "set_" + field.getName();
        fastpartys = new Class[]{field.getType()};
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
        return field.getModifiers();
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
        return field.getDeclaringClass();
    }

    /******************************************************************/
    /* Execution Service                                              */
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
     * @throws EngineMessage FFI error.
     */
    public final boolean findFirst(Goal r, DisplayClause u,
                                   Engine en)
            throws EngineMessage {
        Object temp = en.skel;
        Display ref = en.display;
        Object obj = convertObj(temp, ref, en);
        Object[] args = convertArgs(temp, ref, en);
        invokeSetter(obj, args);
        return r.getNextRaw(u, en);
    }

    /**
     * <p>Invoke the method.</p>
     *
     * @param obj  The receiver.
     * @param args The arguments array.
     * @throws EngineMessage FFI error.
     */
    final void invokeSetter(Object obj, Object[] args)
            throws EngineMessage {
        try {
            field.set(obj, args[0]);
        } catch (IllegalAccessException x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_ACCESS,
                    BranchAPI.OP_PERMISSION_FIELD,
                    new SkelAtom(field.getName())));
        } catch (IllegalArgumentException x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    BranchAPI.OP_PERMISSION_APPLY,
                    BranchAPI.OP_PERMISSION_SETTER,
                    new SkelAtom(field.getName())));
        } catch (NullPointerException x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    BranchAPI.OP_PERMISSION_LOOKUP,
                    BranchAPI.OP_PERMISSION_SETTER,
                    new SkelAtom(field.getName())));
        } catch (NoClassDefFoundError x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_LINK,
                    EngineMessage.OP_PERMISSION_CLASS,
                    new SkelAtom(x.getMessage())));
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
        return field.hashCode();
    }

    /**
     * <p>Compare with another delegate.</p>
     *
     * @param o The other delegate.
     * @return True if this delegate equals the other delegate, otherwise false.
     */
    public boolean equals(Object o) {
        if (!(o instanceof MemberFieldSet))
            return false;
        MemberFieldSet dpfs = (MemberFieldSet) o;
        return field.equals(dpfs.field);
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
        return new SkelCompound(new SkelAtom("foreign_setter"),
                SpecialSpecial.classToName(field.getDeclaringClass(), source, en),
                new SkelAtom(field.getName()));
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
        return fastname;
    }

    /**
     * <p>Convert the delegate to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        return field.toString() + " (setter)";
    }

}
