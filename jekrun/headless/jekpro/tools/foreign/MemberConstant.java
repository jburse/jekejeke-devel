package jekpro.tools.foreign;

import jekpro.model.builtin.SpecialSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.reference.reflect.SpecialForeign;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.array.Types;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.MutableBit;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

import java.lang.reflect.Field;

/**
 * <p>Specialization of a delegate for a number constants.</p>
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
final class MemberConstant extends AbstractMember {
    private static final String OP_FOREIGN_CONST = "foreign_const";

    private final Field field;

    /**
     * <p>Create constant evaluable function.</p>
     *
     * @param f The field.
     */
    MemberConstant(Field f) {
        field = f;
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
        return field.getModifiers();
    }

    /**
     * <p>Retrieve the return type as Java class.</p>
     *
     * @return The return type as Java class.
     */
    public Class getReturnType() {
        return field.getType();
    }

    /**
     * <p>Retrieve the parameter types as Java classes.</p>
     *
     * @return The parameter types as Java classes.
     */
    public Class[] getParameterTypes() {
        return SpecialForeign.VOID_TYPES;
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
    public final boolean moniEvaluate(Engine en)
            throws EngineMessage {
        Object temp = en.skel;
        Display ref = en.display;
        Object obj;
        if ((subflags & AbstractDelegate.MASK_DELE_VIRT) != 0) {
            obj = Types.denormProlog(encodeobj, ((SkelCompound) temp).args[0], ref);
        } else {
            obj = null;
        }
        Object res = AutoClass.invokeGetter(field, obj);
        res = Types.normJava(encoderet, res);
        if (res == null)
            throw new EngineMessage(EngineMessage.representationError(
                    AbstractFactory.OP_REPRESENTATION_NULL));
        en.skel = AbstractTerm.getSkel(res);
        en.display = AbstractTerm.getDisplay(res);
        Object check = AbstractTerm.getMarker(res);
        return (check != null && ((MutableBit) check).getBit());
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
        if (!(o instanceof MemberConstant))
            return false;
        MemberConstant dec = (MemberConstant) o;
        return field.equals(dec.field);
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
        return new SkelCompound(new SkelAtom(OP_FOREIGN_CONST),
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
        return field.getName();
    }

    /**
     * <p>Convert the delegate to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        return field.toString() + " (evaluable)";
    }

}
