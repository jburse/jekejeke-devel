package jekpro.tools.foreign;

import jekpro.model.inter.Engine;
import jekpro.model.molec.BindCount;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.reference.reflect.SpecialForeign;
import jekpro.tools.array.Types;
import jekpro.tools.term.*;

import java.lang.reflect.Member;
import java.lang.reflect.Method;

/**
 * <p>Specialization of a delegate for a deterministic predicate.</p>
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
final class MemberMethodDet extends AbstractMember {
    static final String OP_FOREIGN = "foreign";

    private final Method method;

    /**
     * <p>Create method predicate.</p>
     *
     * @param m The method.
     */
    MemberMethodDet(Method m) {
        method = m;
    }

    /**
     * <p>Retrieve the proxy that is wrapped.</p>
     *
     * @return The proxy.
     */
    public Member getProxy() {
        return method;
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

    /******************************************************************/
    /* Variation Points Predicate                                     */
    /******************************************************************/

    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The interpreter.
     * @return True if the goal succeeded, otherwise false.
     * @throws EngineException FFI error.
     * @throws EngineMessage   FFI error.
     */
    public final boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        Object temp = en.skel;
        Display ref = en.display;
        int hint = en.store.foyer.getHint();
        Object obj = convertRecv(temp, ref);
        Object[] args = convertArgs(temp, ref, en, null);
        switch (hint) {
            case Foyer.HINT_WEB:
                checkRecv(obj);
                checkArgs(args);
                break;
            default:
                break;
        }
        Object res = invokeMethod(method, obj, args, en);
        if ((subflags & MASK_METH_FUNC) != 0) {
            res = Types.normJava(encoderet, res);
        } else {
            res = Types.noretNormJava(encoderet, res);
        }
        if (res == null)
            return false;
        Display d = AbstractTerm.getDisplay(res);
        Object[] help;
        if (res != AbstractSkel.VOID_OBJ &&
                !en.unifyTerm((help = ((SkelCompound) temp).args)[help.length - 1], ref,
                        AbstractTerm.getSkel(res), d))
            return false;
        ResetableBit check = AbstractTerm.getMarker(res);
        if (check != null && check.getBit()) {
            BindCount.remTab(d.bind, en);
            check.resetBit();
        }
        return en.getNext();
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
        if (!(o instanceof MemberMethodDet))
            return false;
        MemberMethodDet dpmd = (MemberMethodDet) o;
        return method.equals(dpmd.method);
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
        return new SkelCompound(new SkelAtom(OP_FOREIGN),
                SpecialForeign.classToName(method.getDeclaringClass(), source, en),
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
        return method.toString();
    }

}
