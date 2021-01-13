package jekpro.tools.foreign;

import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.array.Types;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.proxy.RuntimeWrap;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelCompound;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Member;
import java.lang.reflect.Method;

/**
 * <p>Specialization of a delegate for a deterministic predicate.</p>
 * <p>Non-static Java method is called via invokevirtual.</p>
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
final class MemberVirtualDet extends AbstractMember {
    static final String OP_FOREIGN = "foreign";

    private final Method method;

    /**
     * <p>Create method predicate.</p>
     *
     * @param m The method.
     */
    MemberVirtualDet(Method m) {
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

    /**
     * <p>Retrieve the variant that is wrapped.</p>
     *
     * @return The variant.
     */
    public String getVariant() {
        return OP_FOREIGN;
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

    /******************************************************************/
    /* Variation Points Predicate                                     */
    /******************************************************************/

    /**
     * <p>Arithmetically evaluate a compound.</p>
     * <p>The evaluable is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the contskel and contdisplay of the engine.</p>
     * <p>The result is passed via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @throws EngineMessage   FFI error.
     * @throws EngineException FFI error.
     */
    public final void moniEvaluate(Engine en)
            throws EngineMessage, EngineException {
        Object temp = en.skel;
        Display ref = en.display;
        Object obj = convertRecv(temp, ref);
        if ((en.store.foyer.getHint() & Foyer.HINT_MASK_LMTD) != 0)
            checkRecv(obj);
        Object[] args = computeAndConvertArgs(temp, ref, en);
        if ((en.store.foyer.getHint() & Foyer.HINT_MASK_LMTD) != 0)
            checkArgs(args);
        Object res = invokeVirtual(method, obj, args);
        res = Types.normJava(encoderet, res);
        if (res == null)
            throw new EngineMessage(EngineMessage.representationError(
                    AbstractFactory.OP_REPRESENTATION_NULL));
        en.skel = AbstractTerm.getSkel(res);
        en.display = AbstractTerm.getDisplay(res);
    }

    /**
     * <p>Logically evaluate a term in a list of goals for the first time.</p>
     * <p>The term is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The interpreter.
     * @return True if the term succeeded, otherwise false.
     * @throws EngineException FFI error.
     * @throws EngineMessage   FFI error.
     */
    public final boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        Object temp = en.skel;
        Display ref = en.display;
        Object obj = convertRecv(temp, ref);
        if ((en.store.foyer.getHint() & Foyer.HINT_MASK_LMTD) != 0)
            checkRecv(obj);
        Object[] args = convertArgs(temp, ref, en, null);
        if ((en.store.foyer.getHint() & Foyer.HINT_MASK_LMTD) != 0)
            checkArgs(args);
        Object res = invokeVirtual(method, obj, args);
        if ((subflags & MASK_METH_FUNC) != 0) {
            res = Types.normJava(encoderet, res);
        } else {
            res = Types.noretNormJava(encoderet, res);
        }
        if (res == null)
            return false;
        Display d = AbstractTerm.getDisplay(res);
        Object[] help;
        boolean ext = d.getAndReset();
        if (res != AbstractSkel.VOID_OBJ &&
                !BindUniv.unifyTerm(AbstractTerm.getSkel(res), d,
                        (help = ((SkelCompound) temp).args)[help.length - 1], ref, en))
            return false;
        if (ext)
            d.remTab(en);
        return true;
    }

    /**
     * <p>Invoke the method.</p>
     *
     * @param obj  The receiver.
     * @param args The arguments array.
     * @return The invokcation result.
     * @throws EngineException FFI error.
     * @throws EngineMessage   FFI error.
     */
    static Object invokeVirtual(Method method, Object obj,
                                Object[] args)
            throws EngineException, EngineMessage {
        try {
            return method.invoke(obj, args);
        } catch (InvocationTargetException y) {
            Throwable x = y.getCause();
            if (x instanceof RuntimeWrap)
                x = x.getCause();
            if (x instanceof InterpreterException) {
                throw (EngineException) ((InterpreterException) x).getException();
            } else {
                throw Types.mapThrowable(x);
            }
        } catch (Exception x) {
            throw Types.mapException(x, method);
        } catch (Error x) {
            throw Types.mapError(x);
        }
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
        return method.toString() + " (virtual)";
    }

}
