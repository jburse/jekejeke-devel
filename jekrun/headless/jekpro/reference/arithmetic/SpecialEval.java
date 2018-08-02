package jekpro.reference.arithmetic;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindVar;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * <p>Provides built-in predicates for the eval theory.</p>
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
public final class SpecialEval extends AbstractSpecial {
    private final static int SPECIAL_IS = 0;

    /**
     * <p>Create an arithmetic special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialEval(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case SPECIAL_IS:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                boolean multi = en.computeExpr(temp[1], ref);
                Display d = en.display;
                if (!en.unifyTerm(temp[0], ref, en.skel, d))
                    return false;
                if (multi)
                    d.remTab(en);
                return en.getNext();
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /*****************************************************************/
    /* Deref And Cast                                                */
    /*****************************************************************/

    /**
     * <p>Check whether the given term is a Prolog number.</p>
     *
     * @param t The term skel.
     * @param d The display skel.
     * @return The number.
     * @throws EngineMessage Not a number.
     */
    public static Number derefAndCastNumber(Object t, Display d)
            throws EngineMessage {
        BindVar b;
        while (t instanceof SkelVar &&
                (b = d.bind[((SkelVar) t).id]).display != null) {
            t = b.skel;
            d = b.display;
        }
        if (t instanceof Number) {
            return (Number) t;
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_NUMBER, t), d);
        }
    }

    /**
     * <p>Check whether the given term is a Prolog integer.</p>
     *
     * @param t The term skel.
     * @param d The display skel.
     * @return The integer, either Integer or BigInteger.
     * @throws EngineMessage Not a integer.
     */
    public static Number derefAndCastInteger(Object t, Display d)
            throws EngineMessage {
        BindVar b;
        while (t instanceof SkelVar &&
                (b = d.bind[((SkelVar) t).id]).display != null) {
            t = b.skel;
            d = b.display;
        }
        if (t instanceof Integer) {
            return (Integer) t;
        } else if (t instanceof BigInteger) {
            return (BigInteger) t;
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_INTEGER, t), d);
        }
    }

    /**
     * <p>Check whether the given term is a Prolog decimal.</p>
     *
     * @param t The term skel.
     * @param d The display skel.
     * @return The decimal, either Long or BigDecimal.
     * @throws EngineMessage Not a integer.
     */
    public static Number derefAndCastDecimal(Object t, Display d)
            throws EngineMessage {
        BindVar b;
        while (t instanceof SkelVar &&
                (b = d.bind[((SkelVar) t).id]).display != null) {
            t = b.skel;
            d = b.display;
        }
        if (t instanceof Long) {
            return (Long) t;
        } else if (t instanceof BigDecimal) {
            return (BigDecimal) t;
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_DECIMAL, t), d);
        }
    }

}
