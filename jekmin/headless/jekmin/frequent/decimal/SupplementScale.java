package jekmin.frequent.decimal;

import jekpro.model.inter.Engine;
import jekpro.model.inter.Special;
import jekpro.model.molec.Display;
import jekpro.model.molec.DisplayClause;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Goal;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;

import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

/**
 * <p>This module provides built-ins for access of decimals.</p>
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
public class SupplementScale extends Special {
    private final static int EVALUABLE_SCALE = 0;
    private final static int EVALUABLE_UNSCALED_VALUE = 1;
    private final static int EVALUABLE_NEW_DECIMAL = 2;
    private final static int EVALUABLE_PRECISION = 3;
    private final static int EVALUABLE_REQUESTED = 4;
    private final static int EVALUABLE_NEW_CONTEXT = 5;

    /**
     * <p>Create a decimal access special.</p>
     *
     * @param i The id.
     */
    public SupplementScale(int i) {
        super(i);
        subflags |= MASK_DELE_ARIT;
    }

    /**
     * <p>Arithmetically evaluate an evaluable.</p>
     * <p>The evaluable is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The result is passed via the skel and display of the engine.</p>
     *
     * @param r  The continuation skel.
     * @param u  The continuation display.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public final void evalEvaluable(Goal r, DisplayClause u, Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case EVALUABLE_SCALE:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.computeExpr(temp[0], ref, r, u);
                Number alfa = EngineMessage.castDecimal(en.skel, en.display);
                en.skel = Integer.valueOf(TermAtomic.scale(alfa));
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_UNSCALED_VALUE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref, r, u);
                alfa = EngineMessage.castDecimal(en.skel, en.display);
                en.skel = TermAtomic.normBigInteger(TermAtomic.unscaledValue(alfa));
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_NEW_DECIMAL:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref, r, u);
                alfa = EngineMessage.castInteger(en.skel, en.display);
                en.computeExpr(temp[1], ref, r, u);
                Number beta = EngineMessage.castInteger(en.skel, en.display);
                en.skel = newDecimal(alfa, beta);
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_PRECISION:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref, r, u);
                alfa = EngineMessage.castDecimal(en.skel, en.display);
                en.skel = Integer.valueOf(TermAtomic.precision(alfa));
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_REQUESTED:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref, r, u);
                MathContext mc = SpecialArith.castContext(en.skel, en.display);
                en.skel = Integer.valueOf(mc.getPrecision());
                en.display = Display.DISPLAY_CONST;
                return;
            case EVALUABLE_NEW_CONTEXT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref, r, u);
                alfa = EngineMessage.castInteger(en.skel, en.display);
                en.skel = newContext(alfa);
                en.display = Display.DISPLAY_CONST;
                return;
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /********************************************************************/
    /* Additional Unary Decimal Built-in:                               */
    /*      new_conext/2: newContext()                                  */
    /********************************************************************/

    /**
     * <p>Create a new context.</p>
     *
     * @param m The first operand.
     * @return The result.
     * @throws EngineMessage Shit happens.
     */
    private static MathContext newContext(Number m)
            throws EngineMessage {
        EngineMessage.checkNotLessThanZero(m);
        int k = EngineMessage.castIntValue(m);
        return new MathContext(k, RoundingMode.HALF_EVEN);
    }

    /********************************************************************/
    /* Additional Binary Decimal Built-in:                              */
    /*      new_decimal/3: newDecimal()                                 */
    /********************************************************************/

    /**
     * <p>Create a new decimal.</p>
     *
     * @param m The first operand.
     * @param n The second operand.
     * @return The result.
     * @throws EngineMessage Shit happens.
     */
    private static Number newDecimal(Number m, Number n)
            throws EngineMessage {
        int k = EngineMessage.castIntValue(n);
        if (m instanceof Integer) {
            return TermAtomic.normBigDecimal(m.intValue(), k);
        } else {
            return TermAtomic.normBigDecimal((BigInteger) m, k);
        }
    }

}
