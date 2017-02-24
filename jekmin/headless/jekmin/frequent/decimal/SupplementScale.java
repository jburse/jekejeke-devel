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

import java.math.BigDecimal;
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
                en.skel = Integer.valueOf(precision(alfa));
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

    /********************************************************************/
    /* Decimal Accessors                                                */
    /********************************************************************/

    /**
     * <p>Retrieve the precision.</p>
     *
     * @param n The decimal.
     * @return The precision.
     */
    public static int precision(Number n) {
        if (n instanceof Long) {
            return log10(n.longValue());
        } else {
            return ((BigDecimal) n).precision();
        }
    }

    /**
     * <p>Denormalize the given number to a big decimal.</p>
     * <p>If the number is of type long then it is converted to a big decimal.</p>
     * <p>If the number is of type big decimal then it is returned.</p>
     * <p>Otherwise an exception is thrown.</p>
     *
     * @param m  The number.
     * @param mc The math context.
     * @return The big decimal.
     */
    public static BigDecimal widenBigDecimal(Number m, MathContext mc) {
        if (m instanceof Integer) {
            if (mc.getPrecision() != 0 &&
                    (log10(m.intValue()) > mc.getPrecision())) {
                return new BigDecimal(m.intValue(), mc);
            } else {
                return BigDecimal.valueOf(m.intValue());
            }
        } else if (m instanceof BigInteger) {
            return new BigDecimal((BigInteger) m, mc);
        } else if (m instanceof Long) {
            if (mc.getPrecision() != 0 &&
                    (log10(m.longValue()) > mc.getPrecision())) {
                return new BigDecimal(m.longValue(), mc);
            } else {
                return BigDecimal.valueOf(m.longValue());
            }
        } else if (m instanceof BigDecimal) {
            BigDecimal d = (BigDecimal) m;
            if (mc.getPrecision() != 0 &&
                    (d.precision() > mc.getPrecision())) {
                return new BigDecimal(d.unscaledValue(), d.scale(), mc);
            } else {
                return d;
            }
        } else {
            return new BigDecimal(m.doubleValue(), mc);
        }
    }

    /********************************************************************/
    /* Decimal Helpers                                                  */
    /********************************************************************/

    /**
     * <p>Ceiling of log base 10 of the absolute value.</p>
     *
     * @param x The argument.
     * @return The logarithm of the argument.
     */
    public static int log10(long x) {
        long[] tab = LONG_TEN_POWERS_TABLE;
        if (x == Long.MIN_VALUE)
            return tab.length;
        if (x < 0)
            x = -x;
        if (x < 10)
            return 1;
        int r = ((64 - Long.numberOfLeadingZeros(x) + 1) * 1233) >>> 12;
        return (r >= tab.length || x < tab[r]) ? r : r + 1;
    }

    /**
     * <p>The base 10 table.</p>
     */
    private static final long[] LONG_TEN_POWERS_TABLE = {
            1,                     // 0 / 10^0
            10,                    // 1 / 10^1
            100,                   // 2 / 10^2
            1000,                  // 3 / 10^3
            10000,                 // 4 / 10^4
            100000,                // 5 / 10^5
            1000000,               // 6 / 10^6
            10000000,              // 7 / 10^7
            100000000,             // 8 / 10^8
            1000000000,            // 9 / 10^9
            10000000000L,          // 10 / 10^10
            100000000000L,         // 11 / 10^11
            1000000000000L,        // 12 / 10^12
            10000000000000L,       // 13 / 10^13
            100000000000000L,      // 14 / 10^14
            1000000000000000L,     // 15 / 10^15
            10000000000000000L,    // 16 / 10^16
            100000000000000000L,   // 17 / 10^17
            1000000000000000000L   // 18 / 10^18
    };

}
