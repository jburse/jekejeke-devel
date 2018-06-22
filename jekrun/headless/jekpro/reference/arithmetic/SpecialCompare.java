package jekpro.reference.arithmetic;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.reference.structure.SpecialLexical;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.TermAtomic;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * <p>Provides built-in predicates for the compare theory.</p>
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
public final class SpecialCompare extends AbstractSpecial {
    private final static int SPECIAL_COMPARE_EQ = 0;
    private final static int SPECIAL_COMPARE_NQ = 1;
    private final static int SPECIAL_COMPARE_LS = 2;
    private final static int SPECIAL_COMPARE_LQ = 3;
    private final static int SPECIAL_COMPARE_GR = 4;
    private final static int SPECIAL_COMPARE_GQ = 5;

    public static final int CATEGORY_INTEGER = 0;
    public static final int CATEGORY_BIG_INTEGER = 1;
    public static final int CATEGORY_FLOAT = 2;
    public static final int CATEGORY_DOUBLE = 3;
    public static final int CATEGORY_LONG = 4;
    public static final int CATEGORY_BIG_DECIMAL = 5;

    public static final String OP_ILLEGAL_CATEGORY = "illegal category";

    /**
     * <p>Create an compare special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialCompare(int i) {
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
        Object[] temp = ((SkelCompound) en.skel).args;
        Display ref = en.display;
        en.computeExpr(temp[0], ref);
        Number val = EngineMessage.castNumber(en.skel, en.display);
        en.computeExpr(temp[1], ref);
        Number val2 = EngineMessage.castNumber(en.skel, en.display);
        switch (id) {
            case SPECIAL_COMPARE_EQ:
                if (!SpecialCompare.testEq(val, val2))
                    return false;
                return en.getNextRaw();
            case SPECIAL_COMPARE_NQ:
                if (SpecialCompare.testEq(val, val2))
                    return false;
                return en.getNextRaw();
            case SPECIAL_COMPARE_LS:
                if (SpecialCompare.computeCmp(val, val2) >= 0)
                    return false;
                return en.getNextRaw();
            case SPECIAL_COMPARE_LQ:
                if (SpecialCompare.computeCmp(val, val2) > 0)
                    return false;
                return en.getNextRaw();
            case SPECIAL_COMPARE_GR:
                if (SpecialCompare.computeCmp(val, val2) <= 0)
                    return false;
                return en.getNextRaw();
            case SPECIAL_COMPARE_GQ:
                if (SpecialCompare.computeCmp(val, val2) < 0)
                    return false;
                return en.getNextRaw();
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /********************************************************************/
    /* Arithmetic Comparison:                                           */
    /*      (<)/2, (>)/2, (=<)/2, (>=)/2: cmp()                         */
    /*      (=:=)/2, (=\=)/2: eqTerm()                                  */
    /********************************************************************/

    /**
     * <p>Check whether two Prolog numbers are equal.</p>
     *
     * @param m The first Prolog number.
     * @param n The second Prolog number.
     * @return True if they are equal, false otherwise.
     * @throws EngineMessage Shit happens.
     */
    public static boolean testEq(Number m, Number n) throws EngineMessage {
        switch (Math.max(SpecialCompare.category(m), SpecialCompare.category(n))) {
            case SpecialCompare.CATEGORY_INTEGER:
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                return m.equals(n);
            case SpecialCompare.CATEGORY_FLOAT:
                return m.floatValue() == n.floatValue();
            case SpecialCompare.CATEGORY_DOUBLE:
                return m.doubleValue() == n.doubleValue();
            case SpecialCompare.CATEGORY_LONG:
            case SpecialCompare.CATEGORY_BIG_DECIMAL:
                return TermAtomic.widenBigDecimal(m).compareTo(
                        TermAtomic.widenBigDecimal(n)) == 0;
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Compare two Prolog numbers.</p>
     *
     * @param m The first Prolog number.
     * @param n The second Prolog number.
     * @return <0 m < n,  0 m == m, >0 m > n.
     */
    public static int computeCmp(Number m, Number n) {
        switch (Math.max(SpecialCompare.category(m), SpecialCompare.category(n))) {
            case SpecialCompare.CATEGORY_INTEGER:
            case SpecialCompare.CATEGORY_BIG_INTEGER:
                return SpecialLexical.compareInteger(m, n);
            case SpecialCompare.CATEGORY_FLOAT:
                float x = m.floatValue();
                float y = n.floatValue();
                if (x < y) return -1;
                if (x == y) return 0;
                return 1;
            case SpecialCompare.CATEGORY_DOUBLE:
                double a = m.doubleValue();
                double b = n.doubleValue();
                if (a < b) return -1;
                if (a == b) return 0;
                return 1;
            case SpecialCompare.CATEGORY_LONG:
            case SpecialCompare.CATEGORY_BIG_DECIMAL:
                return TermAtomic.widenBigDecimal(m).compareTo(
                        TermAtomic.widenBigDecimal(n));
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Determine the category of a Prolog number.</p>
     *
     * @param m The Prolog number.
     * @return The category.
     */
    public static int category(Number m) {
        if (m instanceof Integer) {
            return SpecialCompare.CATEGORY_INTEGER;
        } else if (m instanceof BigInteger) {
            return SpecialCompare.CATEGORY_BIG_INTEGER;
        } else if (m instanceof Float) {
            return SpecialCompare.CATEGORY_FLOAT;
        } else if (m instanceof Double) {
            return SpecialCompare.CATEGORY_DOUBLE;
        } else if (m instanceof Long) {
            return SpecialCompare.CATEGORY_LONG;
        } else if (m instanceof BigDecimal) {
            return SpecialCompare.CATEGORY_BIG_DECIMAL;
        } else {
            throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

}
