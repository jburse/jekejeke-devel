package jekpro.reference.arithmetic;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SpecialCompare extends AbstractSpecial {
    private final static int SPECIAL_COMPARE_EQ = 0;
    private final static int SPECIAL_COMPARE_NQ = 1;
    private final static int SPECIAL_COMPARE_LS = 2;
    private final static int SPECIAL_COMPARE_LQ = 3;
    private final static int SPECIAL_COMPARE_GQ = 5;
    private final static int SPECIAL_DIVMOD = 6;
    private final static int SPECIAL_NUMBER_COMPARE = 7;

    public static final int NUM_INTEGER = 0;
    public static final int NUM_BIG_INTEGER = 1;
    public static final int NUM_FLOAT = 2;
    public static final int NUM_DOUBLE = 3;
    public static final int NUM_LONG = 4;
    public static final int NUM_BIG_DECIMAL = 5;

    public final static double EPSILON = Math.ulp(1.0);
    public final static float EPSILON32 = Math.ulp(1.0f);

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
     * <p>Logically evaluate a term in a list of goals for the first time.</p>
     * <p>The term is passed via the skel and display of the engine.</p>
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
            case SPECIAL_COMPARE_EQ:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.computeExpr(temp[0], ref);
                Display d = en.display;
                boolean multi = d.getAndReset();
                Number alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                if (multi)
                    d.remTab(en);
                en.computeExpr(temp[1], ref);
                d = en.display;
                multi = d.getAndReset();
                Number beta = SpecialEval.derefAndCastNumber(en.skel, d);
                if (multi)
                    d.remTab(en);
                if (!SpecialCompare.testEq(alfa, beta))
                    return false;
                return true;
            case SPECIAL_COMPARE_NQ:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref);
                d = en.display;
                multi = d.getAndReset();
                alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                if (multi)
                    d.remTab(en);
                en.computeExpr(temp[1], ref);
                d = en.display;
                multi = d.getAndReset();
                beta = SpecialEval.derefAndCastNumber(en.skel, d);
                if (multi)
                    d.remTab(en);
                if (SpecialCompare.testEq(alfa, beta))
                    return false;
                return true;
            case SPECIAL_COMPARE_LS:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref);
                d = en.display;
                multi = d.getAndReset();
                alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                if (multi)
                    d.remTab(en);
                en.computeExpr(temp[1], ref);
                d = en.display;
                multi = d.getAndReset();
                beta = SpecialEval.derefAndCastNumber(en.skel, d);
                if (multi)
                    d.remTab(en);
                if (SpecialCompare.computeCmp(alfa, beta) >= 0)
                    return false;
                return true;
            case SPECIAL_COMPARE_LQ:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref);
                d = en.display;
                multi = d.getAndReset();
                alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                if (multi)
                    d.remTab(en);
                en.computeExpr(temp[1], ref);
                d = en.display;
                multi = d.getAndReset();
                beta = SpecialEval.derefAndCastNumber(en.skel, d);
                if (multi)
                    d.remTab(en);
                if (SpecialCompare.computeCmp(alfa, beta) > 0)
                    return false;
                return true;
            case SPECIAL_COMPARE_GQ:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                en.computeExpr(temp[0], ref);
                d = en.display;
                multi = d.getAndReset();
                alfa = SpecialEval.derefAndCastNumber(en.skel, d);
                if (multi)
                    d.remTab(en);
                en.computeExpr(temp[1], ref);
                d = en.display;
                multi = d.getAndReset();
                beta = SpecialEval.derefAndCastNumber(en.skel, d);
                if (multi)
                    d.remTab(en);
                if (SpecialCompare.computeCmp(alfa, beta) < 0)
                    return false;
                return true;
            case SPECIAL_DIVMOD:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                alfa = SpecialEval.derefAndCastNumber(temp[0], ref);
                beta = SpecialEval.derefAndCastNumber(temp[1], ref);
                Number[] res = divMod(alfa, beta);
                if (!en.unifyTerm(temp[2], ref, res[0], Display.DISPLAY_CONST))
                    return false;
                if (!en.unifyTerm(temp[3], ref, res[1], Display.DISPLAY_CONST))
                    return false;
                return true;
            case SPECIAL_NUMBER_COMPARE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                alfa = SpecialEval.derefAndCastNumber(temp[1], ref);
                beta = SpecialEval.derefAndCastNumber(temp[2], ref);
                int res2 = SpecialCompare.computeCmp(alfa, beta) ;
                if (!en.unifyTerm(temp[0], ref,
                        SpecialLexical.compAtom(res2, en), Display.DISPLAY_CONST))
                    return false;
                return true;
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
     * @throws EngineMessage Not a Prolog number.
     */
    public static boolean testEq(Number m, Number n)
            throws EngineMessage {
        switch (Math.max(SpecialCompare.numType(m), SpecialCompare.numType(n))) {
            case SpecialCompare.NUM_INTEGER:
            case SpecialCompare.NUM_BIG_INTEGER:
                return m.equals(n);
            case SpecialCompare.NUM_FLOAT:
                return m.floatValue() == n.floatValue();
            case SpecialCompare.NUM_DOUBLE:
                return m.doubleValue() == n.doubleValue();
            case SpecialCompare.NUM_LONG:
            case SpecialCompare.NUM_BIG_DECIMAL:
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
     * @throws EngineMessage Not a Prolog number.
     */
    public static int computeCmp(Number m, Number n)
            throws EngineMessage {
        switch (Math.max(SpecialCompare.numType(m), SpecialCompare.numType(n))) {
            case SpecialCompare.NUM_INTEGER:
            case SpecialCompare.NUM_BIG_INTEGER:
                return SpecialCompare.compareIntegerArithmetical(m, n);
            case SpecialCompare.NUM_FLOAT:
                float x = m.floatValue();
                float y = n.floatValue();
                if (x < y) return -1;
                if (x == y) return 0;
                return 1;
            case SpecialCompare.NUM_DOUBLE:
                double a = m.doubleValue();
                double b = n.doubleValue();
                if (a < b) return -1;
                if (a == b) return 0;
                return 1;
            case SpecialCompare.NUM_LONG:
            case SpecialCompare.NUM_BIG_DECIMAL:
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
     * @throws EngineMessage Not a Prolog number.
     */
    public static int numType(Number m) throws EngineMessage {
        if (m instanceof Integer) {
            return SpecialCompare.NUM_INTEGER;
        } else if (m instanceof BigInteger) {
            return SpecialCompare.NUM_BIG_INTEGER;
        } else if (m instanceof Float) {
            return SpecialCompare.NUM_FLOAT;
        } else if (m instanceof Double) {
            return SpecialCompare.NUM_DOUBLE;
        } else if (m instanceof Long) {
            return SpecialCompare.NUM_LONG;
        } else if (m instanceof BigDecimal) {
            return SpecialCompare.NUM_BIG_DECIMAL;
        } else {
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_STRICT, m));
        }
    }


    /**
     * <p>Compare two Prolog integers lexically.</p>
     *
     * @param alfa The first Prolog integer.
     * @param beta The second Prolog integer.
     * @return <0 alfa < beta, 0 alfa = beta, >0 alfa > beta
     */
    public static int compareIntegerArithmetical(Object alfa, Object beta) {
        if (alfa instanceof Integer) {
            if (beta instanceof Integer) {
                return ((Integer) alfa).compareTo((Integer) beta);
            } else {
                return -((BigInteger) beta).signum();
            }
        } else {
            if (beta instanceof Integer) {
                return ((BigInteger) alfa).signum();
            } else {
                return ((BigInteger) alfa).compareTo((BigInteger) beta);
            }
        }
    }

    /********************************************************************/
    /* Rounding Operations (Supplement):                                */
    /*      divmod/4: divMod()                                          */
    /********************************************************************/

    /**
     * <p>Divide and truncate the two numbers, and also compute modulo.</p>
     * <p>The results corresponds to the truncation of the real division.</p>
     * <pre>
     *       divmod(X,Y,Z,T) where
     *             Z = integer(floor(X / Y)) and
     *             T = X - Z * Y
     * </pre>
     *
     * @param a The first operand.
     * @param b The second operand.
     * @return The division result and the modulo.
     * @throws ArithmeticException Illegal value.
     * @throws EngineMessage Not a Prolog value.
     */
    private static Number[] divMod(Number a, Number b)
            throws ArithmeticException, EngineMessage {
        switch (Math.max(SpecialCompare.numType(a), SpecialCompare.numType(b))) {
            case SpecialCompare.NUM_INTEGER:
                int u = b.intValue();
                if (u == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                Number[] res = new Number[2];
                int v = a.intValue();
                res[0] = TermAtomic.normBigInteger(floorDiv(v, u));
                res[1] = Integer.valueOf(floorMod(v, u));
                return res;
            case SpecialCompare.NUM_BIG_INTEGER:
                BigInteger p = TermAtomic.widenBigInteger(b);
                if (p.signum() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                res = new Number[2];
                BigInteger[] res2 = floorDivMod(TermAtomic.widenBigInteger(a), p);
                res[0] = TermAtomic.normBigInteger(res2[0]);
                res[1] = TermAtomic.normBigInteger(res2[1]);
                return res;
            case SpecialCompare.NUM_FLOAT:
                float f = b.floatValue();
                if (f == 0.0f)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                res = new Number[2];
                float g1 = a.floatValue();
                res[0] = EvaluableRound.toInteger((float)Math.floor(g1 / f));
                res[1] = TermAtomic.makeFloat(floorMod(g1,f));
                return res;
            case SpecialCompare.NUM_DOUBLE:
                double d = b.doubleValue();
                if (d == 0.0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                res = new Number[2];
                double e1 = a.doubleValue();
                res[0] = EvaluableRound.toInteger(Math.floor(e1 / d));
                res[1] = TermAtomic.makeDouble(floorMod(e1,d));
                return res;
            case SpecialCompare.NUM_LONG:
            case SpecialCompare.NUM_BIG_DECIMAL:
                BigDecimal h = TermAtomic.widenBigDecimal(b);
                if (h.signum() == 0)
                    throw new ArithmeticException(
                            EngineMessage.OP_EVALUATION_ZERO_DIVISOR);
                res = new Number[2];
                BigDecimal j = TermAtomic.widenBigDecimal(a);
                BigDecimal k = j.divide(h, 0, BigDecimal.ROUND_FLOOR);
                res[0] = TermAtomic.normBigInteger(k.unscaledValue());
                res[1] = TermAtomic.normBigDecimal(j.subtract(k.multiply(h)));
                return res;
            default:
                throw new IllegalArgumentException(SpecialCompare.OP_ILLEGAL_CATEGORY);
        }
    }

    /**
     * <p>Compute the floor div.</p>
     *
     * @param v The numerator.
     * @param u The denumerator.
     * @return The floor div.
     */
    public static long floorDiv(int v, int u) {
        if ((v < 0) != (u < 0)) {
            int res = v % u;
            long h = (long) v / u;
            if (res != 0) {
                return h - 1;
            } else {
                return h;
            }
        } else {
            return (long) v / u;
        }
    }

    /**
     * <p>Compute the floor mod.</p>
     *
     * @param v The numerator.
     * @param u The denumerator.
     * @return The floor div.
     */
    public static int floorMod(int v, int u) {
        if ((v < 0) != (u < 0)) {
            int res = v % u;
            if (res != 0) {
                return res + u;
            } else {
                return res;
            }
        } else {
            return v % u;
        }
    }

    /**
     * <p>Compute the floor div.</p>
     *
     * @param v The numerator.
     * @param u The denumerator.
     * @return The floor div.
     */
    public static BigInteger floorDiv(BigInteger v, BigInteger u) {
        if ((v.signum() < 0) != (u.signum() < 0)) {
            BigInteger[] res = v.divideAndRemainder(u);
            if (res[1].signum() != 0) {
                return res[0].subtract(BigInteger.ONE);
            } else {
                return res[0];
            }
        } else {
            return v.divide(u);
        }
    }

    /**
     * <p>Compute the floor mod.</p>
     *
     * @param v The numerator.
     * @param u The denumerator.
     * @return The floor mod.
     */
    public static BigInteger floorMod(BigInteger v, BigInteger u) {
        if ((v.signum() < 0) != (u.signum() < 0)) {
            BigInteger res = v.remainder(u);
            if (res.signum() != 0) {
                return res.add(u);
            } else {
                return res;
            }
        } else {
            return v.remainder(u);
        }
    }

    /**
     * <p>Compute the floor div.</p>
     *
     * @param v The numerator.
     * @param u The denumerator.
     * @return The floor div.
     */
    public static BigInteger[] floorDivMod(BigInteger v, BigInteger u) {
        if ((v.signum() < 0) != (u.signum() < 0)) {
            BigInteger[] res = v.divideAndRemainder(u);
            if (res[1].signum() != 0) {
                res[0] = res[0].subtract(BigInteger.ONE);
                res[1] = res[1].add(u);
                return res;
            } else {
                return res;
            }
        } else {
            return v.divideAndRemainder(u);
        }
    }

    /**
     * <p>Compute the floor mod.</p>
     *
     * @param v The numerator.
     * @param u The denumerator.
     * @return The floor div.
     */
    public static float floorMod(float v, float u) {
        if ((v < 0) != (u < 0)) {
            float res = v % u;
            if (res != 0) {
                return res + u;
            } else {
                return res;
            }
        } else {
            return v % u;
        }
    }

    /**
     * <p>Compute the floor mod.</p>
     *
     * @param v The numerator.
     * @param u The denumerator.
     * @return The floor div.
     */
    public static double floorMod(double v, double u) {
        if ((v < 0) != (u < 0)) {
            double res = v % u;
            if (res != 0) {
                return res + u;
            } else {
                return res;
            }
        } else {
            return v % u;
        }
    }

    public static void main(String[] args) {
        /*
        int x=7;
        int y=5;
        System.out.println("x="+x+", y="+y+", floorDiv(x,y)="+floorDiv(x,y));
        System.out.println("x="+x+", y="+y+", floorMod(x,y)="+floorMod(x,y));
        x=-7;
        y=5;
        System.out.println("x="+x+", y="+y+", floorDiv(x,y)="+floorDiv(x,y));
        System.out.println("x="+x+", y="+y+", floorMod(x,y)="+floorMod(x,y));
        x=7;
        y=-5;
        System.out.println("x="+x+", y="+y+", floorDiv(x,y)="+floorDiv(x,y));
        System.out.println("x="+x+", y="+y+", floorMod(x,y)="+floorMod(x,y));
        x=-7;
        y=-5;
        System.out.println("x="+x+", y="+y+", floorDiv(x,y)="+floorDiv(x,y));
        System.out.println("x="+x+", y="+y+", floorMod(x,y)="+floorMod(x,y));
        */
        /*
        BigInteger x = BigInteger.valueOf(7);
        BigInteger y = BigInteger.valueOf(5);
        System.out.println("x=" + x + ", y=" + y + ", floorDiv(x,y)=" + floorDiv(x, y));
        System.out.println("x=" + x + ", y=" + y + ", floorMod(x,y)=" + floorMod(x, y));
        x = BigInteger.valueOf(7);
        y = BigInteger.valueOf(-5);
        System.out.println("x=" + x + ", y=" + y + ", floorDiv(x,y)=" + floorDiv(x, y));
        System.out.println("x=" + x + ", y=" + y + ", floorMod(x,y)=" + floorMod(x, y));
        x = BigInteger.valueOf(-7);
        y = BigInteger.valueOf(5);
        System.out.println("x=" + x + ", y=" + y + ", floorDiv(x,y)=" + floorDiv(x, y));
        System.out.println("x=" + x + ", y=" + y + ", floorMod(x,y)=" + floorMod(x, y));
        x = BigInteger.valueOf(-7);
        y = BigInteger.valueOf(-5);
        System.out.println("x=" + x + ", y=" + y + ", floorDiv(x,y)=" + floorDiv(x, y));
        System.out.println("x=" + x + ", y=" + y + ", floorMod(x,y)=" + floorMod(x, y));
        */
        /*
        BigInteger x = BigInteger.valueOf(7);
        BigInteger y = BigInteger.valueOf(5);
        System.out.println("x=" + x + ", y=" + y + ", floorDiv(x,y)=" + floorDivMod(x, y)[0]);
        System.out.println("x=" + x + ", y=" + y + ", floorMod(x,y)=" + floorDivMod(x, y)[1]);
        x = BigInteger.valueOf(7);
        y = BigInteger.valueOf(-5);
        System.out.println("x=" + x + ", y=" + y + ", floorDiv(x,y)=" + floorDivMod(x, y)[0]);
        System.out.println("x=" + x + ", y=" + y + ", floorMod(x,y)=" + floorDivMod(x, y)[1]);
        x = BigInteger.valueOf(-7);
        y = BigInteger.valueOf(5);
        System.out.println("x=" + x + ", y=" + y + ", floorDiv(x,y)=" + floorDivMod(x, y)[0]);
        System.out.println("x=" + x + ", y=" + y + ", floorMod(x,y)=" + floorDivMod(x, y)[1]);
        x = BigInteger.valueOf(-7);
        y = BigInteger.valueOf(-5);
        System.out.println("x=" + x + ", y=" + y + ", floorDiv(x,y)=" + floorDivMod(x, y)[0]);
        System.out.println("x=" + x + ", y=" + y + ", floorMod(x,y)=" + floorDivMod(x, y)[1]);
        */
    }

}
