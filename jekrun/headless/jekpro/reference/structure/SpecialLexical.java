package jekpro.reference.structure;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.reference.arithmetic.SpecialCompare;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Comparator;

/**
 * <p>Provides built-in predicates for lexical comparison.</p>
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
public final class SpecialLexical extends AbstractSpecial {
    private final static int SPECIAL_LEX_EQ = 0;
    private final static int SPECIAL_LEX_NQ = 1;
    private final static int SPECIAL_LEX_LS = 2;
    private final static int SPECIAL_LEX_LQ = 3;
    private final static int SPECIAL_LEX_GR = 4;
    private final static int SPECIAL_LEX_GQ = 5;
    private final static int SPECIAL_COMPARE = 6;
    private final static int SPECIAL_LOCALE_COMPARE = 7;

    /* the syntactic equality categories */
    public final static int EQ_TYPE_VAR = 0;
    public final static int EQ_TYPE_ATOMIC = 1;
    public final static int EQ_TYPE_COMPOUND = 2;

    /**
     * <p>Create an compare special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialLexical(int i) {
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
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        try {
            switch (id) {
                case SPECIAL_LEX_EQ:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    BindCount[] ref = en.display;
                    if (!equalTerm(temp[0], ref, temp[1], ref))
                        return false;
                    return en.getNextRaw();
                case SPECIAL_LEX_NQ:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (equalTerm(temp[0], ref, temp[1], ref))
                        return false;
                    return en.getNextRaw();
                case SPECIAL_LEX_LS:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (SpecialLexical.compareTerm(temp[0], ref,
                            temp[1], ref, en) >= 0)
                        return false;
                    return en.getNextRaw();
                case SPECIAL_LEX_LQ:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (SpecialLexical.compareTerm(temp[0], ref,
                            temp[1], ref, en) > 0)
                        return false;
                    return en.getNextRaw();
                case SPECIAL_LEX_GR:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (SpecialLexical.compareTerm(temp[0], ref,
                            temp[1], ref, en) <= 0)
                        return false;
                    return en.getNextRaw();
                case SPECIAL_LEX_GQ:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (SpecialLexical.compareTerm(temp[0], ref,
                            temp[1], ref, en) < 0)
                        return false;
                    return en.getNextRaw();
                case SPECIAL_COMPARE:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    Object witmolec = SpecialLexical.comparisonAtom(
                            SpecialLexical.compareTerm(temp[1], ref, temp[2], ref, en), en);
                    if (!en.unifyTerm(temp[0], ref, witmolec, BindCount.DISPLAY_CONST))
                        return false;
                    return en.getNext();
                case SPECIAL_LOCALE_COMPARE:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    Comparator cmp = EngineLexical.comparatorAtom(temp[0], ref);
                    witmolec = SpecialLexical.comparisonAtom(new EngineLexical(cmp, en)
                            .localeCompareTerm(temp[2], ref, temp[3], ref), en);
                    if (!en.unifyTerm(temp[1], ref, witmolec, BindCount.DISPLAY_CONST))
                        return false;
                    return en.getNext();
                default:
                    throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
            }
        } catch (ArithmeticException x) {
            throw new EngineMessage(EngineMessage.evaluationError(x.getMessage()));
        }
    }

    /**
     * <p>Compute the comparison atom from n integer.</p>
     *
     * @param res The comparison result.
     * @param en  The engine.
     * @return The comparison atom.
     */
    private static SkelAtom comparisonAtom(int res, Engine en) {
        if (res < 0) {
            return en.store.foyer.ATOM_LESS;
        } else if (res == 0) {
            return en.store.foyer.ATOM_EQUAL;
        } else {
            return en.store.foyer.ATOM_GREATER;
        }
    }

    /**********************************************************/
    /* Equality Test                                          */
    /**********************************************************/

    /**
     * <p>Determine the eq type class of a prolog term. The
     * prolog term should be already dereferenced.</P>
     *
     * @param a The prolog term.
     * @return The type.
     */
    private static int eqType(Object a) {
        if (a instanceof SkelVar) {
            return EQ_TYPE_VAR;
        } else if (a instanceof SkelCompound) {
            return EQ_TYPE_COMPOUND;
        } else {
            return EQ_TYPE_ATOMIC;
        }
    }

    /**
     * <p>Check two terms for lexical equivalence.</p>
     * <p>Teil recursive solution.</p>
     *
     * @param alfa The first term.
     * @param d1   The display of the first term.
     * @param beta The second term.
     * @param d2   The display of the second term.
     * @return True if they are lexically equal, otherwise false.
     */
    public static boolean equalTerm(Object alfa, BindCount[] d1,
                                    Object beta, BindCount[] d2) {
        for (; ; ) {
            BindVar b1;
            while (alfa instanceof SkelVar &&
                    (b1 = d1[((SkelVar) alfa).id]).display != null) {
                alfa = b1.skel;
                d1 = b1.display;
            }
            int k = eqType(alfa);
            while (beta instanceof SkelVar &&
                    (b1 = d2[((SkelVar) beta).id]).display != null) {
                beta = b1.skel;
                d2 = b1.display;
            }
            if (k != eqType(beta))
                return false;
            switch (k) {
                case EQ_TYPE_VAR:
                    return (alfa == beta && d1 == d2);
                case EQ_TYPE_ATOMIC:
                    return alfa.equals(beta);
                case EQ_TYPE_COMPOUND:
                    Object[] t1 = ((SkelCompound) alfa).args;
                    Object[] t2 = ((SkelCompound) beta).args;
                    if (t1.length != t2.length)
                        return false;
                    if (!((SkelCompound) alfa).sym.equals(((SkelCompound) beta).sym))
                        return false;
                    int i = 0;
                    for (; i < t1.length - 1; i++) {
                        if (!equalTerm(t1[i], d1, t2[i], d2))
                            return false;
                    }
                    alfa = t1[i];
                    beta = t2[i];
                    break;
                default:
                    throw new IllegalArgumentException("unknown type");
            }
        }
    }

    /**********************************************************/
    /* Comparison Function                                    */
    /**********************************************************/

    /**
     * <p>Compare two terms lexically.</p>
     * <p>As a side effect will dynamically allocate display serial numbers.</p>
     * <p>Teil recursive solution.</p>
     * <p>Throws a runtime exception for uncomparable references.</p>
     *
     * @param alfa The skeleton of the first term.
     * @param d1   The display of the first term.
     * @param beta The skeleton of the second term.
     * @param d2   The display of the second term.
     * @param en   The engine.
     * @return <0 alfa < beta, 0 alfa = beta, >0 alfa > beta
     */
    public static int compareTerm(Object alfa, BindCount[] d1,
                                  Object beta, BindCount[] d2, Engine en)
            throws ArithmeticException {
        for (; ; ) {
            BindCount b1;
            while (alfa instanceof SkelVar &&
                    (b1 = d1[((SkelVar) alfa).id]).display != null) {
                alfa = b1.skel;
                d1 = b1.display;
            }
            int i = EngineLexical.cmpType(alfa);
            while (beta instanceof SkelVar &&
                    (b1 = d2[((SkelVar) beta).id]).display != null) {
                beta = b1.skel;
                d2 = b1.display;
            }
            int k = i - EngineLexical.cmpType(beta);
            if (k != 0) return k;
            switch (i) {
                case EngineLexical.CMP_TYPE_VAR:
                    b1 = d1[((SkelVar) alfa).id];
                    i = b1.serno;
                    if (i == -1)
                        i = BindSerno.bindSerno(b1, en);
                    b1 = d2[((SkelVar) beta).id];
                    k = b1.serno;
                    if (k == -1)
                        k = BindSerno.bindSerno(b1, en);
                    return i - k;
                case EngineLexical.CMP_TYPE_DECIMAL:
                    return compareDecimalLexical(alfa, beta);
                case EngineLexical.CMP_TYPE_FLOAT:
                    return compareFloatLexical(alfa, beta);
                case EngineLexical.CMP_TYPE_INTEGER:
                    return SpecialCompare.compareIntegerArithmetical(alfa, beta);
                case EngineLexical.CMP_TYPE_REF:
                    if (alfa instanceof Comparable)
                        return ((Comparable) alfa).compareTo(beta);
                    throw new ArithmeticException(EngineMessage.OP_EVALUATION_ORDERED);
                case EngineLexical.CMP_TYPE_ATOM:
                    return ((SkelAtom) alfa).compareTo(((SkelAtom) beta));
                case EngineLexical.CMP_TYPE_COMPOUND:
                    Object[] t1 = ((SkelCompound) alfa).args;
                    Object[] t2 = ((SkelCompound) beta).args;
                    k = t1.length - t2.length;
                    if (k != 0) return k;
                    k = ((SkelCompound) alfa).sym.compareTo(((SkelCompound) beta).sym);
                    if (k != 0) return k;
                    i = 0;
                    for (; i < t1.length - 1; i++) {
                        k = compareTerm(t1[i], d1, t2[i], d2, en);
                        if (k != 0) return k;
                    }
                    alfa = t1[i];
                    beta = t2[i];
                    break;
                default:
                    throw new IllegalArgumentException("unknown type");
            }
        }
    }

    /**
     * <p>Compare two Prolog floats lexically.</p>
     *
     * @param alfa The first Prolog float.
     * @param beta The second Prolog float.
     * @return <0 alfa < beta, 0 alfa = beta, >0 alfa > beta
     */
    public static int compareFloatLexical(Object alfa, Object beta) {
        if (!(alfa instanceof Double)) {
            if (!(beta instanceof Double)) {
                return ((Float) alfa).compareTo((Float) beta);
            } else {
                return -1;
            }
        } else if (!(beta instanceof Double)) {
            return 1;
        } else {
            return ((Double) alfa).compareTo((Double) beta);
        }
    }

    /**
     * <p>Compare two Prolog decimals lexically.</p>
     *
     * @param alfa The first Prolog decimal.
     * @param beta The second Prolog decimal.
     * @return <0 alfa < beta, 0 alfa = beta, >0 alfa > beta
     */
    public static int compareDecimalLexical(Object alfa, Object beta) {
        if (alfa instanceof Long) {
            if (beta instanceof Long) {
                return ((Long) alfa).compareTo((Long) beta);
            } else {
                int k2 = -((BigDecimal) beta).scale();
                if (k2 != 0) return k2;
                BigInteger unscaled1 = BigInteger.valueOf(((Long) alfa).longValue());
                BigInteger unscaled2 = ((BigDecimal) beta).unscaledValue();
                return unscaled1.compareTo(unscaled2);
            }
        } else {
            if (beta instanceof Long) {
                int k2 = ((BigDecimal) alfa).scale();
                if (k2 != 0) return k2;
                BigInteger unscaled1 = ((BigDecimal) alfa).unscaledValue();
                BigInteger unscaled2 = BigInteger.valueOf(((Long) beta).longValue());
                return unscaled1.compareTo(unscaled2);
            } else {
                int scale1 = ((BigDecimal) alfa).scale();
                int scale2 = ((BigDecimal) beta).scale();
                int k2 = scale1 - scale2;
                if (k2 != 0) return k2;
                BigInteger unscaled1 = ((BigDecimal) alfa).unscaledValue();
                BigInteger unscaled2 = ((BigDecimal) beta).unscaledValue();
                return unscaled1.compareTo(unscaled2);
            }
        }
    }

}
