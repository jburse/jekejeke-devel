package jekpro.reference.structure;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.array.Types;
import jekpro.tools.call.InterpreterException;
import jekpro.tools.proxy.RuntimeWrap;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

import java.math.BigDecimal;
import java.math.BigInteger;

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
    private final static int SPECIAL_COMPARE_OPT = 7;

    /* the syntactic equality categories */
    public final static int EQ_TYPE_VAR = 0;
    public final static int EQ_TYPE_ATOMIC = 1;
    public final static int EQ_TYPE_COMPOUND = 2;

    /* the lexical compare categories */
    public final static int CMP_TYPE_VAR = 0;
    public final static int CMP_TYPE_DECIMAL = 1;
    public final static int CMP_TYPE_FLOAT = 2;
    public final static int CMP_TYPE_INTEGER = 3;
    public final static int CMP_TYPE_REF = 4;
    public final static int CMP_TYPE_ATOM = 5;
    public final static int CMP_TYPE_COMPOUND = 6;

    /**
     * <p>Create an compare special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialLexical(int i) {
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
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        try {
            switch (id) {
                case SPECIAL_LEX_EQ:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    if (!equalTerm(temp[0], ref, temp[1], ref))
                        return false;
                    return true;
                case SPECIAL_LEX_NQ:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (equalTerm(temp[0], ref, temp[1], ref))
                        return false;
                    return true;
                case SPECIAL_LEX_LS:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (en.compareTerm(temp[0], ref, temp[1], ref) >= 0)
                        return false;
                    return true;
                case SPECIAL_LEX_LQ:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (en.compareTerm(temp[0], ref, temp[1], ref) > 0)
                        return false;
                    return true;
                case SPECIAL_LEX_GR:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (en.compareTerm(temp[0], ref, temp[1], ref) <= 0)
                        return false;
                    return true;
                case SPECIAL_LEX_GQ:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (en.compareTerm(temp[0], ref, temp[1], ref) < 0)
                        return false;
                    return true;
                case SPECIAL_COMPARE:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    int res = en.compareTerm(temp[1], ref, temp[2], ref);
                    if (!en.unifyTerm(SpecialLexical.compAtom(res, en), Display.DISPLAY_CONST, temp[0], ref))
                        return false;
                    return true;
                case SPECIAL_COMPARE_OPT:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    AbstractLexical el = AbstractLexical.decodeSortOpts(temp[3], ref, en);
                    if (el instanceof LexicalCollator &&
                            ((LexicalCollator) el).getCmpStr() == null) {
                        res = en.compareTerm(temp[1], ref, temp[2], ref);
                    } else {
                        el.setEngine(en);
                        res = el.compareTerm(temp[1], ref, temp[2], ref);
                    }
                    if ((el.getFlags() & LexicalCollator.MASK_FLAG_RVRS) != 0)
                        res = -res;
                    if (!en.unifyTerm(SpecialLexical.compAtom(res, en), Display.DISPLAY_CONST, temp[0], ref))
                        return false;
                    return true;
                default:
                    throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
            }
        } catch (RuntimeWrap x) {
            Throwable y = x.getCause();
            if (y instanceof InterpreterException) {
                throw (EngineException) ((InterpreterException) y).getException();
            } else {
                throw Types.mapThrowable(y);
            }
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
        }
    }

    /**
     * <p>Compute the comparison atom from an integer.</p>
     *
     * @param res The comparison result.
     * @param en  The engine.
     * @return The comparison atom.
     */
    public static SkelAtom compAtom(int res, Engine en) {
        if (res < 0) {
            return en.store.foyer.ATOM_LESS;
        } else if (res == 0) {
            return en.store.foyer.ATOM_EQUAL;
        } else {
            return en.store.foyer.ATOM_GREATER;
        }
    }

    /**********************************************************/
    /* Term Equality                                          */
    /**********************************************************/

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
    public static boolean equalTerm(Object alfa, Display d1,
                                    Object beta, Display d2) {
        for (; ; ) {
            BindUniv b1;
            while (alfa instanceof SkelVar &&
                    (b1 = d1.bind[((SkelVar) alfa).id]).display != null) {
                alfa = b1.skel;
                d1 = b1.display;
            }
            while (beta instanceof SkelVar &&
                    (b1 = d2.bind[((SkelVar) beta).id]).display != null) {
                beta = b1.skel;
                d2 = b1.display;
            }
            int k = eqType(alfa);
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
                        if (!SpecialLexical.equalTerm(t1[i], d1, t2[i], d2))
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

    /**********************************************************/
    /* Term Comparison                                        */
    /**********************************************************/

    /**
     * <p>Determine the compare type class of a prolog term. The
     * prolog term should be already dereferenced.</P>
     *
     * @param a The prolog term.
     * @return The type.
     */
    public static int cmpType(Object a) {
        if (a instanceof SkelVar) {
            return CMP_TYPE_VAR;
        } else if (a instanceof SkelCompound) {
            return CMP_TYPE_COMPOUND;
        } else if (a instanceof SkelAtom) {
            return CMP_TYPE_ATOM;
        } else if (a instanceof Integer || a instanceof BigInteger) {
            return CMP_TYPE_INTEGER;
        } else if (a instanceof Float || a instanceof Double) {
            return CMP_TYPE_FLOAT;
        } else if (a instanceof Long || a instanceof BigDecimal) {
            return CMP_TYPE_DECIMAL;
        } else {
            return CMP_TYPE_REF;
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
        } else {
            if (!(beta instanceof Double)) {
                return 1;
            } else {
                return ((Double) alfa).compareTo((Double) beta);
            }
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
