package jekpro.reference.structure;

import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.arithmetic.SpecialCompare;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

import java.util.Comparator;

/**
 * <p>Comparator based on string collator.</p>
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
public final class LexicalCollator extends AbstractLexical {
    private Comparator<String> cmpstr;
    private String locstr;

    /**
     * <p>Set the string comparator.</p>
     *
     * @param c The string comparator.
     */
    public void setCmpStr(Comparator<String> c) {
        cmpstr = c;
    }

    /**
     * <p>Retrieve the string comparator.</p>
     *
     * @return The string comparator.
     */
    public Comparator<String> getCmpStr() {
        return cmpstr;
    }

    /**
     * <p>Set the locale string.</p>
     *
     * @param l The locale string.
     */
    public void setLocStr(String l) {
        locstr = l;
    }

    /**
     * <p>Retrieve the locale string.</p>
     *
     * @return The locale string.
     */
    public String getLocStr() {
        return locstr;
    }

    /**************************************************************/
    /* Compare Term                                               */
    /**************************************************************/

    /**
     * <p>Compare two terms lexically with the string comparator.</p>
     * <p>As a side effect will dynamically allocate display serial numbers.</p>
     * <p>Teil recursive solution.</p>
     *
     * @param alfa The first skeleton.
     * @param d1   The first display.
     * @param beta The second skeleton.
     * @param d2   The second display.
     * @return <0 alfa < beta, 0 alfa = beta, >0 alfa > beta
     * @throws ArithmeticException Incomparable reference.
     */
    public int compareTerm(Object alfa, Display d1,
                           Object beta, Display d2)
            throws ArithmeticException {
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
            int i = SpecialLexical.cmpType(alfa);
            int k = i - SpecialLexical.cmpType(beta);
            if (k != 0) return k;
            switch (i) {
                case SpecialLexical.CMP_TYPE_VAR:
                    i = d1.bind[((SkelVar) alfa).id].getValue(engine);
                    k = d2.bind[((SkelVar) beta).id].getValue(engine);
                    return i - k;
                case SpecialLexical.CMP_TYPE_DECIMAL:
                    return SpecialLexical.compareDecimalLexical(alfa, beta);
                case SpecialLexical.CMP_TYPE_FLOAT:
                    return SpecialLexical.compareFloatLexical(alfa, beta);
                case SpecialLexical.CMP_TYPE_INTEGER:
                    return SpecialCompare.compareIntegerArithmetical(alfa, beta);
                case SpecialLexical.CMP_TYPE_REF:
                    if (alfa instanceof Comparable)
                        return ((Comparable) alfa).compareTo(beta);
                    throw new ArithmeticException(EngineMessage.OP_EVALUATION_ORDERED);
                case SpecialLexical.CMP_TYPE_ATOM:
                    return ((SkelAtom) alfa).compareTo(((SkelAtom) beta), cmpstr);
                case SpecialLexical.CMP_TYPE_COMPOUND:
                    Object[] t1 = ((SkelCompound) alfa).args;
                    Object[] t2 = ((SkelCompound) beta).args;
                    k = t1.length - t2.length;
                    if (k != 0) return k;
                    k = ((SkelCompound) alfa).sym.compareTo(((SkelCompound) beta).sym, cmpstr);
                    if (k != 0) return k;
                    i = 0;
                    for (; i < t1.length - 1; i++) {
                        k = compareTerm(t1[i], d1, t2[i], d2);
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

    /**************************************************************/
    /* Compare Skel                                               */
    /**************************************************************/

    /**
     * <p>Compare two skeletons lexically with the string comparator.</p>
     * <p>Teil recursive solution.</p>
     *
     * @param alfa The first skeleton.
     * @param beta The second skeleton.
     * @return <0 alfa < beta, 0 alfa = beta, >0 alfa > beta
     * @throws ArithmeticException Incomparable reference.
     */
    public int compareTermSkel(Object alfa, Object beta)
            throws ArithmeticException {
        for (; ; ) {
            int i = SpecialLexical.cmpType(alfa);
            int k = i - SpecialLexical.cmpType(beta);
            if (k != 0) return k;
            switch (i) {
                case SpecialLexical.CMP_TYPE_VAR:
                    return ((SkelVar) alfa).compareTo(((SkelVar) beta));
                case SpecialLexical.CMP_TYPE_DECIMAL:
                    return SpecialLexical.compareDecimalLexical(alfa, beta);
                case SpecialLexical.CMP_TYPE_FLOAT:
                    return SpecialLexical.compareFloatLexical(alfa, beta);
                case SpecialLexical.CMP_TYPE_INTEGER:
                    return SpecialCompare.compareIntegerArithmetical(alfa, beta);
                case SpecialLexical.CMP_TYPE_REF:
                    if (alfa instanceof Comparable)
                        return ((Comparable) alfa).compareTo(beta);
                    throw new ArithmeticException(EngineMessage.OP_EVALUATION_ORDERED);
                case SpecialLexical.CMP_TYPE_ATOM:
                    return ((SkelAtom) alfa).compareTo(((SkelAtom) beta), cmpstr);
                case SpecialLexical.CMP_TYPE_COMPOUND:
                    Object[] t1 = ((SkelCompound) alfa).args;
                    Object[] t2 = ((SkelCompound) beta).args;
                    k = t1.length - t2.length;
                    if (k != 0) return k;
                    k = ((SkelCompound) alfa).sym.compareTo(((SkelCompound) beta).sym, cmpstr);
                    if (k != 0) return k;
                    i = 0;
                    for (; i < t1.length - 1; i++) {
                        k = compareTermSkel(t1[i], t2[i]);
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

}
