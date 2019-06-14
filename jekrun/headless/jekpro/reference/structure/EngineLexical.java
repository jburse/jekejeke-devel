package jekpro.reference.structure;

import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.arithmetic.SpecialCompare;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.regex.IgnoreCase;
import matula.util.wire.LangProperties;

import java.text.Collator;
import java.util.Comparator;
import java.util.Locale;

/**
 * <p>Collator based comparator.</p>
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
public final class EngineLexical implements Comparator<Object> {
    private final Comparator<String> cmpstr;
    private final Engine engine;

    /**
     * <p>Create a engine lexical.</p>
     *
     * @param c The comparator.
     */
    private EngineLexical(Comparator<String> c, Engine en) {
        if (c == null)
            throw new NullPointerException("comparator missing");
        cmpstr = c;
        engine = en;
    }

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
     * @return <0 alfa < beta, 0 alfa = beta, >0 alfa > beta
     */
    public int localeCompareTerm(Object alfa, Display d1,
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
                        k = localeCompareTerm(t1[i], d1, t2[i], d2);
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
     * <p>Compute the collator from an atom.</p>
     *
     * @param m  The skeleton.
     * @param d  The display.
     * @param en The engine.
     * @return The collator.
     * @throws EngineMessage Shit happens.
     */
    public static EngineLexical comparatorAtom(Object m, Display d, Engine en)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        Comparator<String> cmpstr;
        if ("IGNORE_CASE".equals(fun)) {
            cmpstr = IgnoreCase.DEFAULT;
        } else {
            Locale loc = LangProperties.stringToLocale(fun);
            cmpstr = (Comparator) Collator.getInstance(loc);
        }
        return new EngineLexical(cmpstr, en);
    }

    /**
     * <p>Compare two objects.</p>
     *
     * @param o1 The first object.
     * @param o2 The second object.
     * @return <0 o1 < o2, 0 o1 = o2, >0 o1 > o2
     */
    public final int compare(Object o1, Object o2)
            throws ArithmeticException {
        return localeCompareTerm(AbstractTerm.getSkel(o1), AbstractTerm.getDisplay(o1),
                AbstractTerm.getSkel(o2), AbstractTerm.getDisplay(o2));
    }

}
