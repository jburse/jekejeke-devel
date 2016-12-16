package jekpro.reference.structure;

import jekpro.frequent.system.ForeignLocale;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

import java.text.Collator;
import java.util.Comparator;
import java.util.Locale;

/**
 * <p>Collator based comparator.</p>
 *
 * @author Copyright 2016, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 1.1.7 (a fast and small prolog interpreter)
 */
public final class EngineLexical implements Comparator<Object> {
    private final Engine engine;
    private final Collator collator;

    /**
     * <p>Create a engine lexical.</p>
     *
     * @param t  The locale skeleton.
     * @param d  The locale display.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public EngineLexical(Object t, Display d, Engine en)
            throws EngineMessage {
        engine = en;
        collator = collatorAtom(t, d, en);
    }

    /**
     * <p>Implementation of the comparator interface.</p>
     *
     * @param m1 The first molec.
     * @param m2 The second molec.
     * @return The comparison result.
     */
    public int compare(Object m1, Object m2) throws ArithmeticException {
        return localeCompareTerm(AbstractTerm.getSkel(m1), AbstractTerm.getDisplay(m1),
                AbstractTerm.getSkel(m2), AbstractTerm.getDisplay(m2));
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
            BindVar b1;
            while (alfa instanceof SkelVar &&
                    (b1 = d1.bind[((SkelVar) alfa).id]).display != null) {
                alfa = b1.skel;
                d1 = b1.display;
            }
            int i = SpecialLexical.cmpType(alfa);
            while (beta instanceof SkelVar &&
                    (b1 = d2.bind[((SkelVar) beta).id]).display != null) {
                beta = b1.skel;
                d2 = b1.display;
            }
            int k = i - SpecialLexical.cmpType(beta);
            if (k != 0) return k;
            switch (i) {
                case SpecialLexical.CMP_TYPE_VAR:
                    if (d1.serno == -1)
                        BindSerno.bindSerno(d1, engine);
                    if (d2.serno == -1)
                        BindSerno.bindSerno(d2, engine);
                    k = d1.serno - d2.serno;
                    if (k != 0) return k;
                    return ((SkelVar) alfa).compareTo((SkelVar) beta);
                case SpecialLexical.CMP_TYPE_DECIMAL:
                    return SpecialLexical.compareDecimal(alfa, beta);
                case SpecialLexical.CMP_TYPE_FLOAT:
                    return SpecialLexical.compareFloat(alfa, beta);
                case SpecialLexical.CMP_TYPE_INTEGER:
                    return SpecialLexical.compareInteger(alfa, beta);
                case SpecialLexical.CMP_TYPE_REF:
                    if (alfa instanceof Comparable)
                        return ((Comparable) alfa).compareTo(beta);
                    throw new ArithmeticException(EngineMessage.OP_EVALUATION_ORDERED);
                case SpecialLexical.CMP_TYPE_ATOM:
                    return ((SkelAtom) alfa).compareTo(((SkelAtom) beta), collator);
                case SpecialLexical.CMP_TYPE_COMPOUND:
                    Object[] t1 = ((SkelCompound) alfa).args;
                    Object[] t2 = ((SkelCompound) beta).args;
                    k = t1.length - t2.length;
                    if (k != 0) return k;
                    k = ((SkelCompound) alfa).sym.compareTo(((SkelCompound) beta).sym, collator);
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
     * @param t  The skeleton.
     * @param d  The display.
     * @param en The engine.
     * @return The collator.
     * @throws EngineMessage Shit happens.
     */
    private static Collator collatorAtom(Object t, Display d, Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        EngineMessage.checkInstantiated(en.skel);
        String fun = EngineMessage.castString(en.skel, en.display);
        Locale loc = ForeignLocale.stringToLocale(fun);
        return Collator.getInstance(loc);
    }

}
