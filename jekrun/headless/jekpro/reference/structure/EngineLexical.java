package jekpro.reference.structure;

import jekpro.model.inter.Engine;
import jekpro.model.molec.BindUniv;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.WriteOpts;
import jekpro.reference.arithmetic.SpecialCompare;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.regex.IgnoreCase;
import matula.util.wire.LangProperties;

import java.text.Collator;
import java.util.Collections;
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
    private static final String OP_TYPE = "type";
    private static final String OP_TYPE_TREE = "tree";
    private static final String OP_TYPE_HASH = "hash";
    private static final String OP_TYPE_COLLATOR = "collator";
    private static final String OP_IGNORE_CASE = "ignore_case";
    private static final String OP_REVERSE = "reverse";
    private static final String OP_LOCALE = "locale";

    private static final int TYPE_HASH = 1;
    private static final int TYPE_TREE = 2;
    private static final int TYPE_COLLATOR = 3;

    private Comparator<String> cmpstr;
    private Engine engine;
    private boolean reverse;

    /**
     * <p>Retrieve the comparator.</p>
     *
     * @return The comparator.
     */
    public Comparator<String> getComparator() {
        return cmpstr;
    }

    /**
     * <p>Set the engine.</p>
     *
     * @param en The engine.
     */
    public void setEngine(Engine en) {
        engine = en;
    }

    /**
     * <p>Retrieve the reverse flag.</p>
     *
     * @return The reverse flag.
     */
    public boolean getReverse() {
        return reverse;
    }

    /**
     * <p>Compare two objects.</p>
     *
     * @param o1 The first object.
     * @param o2 The second object.
     * @return <0 o1 < o2, 0 o1 = o2, >0 o1 > o2
     * @throws ArithmeticException Incomparable reference.
     */
    public final int compare(Object o1, Object o2)
            throws ArithmeticException {
        if (engine != null) {
            return compareTerm(AbstractTerm.getSkel(o1), AbstractTerm.getDisplay(o1),
                    AbstractTerm.getSkel(o2), AbstractTerm.getDisplay(o2));
        } else {
            return compareTermSkel(o1, o2);
        }
    }

    /**************************************************************/
    /* Compare Term                                               */
    /**************************************************************/

    /**
     * <p>Compare two terms lexically with the atom comparator.</p>
     * <p>As a side effect will dynamically allocate display serial numbers.</p>
     * <p>Teil recursive solution.</p>
     *
     * @param alfa The skeleton of the first term.
     * @param d1   The display of the first term.
     * @param beta The skeleton of the second term.
     * @param d2   The display of the second term.
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
     * <p>Compare two terms lexically with the atom comparator.</p>
     * <p>Teil recursive solution.</p>
     *
     * @param alfa The skeleton of the first term.
     * @param beta The skeleton of the second term.
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

    /*************************************************************/
    /* Sort Options                                              */
    /*************************************************************/

    /**
     * <p>Decode the sort options.</p>
     *
     * @param t  The option skeleton.
     * @param d  The option display.
     * @param en The engine.
     * @throws EngineMessage Type Error.
     */
    public void decodeSortOpts(Object t, Display d, Engine en)
            throws EngineMessage {
        Locale locale = en.store.foyer.locale;
        boolean ignore = false;
        reverse = false;
        int type = EngineLexical.TYPE_TREE;
        en.skel = t;
        en.display = d;
        en.deref();
        while (en.skel instanceof SkelCompound &&
                ((SkelCompound) en.skel).args.length == 2 &&
                ((SkelCompound) en.skel).sym.fun.equals(Foyer.OP_CONS)) {
            Object[] mc = ((SkelCompound) en.skel).args;
            d = en.display;
            en.skel = mc[0];
            en.deref();
            if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(EngineLexical.OP_TYPE)) {
                type = EngineLexical.atomToType(((SkelCompound) en.skel).args[0], en.display);
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(EngineLexical.OP_IGNORE_CASE)) {
                ignore = WriteOpts.atomToBool(((SkelCompound) en.skel).args[0], en.display);
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(EngineLexical.OP_REVERSE)) {
                reverse = WriteOpts.atomToBool(((SkelCompound) en.skel).args[0], en.display);
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(EngineLexical.OP_LOCALE)) {
                locale = EngineLexical.atomToLocale(((SkelCompound) en.skel).args[0], en.display);
            } else {
                EngineMessage.checkInstantiated(en.skel);
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_SORT_OPTION,
                        en.skel), en.display);
            }
            en.skel = mc[1];
            en.display = d;
            en.deref();
        }
        if (en.skel instanceof SkelAtom &&
                ((SkelAtom) en.skel).fun.equals(Foyer.OP_NIL)) {
            /* */
        } else {
            EngineMessage.checkInstantiated(en.skel);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_LIST,
                    en.skel), en.display);
        }
        switch (type) {
            case EngineLexical.TYPE_HASH:
                cmpstr = null;
                break;
            case EngineLexical.TYPE_TREE:
                cmpstr = (ignore ? IgnoreCase.DEFAULT : IgnoreCase.DEFAULT_TERTIARY);
                break;
            case EngineLexical.TYPE_COLLATOR:
                Collator col = Collator.getInstance(locale);
                col.setStrength(ignore ? Collator.SECONDARY : Collator.TERTIARY);
                cmpstr = (Comparator)col;
                break;
            default:
                throw new IllegalArgumentException("illegal type");
        }
    }

    /**
     * <p>Convert an atom to a type.</p>
     *
     * @param m The type skeleton.
     * @param d The type display.
     * @return The type.
     * @throws EngineMessage Domain Error.
     */
    private static int atomToType(Object m, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        if (fun.equals(EngineLexical.OP_TYPE_HASH)) {
            return TYPE_HASH;
        } else if (fun.equals(EngineLexical.OP_TYPE_TREE)) {
            return TYPE_TREE;
        } else if (fun.equals(EngineLexical.OP_TYPE_COLLATOR)) {
            return TYPE_COLLATOR;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_TYPE_OPTION, m), d);
        }
    }

    /**
     * <p>Convert an atom to a collator.</p>
     *
     * @param m The collator skeleton.
     * @param d The collator display.
     * @return The collator.
     * @throws EngineMessage Domain Error.
     */
    private static Locale atomToLocale(Object m, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        return LangProperties.stringToLocale(fun);
    }

}
