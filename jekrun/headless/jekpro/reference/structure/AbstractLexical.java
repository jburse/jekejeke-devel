package jekpro.reference.structure;

import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.WriteOpts;
import jekpro.reference.runtime.ForeignCollector;
import jekpro.tools.proxy.RuntimeWrap;
import jekpro.tools.term.AbstractTerm;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.regex.IgnoreCase;
import matula.util.wire.LangProperties;

import java.text.Collator;
import java.util.Comparator;
import java.util.Locale;

/**
 * <p>The base class for comparators.</p>
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
public abstract class AbstractLexical implements Comparator<Object> {
    private static final String OP_TYPE = "type";
    private static final String OP_TYPE_TREE = "tree";
    private static final String OP_TYPE_HASH = "hash";
    private static final String OP_TYPE_COLLATOR = "collator";
    private static final String OP_TYPE_CALLBACK = "callback";

    private static final String OP_IGNORE_CASE = "ignore_case";
    private static final String OP_REVERSE = "reverse";
    private static final String OP_EAGER = "eager";
    private static final String OP_LOCALE = "locale";

    private static final String OP_SHARE = "share";
    private static final String OP_SHARE_THREAD_LOCAL = "thread_local";
    private static final String OP_SHARE_DYNAMIC = "dynamic";
    private static final String OP_SHARE_GROUP_LOCAL = "group_local";

    private static final String OP_COMPARATOR = "comparator";

    public static final int MASK_FLAG_RVRS = 0x00000001;
    public static final int MASK_FLAG_EAGR = 0x00000002;
    public static final int MASK_FLAG_SHDY = 0x00000004;
    public static final int MASK_FLAG_SHGL = 0x00000008;

    private static final int TYPE_HASH = 1;
    private static final int TYPE_TREE = 2;
    private static final int TYPE_COLLATOR = 3;
    private static final int TYPE_CALLBACK = 4;

    private static final int SHARE_THREAD_LOCAL = 1;
    private static final int SHARE_DYNAMIC = 2;
    private static final int SHARE_GROUP_LOCAL = 3;

    protected Engine engine;
    protected int flags;

    /**
     * <p>Set the engine.</p>
     *
     * @param en The engine.
     */
    public void setEngine(Engine en) {
        engine = en;
    }

    /**
     * <p>Set the flags.</p>
     *
     * @param f The flags.
     */
    public void setFlags(int f) {
        flags = f;
    }

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The the flags.
     */
    public int getFlags() {
        return flags;
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
     * @return The sort options.
     * @throws EngineMessage Type Error.
     */
    public static AbstractLexical decodeSortOpts(Object t, Display d, Engine en)
            throws EngineMessage {
        Locale locale = en.store.foyer.locale;
        boolean ignore = false;
        int type = AbstractLexical.TYPE_TREE;
        int bits = 0;
        Object callback = en.store.foyer.ATOM_COMPARE;
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
                    ((SkelCompound) en.skel).sym.fun.equals(AbstractLexical.OP_TYPE)) {
                type = AbstractLexical.atomToType(((SkelCompound) en.skel).args[0], en.display);
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(AbstractLexical.OP_IGNORE_CASE)) {
                ignore = WriteOpts.atomToBool(((SkelCompound) en.skel).args[0], en.display);
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(AbstractLexical.OP_REVERSE)) {
                if (WriteOpts.atomToBool(((SkelCompound) en.skel).args[0], en.display)) {
                    bits |= MASK_FLAG_RVRS;
                } else {
                    bits &= ~MASK_FLAG_RVRS;
                }
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(AbstractLexical.OP_EAGER)) {
                if (WriteOpts.atomToBool(((SkelCompound) en.skel).args[0], en.display)) {
                    bits |= MASK_FLAG_EAGR;
                } else {
                    bits &= ~MASK_FLAG_EAGR;
                }
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(AbstractLexical.OP_LOCALE)) {
                locale = AbstractLexical.atomToLocale(((SkelCompound) en.skel).args[0], en.display);
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(AbstractLexical.OP_SHARE)) {
                int share = AbstractLexical.atomToShare(((SkelCompound) en.skel).args[0], en.display);
                switch (share) {
                    case SHARE_THREAD_LOCAL:
                        bits &= ~MASK_FLAG_SHDY;
                        bits &= ~MASK_FLAG_SHGL;
                        break;
                    case SHARE_DYNAMIC:
                        bits |= MASK_FLAG_SHDY;
                        bits &= ~MASK_FLAG_SHGL;
                        break;
                    case SHARE_GROUP_LOCAL:
                        bits &= ~MASK_FLAG_SHDY;
                        bits |= MASK_FLAG_SHGL;
                        break;
                    default:
                        throw new IllegalArgumentException("illegal share");
                }
            } else if (en.skel instanceof SkelCompound &&
                    ((SkelCompound) en.skel).args.length == 1 &&
                    ((SkelCompound) en.skel).sym.fun.equals(AbstractLexical.OP_COMPARATOR)) {
                Object[] mc2 = ((SkelCompound) en.skel).args;
                en.skel = mc2[0];
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                callback = AbstractTerm.createMolec(en.skel, en.display);
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
            case AbstractLexical.TYPE_HASH:
                AbstractLexical el = new LexicalCollator();
                el.setFlags(bits);
                return el;
            case AbstractLexical.TYPE_TREE:
                el = new LexicalCollator();
                el.setFlags(bits);
                ((LexicalCollator) el).setCmpStr(ignore ? IgnoreCase.DEFAULT : IgnoreCase.DEFAULT_TERTIARY);
                return el;
            case AbstractLexical.TYPE_COLLATOR:
                el = new LexicalCollator();
                el.setFlags(bits);
                Collator col = Collator.getInstance(locale);
                col.setStrength(ignore ? Collator.SECONDARY : Collator.TERTIARY);
                ((LexicalCollator) el).setCmpStr((Comparator) col);
                ((LexicalCollator) el).setLocStr(locale.toString());
                return el;
            case AbstractLexical.TYPE_CALLBACK:
                el = new LexicalCallback();
                el.setFlags(bits);
                ((LexicalCallback) el).setComparator(callback);
                return el;
            default:
                throw new IllegalArgumentException("illegal type");
        }
    }

    /**
     * !
     * <p>Encode the sort options.</p>
     * <p>The result is returned in skel and display.</p>
     *
     * @param el The sort options.
     * @param en The engine.
     */
    public static void encodeSortOpts(AbstractLexical el, Engine en) {
        en.skel = en.store.foyer.ATOM_NIL;
        en.display = Display.DISPLAY_CONST;
        if (el instanceof LexicalCollator) {
            Comparator cmpstr = ((LexicalCollator) el).getCmpStr();
            if (cmpstr == null) {
                Object t = en.skel;
                Display d = en.display;
                Object m = new SkelCompound(new SkelAtom(OP_TYPE),
                        new SkelAtom(OP_TYPE_HASH));
                ForeignCollector.pairValue(en.store.foyer.CELL_CONS,
                        m, Display.DISPLAY_CONST, t, d, en);
            } else if (cmpstr instanceof IgnoreCase) {
                if (((IgnoreCase) cmpstr).getStrength() != Collator.TERTIARY) {
                    Object t = en.skel;
                    Display d = en.display;
                    Object m = new SkelCompound(new SkelAtom(OP_IGNORE_CASE),
                            new SkelAtom(Foyer.OP_TRUE));
                    ForeignCollector.pairValue(en.store.foyer.CELL_CONS,
                            m, Display.DISPLAY_CONST, t, d, en);
                }
            } else {
                if (((Collator) cmpstr).getStrength() != Collator.TERTIARY) {
                    Object t = en.skel;
                    Display d = en.display;
                    Object m = new SkelCompound(new SkelAtom(OP_IGNORE_CASE),
                            new SkelAtom(Foyer.OP_TRUE));
                    ForeignCollector.pairValue(en.store.foyer.CELL_CONS,
                            m, Display.DISPLAY_CONST, t, d, en);
                }
                Object t = en.skel;
                Display d = en.display;
                Object m = new SkelCompound(new SkelAtom(OP_LOCALE),
                        new SkelAtom(((LexicalCollator) el).getLocStr()));
                ForeignCollector.pairValue(en.store.foyer.CELL_CONS,
                        m, Display.DISPLAY_CONST, t, d, en);
                t = en.skel;
                d = en.display;
                m = new SkelCompound(new SkelAtom(OP_TYPE),
                        new SkelAtom(OP_TYPE_COLLATOR));
                ForeignCollector.pairValue(en.store.foyer.CELL_CONS,
                        m, Display.DISPLAY_CONST, t, d, en);
            }
        } else {
            Object t = en.skel;
            Display d = en.display;
            Object obj = ((LexicalCallback) el).getComparator();
            Object m = new SkelCompound(new SkelAtom(OP_COMPARATOR),
                    AbstractTerm.getSkel(obj));
            ForeignCollector.pairValue(en.store.foyer.CELL_CONS,
                    m, AbstractTerm.getDisplay(obj), t, d, en);
            t = en.skel;
            d = en.display;
            m = new SkelCompound(new SkelAtom(OP_TYPE),
                    new SkelAtom(OP_TYPE_CALLBACK));
            ForeignCollector.pairValue(en.store.foyer.CELL_CONS,
                    m, Display.DISPLAY_CONST, t, d, en);
        }
    }

    /**
     * <p>Convert an atom to a sort type.</p>
     *
     * @param m The type skeleton.
     * @param d The type display.
     * @return The sort type.
     * @throws EngineMessage Domain Error.
     */
    private static int atomToType(Object m, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        if (fun.equals(AbstractLexical.OP_TYPE_HASH)) {
            return TYPE_HASH;
        } else if (fun.equals(AbstractLexical.OP_TYPE_TREE)) {
            return TYPE_TREE;
        } else if (fun.equals(AbstractLexical.OP_TYPE_COLLATOR)) {
            return TYPE_COLLATOR;
        } else if (fun.equals(AbstractLexical.OP_TYPE_CALLBACK)) {
            return TYPE_CALLBACK;
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

    /**
     * <p>Convert an atom to a share type.</p>
     *
     * @param m The type skeleton.
     * @param d The type display.
     * @return The share type.
     * @throws EngineMessage Domain Error.
     */
    private static int atomToShare(Object m, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        if (fun.equals(AbstractLexical.OP_SHARE_THREAD_LOCAL)) {
            return SHARE_THREAD_LOCAL;
        } else if (fun.equals(AbstractLexical.OP_SHARE_DYNAMIC)) {
            return SHARE_DYNAMIC;
        } else if (fun.equals(AbstractLexical.OP_SHARE_GROUP_LOCAL)) {
            return SHARE_GROUP_LOCAL;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_TYPE_OPTION, m), d);
        }
    }

    /**************************************************************/
    /* Compare API                                                */
    /**************************************************************/

    /**
     * <p>Compare two objects.</p>
     *
     * @param o1 The first object.
     * @param o2 The second object.
     * @return <0 o1 < o2, 0 o1 = o2, >0 o1 > o2
     * @throws ArithmeticException Incomparable reference.
     * @throws RuntimeWrap         Interpreter error.
     */
    public final int compare(Object o1, Object o2)
            throws ArithmeticException, RuntimeWrap {
        if (engine != null) {
            return compareTerm(AbstractTerm.getSkel(o1), AbstractTerm.getDisplay(o1),
                    AbstractTerm.getSkel(o2), AbstractTerm.getDisplay(o2));
        } else {
            return compareTermSkel(o1, o2);
        }
    }

    /**
     * <p>Compare two terms lexically.</p>
     * <p>As a side effect will dynamically allocate display serial numbers.</p>
     *
     * @param alfa The first skeleton.
     * @param d1   The first display.
     * @param beta The second skeleton.
     * @param d2   The second display.
     * @return <0 alfa < beta, 0 alfa = beta, >0 alfa > beta
     * @throws ArithmeticException Incomparable reference.
     * @throws RuntimeWrap         Interpreter error.
     */
    public abstract int compareTerm(Object alfa, Display d1,
                                    Object beta, Display d2)
            throws ArithmeticException, RuntimeWrap;

    /**
     * <p>Compare two skeletons lexically.</p>
     *
     * @param alfa The first skeleton.
     * @param beta The second skeleton.
     * @return <0 alfa < beta, 0 alfa = beta, >0 alfa > beta
     * @throws ArithmeticException Incomparable reference.
     * @throws RuntimeWrap         Interpreter error.
     */
    public abstract int compareTermSkel(Object alfa, Object beta)
            throws ArithmeticException, RuntimeWrap;

}