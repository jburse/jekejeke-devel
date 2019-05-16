package jekpro.model.inter;

import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractLocator;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Store;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;
import matula.util.wire.AbstractLivestock;

import java.io.Reader;

/**
 * <p>The class provides a predicate.</p>
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
public final class Predicate {
    public final static String OP_MULTIFILE = "multifile";
    public final static String OP_VIRTUAL = "virtual";

    public final static int MASK_PRED_MULT = 0x00000001;
    public final static int MASK_PRED_NOTR = 0x00000002;
    public final static int MASK_PRED_VIRT = 0x00000004;
    public final static int MASK_PRED_AUTO = 0x00000008;

    public final static int MASK_PRED_NOEX = 0x00000010;
    public final static int MASK_PRED_NOMC = 0x00000020;
    public final static int MASK_PRED_BODY = 0x00000040;
    public final static int MASK_PRED_RULE = 0x00000080;

    public final static int MASK_PRED_VSPR = 0x00000100;
    public final static int MASK_PRED_VSPU = 0x00000200;
    public final static int MASK_PRED_NOBR = 0x00000400;

    /* combine masks */
    public final static int MASK_PRED_VISI = MASK_PRED_VSPR | MASK_PRED_VSPU;

    public final static String OP_QUESTION = "?";
    public final static String OP_HASH = "#";

    public static final int MASK_TRCK_AUTO = 0x00000001;
    public static final int MASK_TRCK_STYL = 0x00000002;
    public static final int MASK_TRCK_DISC = 0x00000004;
    public static final int MASK_TRCK_DYNA = 0x00000008;

    public static final int MASK_TRCK_OVRD = 0x00000010;
    public static final int MASK_TRCK_MULT = 0x00000020;
    public static final int MASK_TRCK_VSPR = 0x00000040;
    public static final int MASK_TRCK_VSPU = 0x00000080;

    public static final int MASK_TRCK_PRED = 0x00000100;
    public static final int MASK_TRCK_FUNC = 0x00000200;
    public static final int MASK_TRCK_TRLC = 0x00080400;
    public static final int MASK_TRCK_GRLC = 0x00080800;

    private final int arity;
    private final String fun;
    private int flags;
    public AbstractDelegate del;
    public final MapHashLink<AbstractSource, Integer> defs = new MapHashLink<AbstractSource, Integer>();
    public MapEntry<AbstractSource, Integer>[] cachedefs;
    public Object meta_predicate;
    public Object meta_function;
    public AbstractSource source;

    /**
     * <p>Create a predicate.</p>
     *
     * @param f The functor.
     * @param a The length.
     */
    public Predicate(String f, int a) {
        fun = f;
        arity = a;
    }

    /**
     * <p>Retrieve the home source.</p>
     *
     * @return The home source.
     */
    public AbstractSource getSource() {
        return source;
    }

    /**
     * <p>Set the home source.</p>
     *
     * @param s The home source.
     */
    public void setSource(AbstractSource s) {
        source = s;
    }

    /**
     * <p>Retrieve the functor.</p>
     *
     * @return The functor.
     */
    public final String getFun() {
        return fun;
    }

    /**
     * <p>Retrieve the length.</p>
     *
     * @return The length.
     */
    public final int getArity() {
        return arity;
    }

    /**
     * <p>Assure that the predicate is existent.</p>
     *
     * @param pick The predicate.
     * @param t    The skel.
     * @param d    The display.
     * @throws EngineMessage Shit happens.
     */
    public static void checkExistentPredicate(Predicate pick, Object t, Display d)
            throws EngineMessage {
        if (pick == null)
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_PROCEDURE, t), d);
    }

    /**
     * <p>Assure that the predicate is existent.</p>
     *
     * @param pick The predicate.
     * @param t    The skel.
     * @param d    The display.
     * @throws EngineMessage Shit happens.
     */
    public static void checkExistentProvable(Predicate pick, Object t, Display d)
            throws EngineMessage {
        if (pick == null)
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_PROVABLE, t), d);
    }

    /**
     * <p>Check a meta predicate declaration.</p>
     * <p>The following syntax is used:</p>
     * <pre>
     *     meta_signature    --> predicate_name
     *         [ "(" meta_specifier { "," meta_specifier } ")" ].
     * </pre>
     *
     * @param pred The predicate.
     * @param c    The declaration skeleton.
     * @param ref  The declaration display.
     * @param en   The engine.
     * @return The meta predicate declaration.
     * @throws EngineMessage Shit happens.
     */
    public static Object checkMetaSpez(Predicate pred,
                                       Object c, Display ref,
                                       Engine en)
            throws EngineMessage {
        en.skel = c;
        en.display = ref;
        en.deref();
        c = en.skel;
        ref = en.display;
        EngineMessage.checkCallable(c, ref);
        int arity = StackElement.callableToArity(c);
        if (arity != pred.getArity())
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_PROPERTY_VALUE, c), ref);
        if (arity != 0) {
            SkelCompound sc = (SkelCompound) c;
            Object[] args = new Object[arity];
            for (int i = 0; i < arity; i++)
                args[i] = checkMetaSpezArg(sc.args[i], ref, en);
            return new SkelCompound(StackElement.callableToName(c), args);
        } else {
            return StackElement.callableToName(c);
        }
    }

    /**
     * <p>Check a meta argument spezifier.</p>
     * <p>Returns a meta argument spezifier skeleton.</p>
     * <p>The following syntax is used:</p>
     * <pre>
     *     meta_specifier    --> integer
     *                         | "?"
     *                         | "::(" meta_specifier2 ")"
     *                         | "#(" meta_specifier3 ")".
     * </pre>
     *
     * @param c   The spezifier skeleton.
     * @param ref The spezifier display.
     * @param en  The engine.
     * @return The meta argument spezifier.
     * @throws EngineMessage Shit happens.
     */
    public static Object checkMetaSpezArg(Object c, Display ref,
                                          Engine en)
            throws EngineMessage {
        try {
            en.skel = c;
            en.display = ref;
            en.deref();
            c = en.skel;
            ref = en.display;
            if (c instanceof Number) {
                Number num = (Number) c;
                SpecialEval.castIntValue(num);
                return num;
            } else if (c instanceof SkelAtom &&
                    ((SkelAtom) c).fun.equals(OP_QUESTION)) {
                return c;
            } else if (c instanceof SkelCompound &&
                    ((SkelCompound) c).args.length == 1 &&
                    ((SkelCompound) c).sym.fun.equals(SpecialQuali.OP_COLONCOLON)) {
                return new SkelCompound(new SkelAtom(SpecialQuali.OP_COLONCOLON),
                        checkMetaSpezArg2(((SkelCompound) c).args[0],
                                ref, en));
            } else if (c instanceof SkelCompound &&
                    ((SkelCompound) c).args.length == 1 &&
                    ((SkelCompound) c).sym.fun.equals(OP_HASH)) {
                return new SkelCompound(new SkelAtom(OP_HASH),
                        checkMetaSpezArg3(((SkelCompound) c).args[0],
                                ref, en));
            } else {
                EngineMessage.checkInstantiated(c);
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_META_ARG,
                        c), ref);
            }
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

    /**
     * <p>Check a meta argument spezifier.</p>
     * <p>Returns a meta argument spezifier skeleton.</p>
     * <p>The following syntax is used:</p>
     * <pre>
     *     meta_specifier2   --> integer
     *                         | "::(" meta_specifier2 ")".
     * </pre>
     *
     * @param c   The spezifier skeleton.
     * @param ref The spezifier display.
     * @param en  The engine.
     * @return The meta argument spezifier.
     * @throws EngineMessage      Shit happens.
     * @throws ClassCastException Validation error.
     */
    private static Object checkMetaSpezArg2(Object c, Display ref,
                                            Engine en)
            throws EngineMessage, ClassCastException {
        en.skel = c;
        en.display = ref;
        en.deref();
        c = en.skel;
        ref = en.display;
        if (c instanceof Number) {
            Number num = (Number) c;
            SpecialEval.castIntValue(num);
            return num;
        } else if (c instanceof SkelCompound &&
                ((SkelCompound) c).args.length == 1 &&
                ((SkelCompound) c).sym.fun.equals(SpecialQuali.OP_COLONCOLON)) {
            return new SkelCompound(new SkelAtom(SpecialQuali.OP_COLONCOLON),
                    checkMetaSpezArg2(((SkelCompound) c).args[0],
                            ref, en));
        } else {
            EngineMessage.checkInstantiated(c);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_META_ARG,
                    c), ref);
        }
    }

    /**
     * <p>Check a meta argument spezifier.</p>
     * <p>Returns a meta argument spezifier skeleton.</p>
     * <p>The following syntax is used:</p>
     * <pre>
     *     meta_specifier3   --> integer.
     * </pre>
     *
     * @param c   The spezifier skeleton.
     * @param ref The spezifier display.
     * @param en  The engine.
     * @return The meta argument spezifier.
     * @throws EngineMessage      Shit happens.
     * @throws ClassCastException Validation error.
     */
    private static Object checkMetaSpezArg3(Object c, Display ref,
                                            Engine en)
            throws EngineMessage, ClassCastException {
        en.skel = c;
        en.display = ref;
        en.deref();
        c = en.skel;
        ref = en.display;
        if (c instanceof Number) {
            Number num = (Number) c;
            SpecialEval.castIntValue(num);
            return num;
        } else {
            EngineMessage.checkInstantiated(c);
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_META_ARG,
                    c), ref);
        }
    }

    /**************************************************************/
    /* Definiion Handling                                         */
    /**************************************************************/

    /**
     * <p>Add a source definition.</p>
     * <p>Can veto that a non-multifile predicate is extended.</p>
     * *
     *
     * @param s  The source definition.
     * @param f  The definition flags.
     * @param en The engine.
     * @return The diff flags.
     * @throws EngineMessage Shit happens.
     */
    public int addDef(AbstractSource s, int f, Engine en)
            throws EngineMessage {
        if (f == 0)
            throw new IllegalArgumentException("zero defs");
        int back;
        boolean ok;
        synchronized (this) {
            Integer im = defs.get(s);
            if (im == null) {
                back = f;
                im = Integer.valueOf(f);
                ok = addDefSafe(s, im);
            } else {
                int f1 = im.intValue();
                int f2 = f1 | f;
                back = f1 ^ f2;
                if (back == 0)
                    return 0;
                defs.remove(s);
                im = Integer.valueOf(f2);
                defs.add(s, im);
                ok = true;
            }
            cachedefs = null;
        }
        if (ok) {
            s.addPredInv(this, f);
            return back;
        }
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_REDEFINE,
                EngineMessage.OP_PERMISSION_PROCEDURE,
                SpecialQuali.indicatorToColonSkel(
                        getFun(), getSource().getStore().user,
                        getArity(), en)));
    }

    /**
     * <p>Add a source definition safely.</p>
     *
     * @param s  The source definition.
     * @param im The definition flags.
     * @return True if definition was added, otherwise false.
     */
    private boolean addDefSafe(AbstractSource s, Integer im) {
        if ((getBits() & Predicate.MASK_PRED_MULT) == 0 && countDefs() >= 1)
            return false;
        defs.add(s, im);
        return true;
    }

    /**
     * <p>Remove a source definition.</p>
     *
     * @param s The source definition.
     * @param f The definition flags.
     * @return The diff flags.
     */
    public int removeDef(AbstractSource s, int f) {
        if (f == 0)
            throw new IllegalArgumentException("zero deps");
        int back;
        synchronized (this) {
            Integer im = defs.get(s);
            if (im == null) {
                return 0;
            } else {
                int f1 = im.intValue();
                int f2 = f1 & ~f;
                back = f1 ^ f2;
                if (back == 0)
                    return 0;
                defs.remove(s);
                if (f2 != 0) {
                    im = Integer.valueOf(f2);
                    defs.add(s, im);
                }
            }
            cachedefs = null;
        }
        s.removePredInv(this, f);
        return back;
    }

    /**
     * <p>Retrieve the load deps.</p>
     *
     * @return The load deps.
     */
    public MapEntry<AbstractSource, Integer>[] snapshotDefs() {
        MapEntry<AbstractSource, Integer>[] res = cachedefs;
        if (res != null)
            return res;
        synchronized (this) {
            res = cachedefs;
            if (res != null)
                return res;
            res = new MapEntry[defs.size];
            defs.toArray(res);
            cachedefs = res;
        }
        return res;
    }

    /*******************************************************/
    /* Uses Set                                            */
    /*******************************************************/

    /**
     * <p>Add a source to a predicate.</p>
     * <p>Can veto that a non-multifile predicate is extended.</p>
     *
     * @param sa   The call-site, not null.
     * @param en   The engine.
     * @param copt The create flag.
     * @throws EngineMessage Shit happens.
     */
    public final void usagePredicate(SkelAtom sa,
                                     Engine en, int copt)
            throws EngineMessage {
        AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
        int flags = MASK_TRCK_AUTO;
        if ((src.getBits() & AbstractSource.MASK_SRC_VSPR) != 0)
            flags |= MASK_TRCK_VSPR;
        if ((src.getBits() & AbstractSource.MASK_SRC_VSPU) != 0)
            flags |= MASK_TRCK_VSPU;
        if ((copt & CachePredicate.MASK_CACH_NSTS) != 0) {
            if ((getBits() & Predicate.MASK_PRED_MULT) != 0)
                flags |= MASK_TRCK_MULT;
            if ((getBits() & Predicate.MASK_PRED_VSPR) != 0)
                flags |= MASK_TRCK_VSPR;
            if ((getBits() & Predicate.MASK_PRED_VSPU) != 0)
                flags |= MASK_TRCK_VSPU;
        }
        int back = addDef(src, flags, en);
        if (back == 0)
            return;
        if ((copt & CachePredicate.MASK_CACH_LOCA) != 0) {
            AbstractLocator locator = src.locator;
            if (locator != null)
                locator.addPosition(sa.getPosition(), this, AbstractLocator.MASK_LOC_INDI);
        }
        updateImport((getBits() & Predicate.MASK_PRED_VSPR) != 0);
    }

    /**
     * <p>Retrieve the usage for a source.</p>
     *
     * @param source The source.
     * @return The usage or null.
     */
    public final Integer getDef(AbstractSource source) {
        synchronized (this) {
            return defs.get(source);
        }
    }

    /**
     * <p>Clear a scope from a predicate.</p>
     *
     * @param scope The scope.
     * @throws EngineMessage Shit happends.
     */
    public void clearPredicate(AbstractSource scope)
            throws EngineMessage {
        removeDef(scope, -1);
        if (getDefOther(scope) != null) {
            AbstractDelegate fun = del;
            if (fun != null)
                fun.shrinkPredicate(this, scope);
        } else {
            getSource().removeRoutine(getArity(), getFun());
            AbstractDelegate fun = del;
            if (fun != null)
                fun.releasePredicate(this);
        }
    }

    /**
     * <p>Check whether there is another source.</p>
     *
     * @param src The source.
     * @return True if there is another source, otherwise false.
     */
    private AbstractSource getDefOther(AbstractSource src) {
        synchronized (this) {
            for (MapEntry<AbstractSource, Integer> entry = defs.getFirstEntry();
                 entry != null; entry = defs.successor(entry)) {
                if (!src.equals(entry.key))
                    return entry.key;
            }
        }
        return null;
    }

    /**
     * <p>Count the uses.</p>
     *
     * @return Return the number of uses.
     */
    private int countDefs() {
        return defs.size();
    }

    /*******************************************************/
    /* Flags                                               */
    /*******************************************************/

    /**
     * <p>Retrieve the flags.</p>
     *
     * @return The flags.
     */
    public final int getBits() {
        return flags;
    }

    /**
     * <p>Set a flag.</p>
     *
     * @param mask The flag mask.
     */
    public final void setBit(int mask) {
        synchronized (this) {
            flags |= mask;
        }
    }

    /**
     * <p>Reset a flag.</p>
     *
     * @param mask The flag mask.
     */
    public final void resetBit(int mask) {
        synchronized (this) {
            flags &= ~mask;
        }
    }

    /**
     * <p>Define a neutral predicate.</p>
     *
     * @param priv The private flag.
     */
    public void updateImport(boolean priv) {
        if (!CacheFunctor.isQuali(getFun())) {
            Store store = getSource().getStore();
            store.foyer.notifyImportvers(store);
        } else {
            AbstractSource source = getSource();
            int f = AbstractSource.MASK_IMPT_MODL;
            f |= (!priv ? AbstractSource.MASK_IMPT_REEX : 0);
            CachePredicate.notifyImportvers(source, f);
        }
    }

    /*************************************************************/
    /* Convenience Methods                                       */
    /*************************************************************/

    /**
     * <p>Get predicate by indicator and possibly create it.</p>
     *
     * @param t    The indicator skel.
     * @param d    The indicator display.
     * @param en   The engine.
     * @param copt The create flag.
     * @return The predicate, or null.
     * @throws EngineMessage Shit happens.
     * @throws EngineMessage Shit happens.
     */
    public static Predicate indicatorToPredicateDefined(Object t, Display d,
                                                        Engine en, int copt)
            throws EngineMessage, EngineException {
        Integer arity = SpecialQuali.colonToIndicator(t, d, en);
        SkelAtom sa = (SkelAtom) en.skel;
        CachePredicate cp = CachePredicate.getPredicateDefined(sa,
                arity.intValue(), en, copt);
        en.skel = sa;
        if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0)
            return null;
        return cp.pick;
    }

    /***********************************************************/
    /* Style Checks Predicate Declaration                      */
    /***********************************************************/

    /**
     * <p>Define a built-in for a predicate.</p>
     *
     * @param pick The predicate.
     * @param del  The delegate.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void definePredicate(Predicate pick,
                                       AbstractDelegate del,
                                       Engine en)
            throws EngineMessage {
        /* check virtual flag */
        boolean f1 = ((pick.getBits() & MASK_PRED_VIRT) != 0);
        boolean f2 = ((del.subflags & AbstractDelegate.MASK_DELE_VIRT) != 0);
        if (f1 != f2) {
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_COERCE,
                    EngineMessage.OP_PERMISSION_VIRTUAL,
                    SpecialQuali.indicatorToColonSkel(
                            pick.getFun(), pick.getSource().getStore().user,
                            pick.getArity(), en)));
        }
        /* create the builtin */
        AbstractDelegate fun = AbstractDelegate.promoteBuiltin(pick, del);
        if (del.equals(fun))
            return;
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_COERCE,
                EngineMessage.OP_PERMISSION_PROCEDURE,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /**
     * <p>Perform a style check on the given predicate.</p>
     * <p>This check is performed during the loading of a module.</p>
     *
     * @param pick The predicate.
     * @param sa   The call-site.
     * @param en   The engine.
     * @throws EngineMessage Printing error.
     */
    public static void checkPredicateDecl(Predicate pick, SkelAtom sa,
                                          Engine en)
            throws EngineMessage, EngineException {
        AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
        Integer def = pick.getDef(src);
        if (def == null)
            return;
        try {
            checkPredicateDiscontiguous(def, pick, en);
            if ((def.intValue() & MASK_TRCK_STYL) != 0)
                return;
            pick.addDef(src, MASK_TRCK_STYL, en);
            checkPredicateOverride(def, sa, pick, en);
            checkPredicateMultifile(def, pick, en);
            if (sa.scope != null &&
                    (sa.scope.getBits() & AbstractSource.MASK_SRC_VSPU) == 0)
                checkPredicatePublic(def, pick, en);
            checkPredicateMetaPredicate(def, pick, en);
            checkPredicateMetaFunction(def, pick, en);
            checkPredicateDynamic(def, pick, en);
            checkPredicateThreadLocal(def, pick, en);
            checkPredicateGroupLocal(def, pick, en);
        } catch (EngineMessage x) {
            EngineException y = new EngineException(x,
                    EngineException.fetchLoc(EngineException.fetchStack(en),
                            sa.getPosition(), en),
                    EngineException.OP_WARNING);
            y.printStackTrace(en);
        }
    }

    /**
     * <p>Perform the discontigous style check.</p>
     *
     * @param loc  The usage.
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateDiscontiguous(Integer loc,
                                                    Predicate pick,
                                                    Engine en)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_STYL) == 0) {
            en.visor.lastsk = new StoreKey(pick.getFun(), pick.getArity());
            return;
        }
        if ((loc.intValue() & MASK_TRCK_DISC) != 0)
            return;
        if (en.visor.lastsk != null &&
                pick.getArity() == en.visor.lastsk.getArity() &&
                pick.getFun() == en.visor.lastsk.getFun())
            return;
        en.visor.lastsk = new StoreKey(pick.getFun(), pick.getArity());
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_DISCONTIGUOUS_PRED,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /**
     * <p>Perform the override style check.</p>
     *
     * @param loc  The usage.
     * @param sa   The functor.
     * @param pick The redicate.
     * @param en   The engine.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateOverride(Integer loc, SkelAtom sa,
                                               Predicate pick,
                                               Engine en)
            throws EngineMessage, EngineException {
        if ((loc.intValue() & MASK_TRCK_OVRD) != 0)
            return;
        AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
        AbstractSource base = CachePredicate.performBase(sa, src, en);
        Predicate over;
        try {
            over = CachePredicate.performOverrides(sa, pick.getArity(), base);
        } catch (InterruptedException x) {
            throw (EngineMessage) AbstractLivestock.sysThreadClear();
        }
        if (over == null || !CachePredicate.visiblePred(over, src))
            return;
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_OVERRIDE_PRED,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /**
     * <p>Perform the multifile style check.</p>
     *
     * @param loc  The location.
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateMultifile(Integer loc,
                                                Predicate pick,
                                                Engine en)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_MULT) != 0)
            return;
        if ((pick.getBits() & Predicate.MASK_PRED_MULT) == 0)
            return;
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_MULTIFILE_PRED,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /**
     * <p>Perform the meta predicate style check.</p>
     *
     * @param loc  The location.
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicatePublic(Integer loc,
                                             Predicate pick,
                                             Engine en)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_VSPU) != 0)
            return;
        if ((pick.getBits() & Predicate.MASK_PRED_VSPU) == 0)
            return;
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_PUBLIC_PRED,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /**
     * <p>Perform the meta predicate style check.</p>
     *
     * @param loc  The location.
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateMetaPredicate(Integer loc,
                                                    Predicate pick,
                                                    Engine en)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_PRED) != 0)
            return;
        if (pick.meta_predicate == null)
            return;
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_META_PREDICATE_PRED,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /**
     * <p>Perform the meta function style check.</p>
     *
     * @param loc  The location.
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateMetaFunction(Integer loc,
                                                   Predicate pick,
                                                   Engine en)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_FUNC) != 0)
            return;
        if (pick.meta_function == null)
            return;
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_META_FUNCTION_PRED,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /**
     * <p>Perform the dynamic style check.</p>
     *
     * @param loc  The location.
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateDynamic(Integer loc,
                                              Predicate pick,
                                              Engine en)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_DYNA) != 0)
            return;
        AbstractDelegate fun = pick.del;
        if (fun == null || (fun.subflags & AbstractDefined.MASK_DEFI_DYNA) == 0)
            return;
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_DYNAMIC_PRED,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /**
     * <p>Perform the thread local style check.</p>
     *
     * @param loc  The location.
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateThreadLocal(Integer loc,
                                                  Predicate pick,
                                                  Engine en)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_TRLC) != 0)
            return;
        AbstractDelegate fun = pick.del;
        if (fun == null || (fun.subflags & AbstractDefined.MASK_DEFI_THLC) == 0)
            return;
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_THREAD_LOCAL_PRED,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /**
     * <p>Perform the group local style check.</p>
     *
     * @param loc  The location.
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateGroupLocal(Integer loc,
                                                 Predicate pick,
                                                 Engine en)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_GRLC) != 0)
            return;
        AbstractDelegate fun = pick.del;
        if (fun == null || (fun.subflags & AbstractDefined.MASK_DEFI_GRLC) == 0)
            return;
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_THREAD_LOCAL_PRED,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /***********************************************************/
    /* Style Checks Predicate Initialization                   */
    /***********************************************************/

    /**
     * <p>Perform a style check on the given predicate.</p>
     * <p>This check is performed at the end of loading a module.</p>
     *
     * @param lr   The reader.
     * @param pick The predicate.
     * @param src  The load site.
     * @param en   The engine.
     * @throws EngineMessage   Printing error.
     * @throws EngineException Printing error.
     */
    public static void checkPredicateInit(Reader lr,
                                          Predicate pick,
                                          AbstractSource src,
                                          Engine en)
            throws EngineMessage, EngineException {
        Integer def = pick.getDef(src);
        if (def == null)
            return;
        try {
            checkPredicateImplemented(def, pick, en);
        } catch (EngineMessage x) {
            PositionKey pos = PositionKey.createPos(lr);
            EngineException y = new EngineException(x,
                    EngineException.fetchLoc(EngineException.fetchStack(en),
                            pos, en),
                    EngineException.OP_WARNING);
            y.printStackTrace(en);
        }
    }

    /**
     * <p>Perform the implemented check for a static predicate.</p>
     *
     * @param loc  The location.
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateImplemented(Integer loc,
                                                  Predicate pick,
                                                  Engine en)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_STYL) != 0)
            return;
        AbstractDelegate fun = pick.del;
        if (fun != null && (fun.subflags & AbstractDefined.MASK_DEFI_ASSE) != 0)
            return;
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_IMPLEMENTATION_PRED,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

}