package jekpro.model.inter;

import jekpro.frequent.system.ForeignThread;
import jekpro.model.builtin.Branch;
import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.*;
import jekpro.reference.reflect.SpecialPred;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.array.AbstractLense;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;
import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;

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
    public final static int MASK_PRED_MULT = 0x00000001;
    public final static int MASK_PRED_NOTR = 0x00000002;
    public final static int MASK_PRED_VIRT = 0x00000004;
    public final static int MASK_PRED_NOST = 0x00000008;

    public final static int MASK_PRED_VSPR = 0x00000010;
    public final static int MASK_PRED_VSPU = 0x00000020;

    public final static int MASK_PRED_NOEX = 0x00000100;
    public final static int MASK_PRED_AUTO = 0x00000200;
    public final static int MASK_PRED_TABL = 0x00000400;
    public final static int MASK_PRED_UTBL = 0x00000800;

    /* combine masks */
    public final static int MASK_PRED_VISI = MASK_PRED_VSPR | MASK_PRED_VSPU;

    public final static String OP_QUESTION = "?";
    public final static String OP_HASH = "#";

    public static final int MASK_TRCK_AUTO = 0x00000001;
    public static final int MASK_TRCK_BODY = 0x00000002;
    public static final int MASK_TRCK_DISC = 0x00000004;
    public static final int MASK_TRCK_DYNA = 0x00000008;

    public static final int MASK_TRCK_OVRD = 0x00000010;
    public static final int MASK_TRCK_MULT = 0x00000020;
    public static final int MASK_TRCK_VSPR = 0x00000040;
    public static final int MASK_TRCK_VSPU = 0x00000080;

    public static final int MASK_TRCK_META = 0x00000100;
    public static final int MASK_TRCK_HEAD = 0x00000200;
    public static final int MASK_TRCK_TRLC = 0x00000400;
    public static final int MASK_TRCK_GRLC = 0x00000800;

    private final int arity;
    private final String fun;
    private int flags;
    public AbstractDelegate del;
    public final MapHashLink<AbstractSource, Integer> defs = new MapHashLink<>();
    public MapEntry<AbstractSource, Integer>[] cachedefs;
    public Object meta_predicate;
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
    public String getFun() {
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

    /**************************************************************/
    /* Definiion Handling                                         */
    /**************************************************************/

    /**
     * <p>Add a source definition.</p>
     * <p>Can veto that a non-multifile predicate is extended.</p>
     * *
     *
     * @param s The source definition.
     * @param f The definition flags.
     * @return The diff flags.
     * @throws EngineMessage Shit happens.
     */
    public int addDef(AbstractSource s, int f)
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
        AbstractSource src = getSource();
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_REDEFINE,
                EngineMessage.OP_PERMISSION_PROCEDURE,
                SpecialPred.indicatorToColonSkel(getFun(), getArity(),
                        src.getFullName(), src.getStore().user)));
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
     * @param sa   The call-site, non null.
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
        int back = addDef(src, flags);
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
        if (!Branch.OP_USER.equals(getSource().getFullName())) {
            AbstractSource source = getSource();
            int f = AbstractSource.MASK_IMPT_MODL;
            f |= (!priv ? AbstractSource.MASK_IMPT_REEX : 0);
            CachePredicate.notifyImportvers(source, f);
        } else {
            Store store = getSource().getStore();
            store.foyer.notifyImportvers(store);
        }
    }

    /**
     * <p>Define a built-in for a predicate.</p>
     *
     * @param pick The predicate.
     * @param del  The delegate.
     * @throws EngineMessage Shit happens.
     */
    public static void definePredicate(Predicate pick,
                                       AbstractDelegate del)
            throws EngineMessage {
        /* check virtual flag */
        boolean f1 = ((pick.getBits() & MASK_PRED_VIRT) != 0);
        boolean f2 = ((del.subflags & AbstractDelegate.MASK_DELE_VIRT) != 0);
        if (f1 != f2) {
            AbstractSource src = pick.getSource();
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_COERCE,
                    EngineMessage.OP_PERMISSION_VIRTUAL,
                    SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                            src.getFullName(), src.getStore().user)));
        }
        /* create the builtin */
        AbstractDelegate fun = AbstractDelegate.promoteBuiltin(pick, del);
        if (del.equals(fun))
            return;
        AbstractSource src = pick.getSource();
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_COERCE,
                EngineMessage.OP_PERMISSION_PROCEDURE,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src.getFullName(), src.getStore().user)));
    }

    /***********************************************************/
    /* Style Checks Predicate Head                             */
    /***********************************************************/

    /**
     * <p>Perform a style check on the given predicate.</p>
     * <p>This check is performed during the loading of a module for
     * directives that do not create a delegate.</p>
     *
     * @param pick The predicate.
     * @param sa   The call-site.
     * @param en   The engine.
     * @throws EngineMessage Printing error.
     */
    public static void checkPredicateHead(Predicate pick, SkelAtom sa,
                                          Engine en)
            throws EngineMessage, EngineException {
        AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
        Integer def = pick.getDef(src);
        if (def == null)
            return;
        try {
            checkPredicateDiscontiguous(def, src, pick, en);
        } catch (EngineMessage x) {
            EngineException y = new EngineException(x,
                    EngineException.fetchStack(en), EngineException.OP_WARNING);
            y.printStackTrace(en);
        }
    }

    /**
     * <p>Perform the discontigous style check.</p>
     *
     * @param loc  The usage.
     * @param src  The call-site.
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateDiscontiguous(Integer loc,
                                                    AbstractSource src,
                                                    Predicate pick,
                                                    Engine en)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_HEAD) == 0) {
            en.visor.lastsk = makeKey(pick);
            pick.addDef(src, MASK_TRCK_HEAD);
            return;
        }
        if ((loc.intValue() & MASK_TRCK_DISC) != 0)
            return;
        StoreKey lastsk = en.visor.lastsk;
        if (lastsk != null &&
                pick.getArity() == lastsk.getArity() &&
                pick.getFun().equals(lastsk.getFun()) &&
                pick.getSource().getFullName().equals(lastsk.getModule()))
            return;
        en.visor.lastsk = makeKey(pick);
        AbstractSource src1 = pick.getSource();
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_DISCONTIGUOUS_PRED,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src1.getFullName(), src1.getStore().user)));
    }

    /**
     * <p>Create a possibly qualified store key.</p>
     *
     * @param pick The predicate,
     * @return The possibly qualified store key.
     */
    private static StoreKey makeKey(Predicate pick) {
        String mod = pick.getSource().getFullName();
        if (!Branch.OP_USER.equals(mod)) {
            return new StoreKeyQuali(pick.getFun(), pick.getArity(), mod);
        } else {
            return new StoreKey(pick.getFun(), pick.getArity());
        }
    }

    /***********************************************************/
    /* Style Checks Predicate Declaration                      */
    /***********************************************************/

    /**
     * <p>Perform a style check on the given predicate.</p>
     * <p>This check is performed during the loading of a module
     * for directives that create a delegate or clauses.</p>
     *
     * @param pick The predicate.
     * @param sa   The call-site.
     * @param en   The engine.
     * @throws EngineMessage Printing error.
     */
    public static void checkPredicateBody(Predicate pick, SkelAtom sa,
                                          Engine en)
            throws EngineMessage, EngineException {
        AbstractSource src = (sa.scope != null ? sa.scope : en.store.user);
        Integer def = pick.getDef(src);
        if (def == null)
            return;
        try {
            checkPredicateDiscontiguous(def, src, pick, en);
        } catch (EngineMessage x) {
            EngineException y = new EngineException(x,
                    EngineException.fetchStack(en), EngineException.OP_WARNING);
            y.printStackTrace(en);
        }
        try {
            if ((def.intValue() & MASK_TRCK_BODY) != 0)
                return;
            pick.addDef(src, MASK_TRCK_BODY);

            checkPredicateMultifile(def, pick);
            if (sa.scope != null &&
                    (sa.scope.getBits() & AbstractSource.MASK_SRC_VSPU) == 0)
                checkPredicatePublic(def, pick);
            checkPredicateMetaPredicate(def, pick);
            checkPredicateDynamic(def, pick);
            checkPredicateThreadLocal(def, pick);
            checkPredicateGroupLocal(def, pick);

            AbstractSource base = CachePredicate.performBase(sa, src, en);
            Predicate over;
            try {
                over = CachePredicate.performOverrides(sa, pick.getArity(), base);
            } catch (InterruptedException x) {
                throw (EngineMessage) ForeignThread.sysThreadClear();
            }

            checkPredicateOverride(def, src, pick, over);
            checkPredicateFresh(def, src, pick, over);
            checkPredicateMetaInherited(def, src, pick, over);
            checkPredicateMetaIllegal(def, src, pick, over);

            checkPredicateNumericSpecial(pick);
            checkPredicateNumericForeign(pick);
        } catch (EngineMessage x) {
            EngineException y = new EngineException(x,
                    EngineException.fetchStack(en), EngineException.OP_WARNING);
            y.printStackTrace(en);
        }
    }

    /**
     * <p>Perform the multifile style check.</p>
     *
     * @param loc  The location.
     * @param pick The predicate.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateMultifile(Integer loc,
                                                Predicate pick)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_MULT) != 0)
            return;
        if ((pick.getBits() & Predicate.MASK_PRED_MULT) == 0)
            return;
        AbstractSource src = pick.getSource();
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_MULTIFILE_PRED,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src.getFullName(), src.getStore().user)));
    }

    /**
     * <p>Perform the public style check.</p>
     *
     * @param loc  The location.
     * @param pick The predicate.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicatePublic(Integer loc,
                                             Predicate pick)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_VSPU) != 0)
            return;
        if ((pick.getBits() & Predicate.MASK_PRED_VSPU) == 0)
            return;
        AbstractSource src = pick.getSource();
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_PUBLIC_PRED,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src.getFullName(), src.getStore().user)));
    }

    /**
     * <p>Perform the meta predicate style check.</p>
     *
     * @param loc  The location.
     * @param pick The predicate.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateMetaPredicate(Integer loc,
                                                    Predicate pick)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_META) != 0)
            return;
        if (pick.meta_predicate == null)
            return;
        AbstractSource src = pick.getSource();
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_META_PREDICATE_PRED,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src.getFullName(), src.getStore().user)));
    }

    /**
     * <p>Perform the dynamic style check.</p>
     *
     * @param loc  The location.
     * @param pick The predicate.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateDynamic(Integer loc,
                                              Predicate pick)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_DYNA) != 0)
            return;
        AbstractDelegate fun = pick.del;
        if (fun == null || (fun.subflags & AbstractDefined.MASK_DEFI_DYNA) == 0)
            return;
        AbstractSource src = pick.getSource();
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_DYNAMIC_PRED,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src.getFullName(), src.getStore().user)));
    }

    /**
     * <p>Perform the thread local style check.</p>
     *
     * @param loc  The location.
     * @param pick The predicate.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateThreadLocal(Integer loc,
                                                  Predicate pick)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_TRLC) != 0)
            return;
        AbstractDelegate fun = pick.del;
        if (fun == null || (fun.subflags & AbstractDefined.MASK_DEFI_THLC) == 0)
            return;
        AbstractSource src = pick.getSource();
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_THREAD_LOCAL_PRED,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src.getFullName(), src.getStore().user)));
    }

    /**
     * <p>Perform the group local style check.</p>
     *
     * @param loc  The location.
     * @param pick The predicate.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateGroupLocal(Integer loc,
                                                 Predicate pick)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_GRLC) != 0)
            return;
        AbstractDelegate fun = pick.del;
        if (fun == null || (fun.subflags & AbstractDefined.MASK_DEFI_GRLC) == 0)
            return;
        AbstractSource src = pick.getSource();
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_THREAD_LOCAL_PRED,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src.getFullName(), src.getStore().user)));
    }

    /**
     * <p>Perform the override style check.</p>
     *
     * @param loc  The usage.
     * @param src  The call-site.
     * @param pick The predicate.
     * @param over The overridden predicate.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateOverride(Integer loc, AbstractSource src,
                                               Predicate pick, Predicate over)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_OVRD) != 0)
            return;
        if (over == null || !CachePredicate.visiblePred(over, src))
            return;
        AbstractSource src1 = pick.getSource();
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_OVERRIDE_PRED,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src1.getFullName(), src1.getStore().user)));
    }

    /**
     * <p>Perform the fresh style check.</p>
     *
     * @param loc  The usage.
     * @param src  The call-site.
     * @param pick The predicate.
     * @param over The overridden predicate.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateFresh(Integer loc, AbstractSource src,
                                            Predicate pick, Predicate over)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_OVRD) == 0)
            return;
        if (over != null && CachePredicate.visiblePred(over, src))
            return;
        AbstractSource src1 = pick.getSource();
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_FRESH_PRED,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src1.getFullName(), src1.getStore().user)));
    }

    /**
     * <p>Perform the meta inherited style check.</p>
     *
     * @param loc  The usage.
     * @param src  The call-site.
     * @param pick The predicate.
     * @param over The overridden predicate.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateMetaInherited(Integer loc, AbstractSource src,
                                                    Predicate pick, Predicate over)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_META) != 0)
            return;
        if (over == null || !CachePredicate.visiblePred(over, src))
            return;
        if (over.meta_predicate == null)
            return;
        AbstractSource src1 = pick.getSource();
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_META_INHERITED,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src1.getFullName(), src1.getStore().user)));
    }

    /**
     * <p>Perform the meta illegal style check.</p>
     *
     * @param loc  The usage.
     * @param src  The call-site.
     * @param pick The predicate.
     * @param over The overridden predicate.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateMetaIllegal(Integer loc, AbstractSource src,
                                                  Predicate pick, Predicate over)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_META) == 0)
            return;
        if (over == null || !CachePredicate.visiblePred(over, src))
            return;
        if (over.meta_predicate != null)
            return;
        AbstractSource src1 = pick.getSource();
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_META_ILLEGAL,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src1.getFullName(), src1.getStore().user)));
    }

    /**
     * <p>Perform the dynamic style check.</p>
     *
     * @param pick The predicate.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateNumericSpecial(Predicate pick)
            throws EngineMessage {
        AbstractDelegate fun = pick.del;
        if (!(fun instanceof AbstractSpecial))
            return;
        if (!((AbstractSpecial) fun).isNumeric())
            return;
        if (pick.meta_predicate != null)
            return;
        if (pick.getArity() == 1)
            return;
        AbstractSource src = pick.getSource();
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_NUMERIC_SPECIAL,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src.getFullName(), src.getStore().user)));
    }

    /**
     * <p>Perform the dynamic style check.</p>
     *
     * @param pick The predicate.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateNumericForeign(Predicate pick)
            throws EngineMessage {
        AbstractDelegate fun = pick.del;
        if (!(fun instanceof AbstractLense))
            return;
        if (!((AbstractLense) fun).isNumeric())
            return;
        if (pick.meta_predicate != null)
            return;
        if (pick.getArity() == 1)
            return;
        AbstractSource src = pick.getSource();
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_NUMERIC_FOREIGN,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src.getFullName(), src.getStore().user)));
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
            checkPredicateImplemented(def, pick);
        } catch (EngineMessage x) {
            PositionKey pos = PositionKey.createPos(lr);
            EngineException y = new EngineException(x, EngineException.fetchLoc(
                    EngineException.fetchStack(en), pos, en),
                    EngineException.OP_WARNING);
            y.printStackTrace(en);
        }
    }

    /**
     * <p>Perform the implemented check for a static predicate.</p>
     *
     * @param loc  The location.
     * @param pick The predicate.
     * @throws EngineMessage The warning.
     */
    private static void checkPredicateImplemented(Integer loc,
                                                  Predicate pick)
            throws EngineMessage {
        if ((loc.intValue() & MASK_TRCK_BODY) != 0)
            return;
        AbstractDelegate fun = pick.del;
        if (fun != null && (fun.subflags & AbstractDefined.MASK_DEFI_ASSE) != 0)
            return;
        AbstractSource src = pick.getSource();
        throw new EngineMessage(EngineMessage.syntaxError(
                EngineMessage.OP_SYNTAX_IMPLEMENTATION_PRED,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src.getFullName(), src.getStore().user)));
    }

}
