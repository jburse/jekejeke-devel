package jekpro.reference.runtime;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.tools.term.*;

/**
 * <p>Provides built-in predicates for qualified evaluation.</p>
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
public final class EvaluableLogic extends AbstractSpecial {
    public final static String OP_COLON = ":";
    public final static String OP_COLONCOLON = "::";

    private final static int EVALUABLE_COLON = 0;
    private final static int EVALUABLE_COLONCOLON = 1;

    /**
     * <p>Create an evaluable quali.</p>
     *
     * @param i The index.
     */
    public EvaluableLogic(int i) {
        super(i);
        switch (i) {
            case EVALUABLE_COLON:
            case EVALUABLE_COLONCOLON:
                subflags |= MASK_DELE_VIRT;
                break;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Arithmetically evaluate an evaluable.</p>
     * <p>The evaluable is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the contskel and contdisplay of the engine.</p>
     * <p>The result is passed via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public final void moniEvaluate(Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case EVALUABLE_COLON:
                SkelCompound temp = (SkelCompound) en.skel;
                Display ref = en.display;

                Object obj = slashToClass(temp.args[0], ref, 0, en);
                SkelAtom mod = SpecialLogic.modToAtom(obj, temp.args[0], ref, en);
                SpecialLogic.colonToCallable(temp.args[1], ref, true, en);
                colonToRoutine(mod, temp.sym, true, en);
                ref = en.display;
                boolean multi = ref.getAndReset();
                en.computeExpr(en.skel, ref);
                if (multi)
                    ref.remTab(en);
                return;
            case EVALUABLE_COLONCOLON:
                temp = (SkelCompound) en.skel;
                ref = en.display;

                en.skel = temp.args[0];
                en.display = ref;
                en.deref();
                Object recv = en.skel;
                Display d2 = en.display;

                obj = slashToClass(recv, d2, CacheModule.MASK_MODULE_CMPD, en);
                mod = SpecialLogic.objToAtom(obj, recv, d2, en);
                SpecialLogic.colonToCallable(temp.args[1], ref, true, en);
                colonToMethod(mod, temp.sym, recv, d2, true, en);
                ref = en.display;
                multi = ref.getAndReset();
                en.computeExpr(en.skel, ref);
                if (multi)
                    ref.remTab(en);
                return;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /****************************************************************/
    /* Extend Helper                                                */
    /****************************************************************/

    /**
     * <p>Add module name to callable.</p>
     *
     * @param mod  The module name.
     * @param sa2  The call site.
     * @param comp The compound flag.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void colonToRoutine(SkelAtom mod, SkelAtom sa2,
                                      boolean comp, Engine en)
            throws EngineMessage {
        if (comp && en.skel instanceof SkelCompound) {
            SkelCompound sc2 = (SkelCompound) en.skel;
            en.skel = new SkelCompound(CacheFunctor.getModFunc(sc2.sym, mod,
                    sa2, en), sc2.args, sc2.var);
        } else if (en.skel instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) en.skel;
            en.skel = CacheFunctor.getModFunc(sa, mod, sa2, en);
        } else {
            EngineMessage.checkInstantiated(en.skel);
            throw new EngineMessage(EngineMessage.typeError(
                    (comp ? EngineMessage.OP_TYPE_CALLABLE :
                            EngineMessage.OP_TYPE_ATOM), en.skel), en.display);
        }
    }

    /**
     * <p>Add module name and receiver to callable.</p>
     *
     * @param mod  The module name.
     * @param sa2  The call site.
     * @param recv The receiver skeleton.
     * @param d2   The receiver display.
     * @param comp The compound flag.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void colonToMethod(SkelAtom mod, SkelAtom sa2,
                                     Object recv, Display d2,
                                     boolean comp,
                                     Engine en)
            throws EngineMessage {
        if (comp && en.skel instanceof SkelCompound) {
            SkelCompound sc2 = (SkelCompound) en.skel;
            Display d3 = en.display;
            SkelAtom sa = CacheFunctor.getModFunc(sc2.sym, mod, sa2, en);
            boolean multi = prependCount(recv, d2,
                    sc2.args, d3, en);
            en.skel = prependAlloc(sa, recv, d2,
                    sc2.args, d3, multi, en);
        } else if (en.skel instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) en.skel;
            sa = CacheFunctor.getModFunc(sa, mod, sa2, en);
            en.skel = new SkelCompound(sa, recv);
            en.display = d2;
        } else {
            EngineMessage.checkInstantiated(en.skel);
            throw new EngineMessage(EngineMessage.typeError(
                    (comp ? EngineMessage.OP_TYPE_CALLABLE :
                            EngineMessage.OP_TYPE_ATOM), en.skel), en.display);
        }
    }

    /***************************************************************/
    /* Prepend Quali                                               */
    /***************************************************************/

    /**
     * <p>Count the needed variable place holders.</p>
     * <p>The reused or new display is returned in the engine display.</p>
     *
     * @param t  The receiver skel.
     * @param d  The receiver display.
     * @param t2 The message arguments.
     * @param d2 The message display.
     * @param en The engine.
     * @return True if new display is returned, otherwise false.
     */
    private static boolean prependCount(Object t, Display d,
                                        Object[] t2, Display d2,
                                        Engine en) {
        int countvar = 0;
        Display last = Display.DISPLAY_CONST;
        boolean multi = false;
        for (int i = -1; i < t2.length; i++) {
            if (i == -1) {
                en.skel = t;
                en.display = d;
            } else {
                en.skel = t2[i];
                en.display = d2;
            }
            en.deref();
            if (SupervisorCopy.getVar(en.skel) != null) {
                countvar++;
                if (last == Display.DISPLAY_CONST) {
                    last = en.display;
                } else if (last != en.display) {
                    multi = true;
                }
            }
        }
        if (multi)
            last = new DisplayMarkable(countvar);
        en.display = last;
        return multi;
    }

    /**
     * <p>Copy the arguments.</p>
     * <p>The reused or new display is passed via the engine display</p>
     * <p>The reused or new display is returned in the engine display.</p>
     *
     * @param sa    The symbol.
     * @param t     The receiver skel.
     * @param d     The receiver display.
     * @param t2    The message arguments.
     * @param d2    The message display.
     * @param multi The multi flag.
     * @param en    The engine.
     * @return The new compound.
     */
    private static SkelCompound prependAlloc(SkelAtom sa,
                                             Object t, Display d,
                                             Object[] t2, Display d2,
                                             boolean multi, Engine en) {
        Display d4 = en.display;
        SkelVar[] vars;
        if (multi) {
            vars = SkelVar.valueOfArray(d4.bind.length);
        } else {
            vars = null;
        }
        Object[] args = new Object[t2.length + 1];
        int countvar = 0;
        boolean ext = (multi && d2.getAndReset());
        for (int i = -1; i < t2.length; i++) {
            if (i == -1) {
                en.skel = t;
                en.display = d;
            } else {
                en.skel = t2[i];
                en.display = d2;
            }
            en.deref();
            if (multi && SupervisorCopy.getVar(en.skel) != null) {
                SkelVar sv = vars[countvar];
                countvar++;
                d4.bind[sv.id].bindUniv(en.skel, en.display, en);
                args[i + 1] = sv;
            } else {
                args[i + 1] = en.skel;
            }
        }
        if (ext)
            d2.remTab(en);
        en.display = d4;
        if (multi) {
            return new SkelCompound(sa, args, vars);
        } else {
            return new SkelCompound(sa, args);
        }
    }

    /**
     * <p>Create a new atom for a given site.</p>
     *
     * @param fun The name of the atom.
     * @param en  The engine.
     * @param sa2 The call-site, or null.
     * @return The new atom.
     */
    public static SkelAtom makeAtom(String fun, Engine en, SkelAtom sa2) {
        AbstractSource scope = (sa2 != null ? sa2.scope : null);
        PositionKey pos = (sa2 != null ? sa2.getPosition() : null);

        int m = (pos != null ? SkelAtom.MASK_ATOM_POSI : 0);
        sa2 = en.store.foyer.createAtom(fun, scope, m);
        sa2.setPosition(pos);

        return sa2;
    }

    /************************************************************/
    /* Callable Slash                                           */
    /************************************************************/

    /**
     * <p>Convert a slash module or receiver to an object.</p>
     * <p>An object has the following syntax.</p>
     * <pre>
     * module   --> package "/" atom
     *            | "{" array "}"
     *            | reference
     *            | atom.
     *
     * receiver --> package "/" callable
     *            | reference
     *            | callable.
     * </pre>
     *
     * @param t     The slash skeleton.
     * @param d     The slash display.
     * @param flags The flags.
     * @param en    The engine.
     * @return The module or class, or null.
     * @throws EngineMessage Shit happens.
     */
    public static Object slashToClass(Object t, Display d,
                                      int flags,
                                      Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SLASH)) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa = slashToPackage(temp.args[0], d, flags, en);
            if (sa == null)
                return null;
            t = temp.args[1];
            en.skel = t;
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
            if ((flags & CacheModule.MASK_MODULE_CMPD) != 0 && (t instanceof SkelCompound)) {
                SkelCompound sc2 = (SkelCompound) t;
                t = CacheModule.getModule(sa, sc2.sym.fun, (flags & CacheModule.MASK_MODULE_NAUT),
                        temp.sym.scope, en);
            } else if (t instanceof SkelAtom) {
                SkelAtom sa2 = (SkelAtom) t;
                t = CacheModule.getModule(sa, sa2.fun, (flags & CacheModule.MASK_MODULE_NAUT),
                        temp.sym.scope, en);
            } else {
                if ((flags & CacheModule.MASK_MODULE_NERR) == 0) {
                    EngineMessage.checkInstantiated(t);
                    throw new EngineMessage(EngineMessage.typeError(
                            ((flags & CacheModule.MASK_MODULE_CMPD) != 0 ? EngineMessage.OP_TYPE_CALLABLE :
                                    EngineMessage.OP_TYPE_ATOM), t), d);
                } else {
                    return null;
                }
            }
        } else if ((flags & CacheModule.MASK_MODULE_CMPD) == 0 && t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 1 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SET)) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa = slashToPackage(temp.args[0], d,
                    (flags | CacheModule.MASK_MODULE_ARRC), en);
            if (sa == null)
                return null;
            t = CacheModule.getModule(sa, null, (flags & CacheModule.MASK_MODULE_NAUT),
                    temp.sym.scope, en);
        } else if (!(t instanceof Number) &&
                !(t instanceof AbstractSkel)) {
            /* */
        } else {
            if ((flags & CacheModule.MASK_MODULE_CMPD) != 0 && (t instanceof SkelCompound)) {
                SkelCompound sc = (SkelCompound) t;
                t = CacheModule.getModule(sc.sym, null, (flags & CacheModule.MASK_MODULE_NAUT) |
                                CacheModule.MASK_MODULE_SOLE,
                        sc.sym.scope, en);
            } else if (t instanceof SkelAtom) {
                SkelAtom sa = (SkelAtom) t;
                t = CacheModule.getModule(sa, null, (flags & CacheModule.MASK_MODULE_NAUT) |
                                CacheModule.MASK_MODULE_SOLE,
                        sa.scope, en);
            } else {
                if ((flags & CacheModule.MASK_MODULE_NERR) == 0) {
                    EngineMessage.checkInstantiated(t);
                    throw new EngineMessage(EngineMessage.domainError(
                            ((flags & CacheModule.MASK_MODULE_CMPD) != 0 ? EngineMessage.OP_DOMAIN_RECEIVER :
                                    EngineMessage.OP_DOMAIN_MODULE), t), d);
                } else {
                    return null;
                }
            }
        }
        return t;
    }

    /**
     * <p>Convert a slash array or package to an atom.</p>
     * <p>Packages and arrays have the following syntax.</p>
     * <pre>
     * array     --> package "/" atom.
     *             | "{" array "}"
     *             | atom.
     *
     * package   --> package "/" atom.
     *             | atom.
     * </pre>
     *
     * @param t     The slash skeleton.
     * @param d     The slash display.
     * @param flags The flag.
     * @param en    The engine.
     * @return The package, or null.
     * @throws EngineMessage Shit happens.
     */
    public static SkelAtom slashToPackage(Object t, Display d,
                                          int flags,
                                          Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SLASH)) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa = slashToPackage(temp.args[0], d,
                    (flags & ~CacheModule.MASK_MODULE_ARRC), en);
            if (sa == null)
                return null;
            en.skel = temp.args[1];
            en.display = d;
            en.deref();
            t = en.skel;
            if (t instanceof SkelAtom)
                return CachePackage.getPackage(sa, ((SkelAtom) t).fun);
            if ((flags & CacheModule.MASK_MODULE_NERR) == 0) {
                EngineMessage.checkInstantiated(t);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_ATOM, t), d);
            } else {
                return null;
            }
        } else if ((flags & CacheModule.MASK_MODULE_ARRC) != 0 && (t instanceof SkelCompound) &&
                ((SkelCompound) t).args.length == 1 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SET)) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa = slashToPackage(temp.args[0], d, flags, en);
            if (sa == null)
                return null;
            return CachePackage.getPackage(sa, null);
        } else {
            if (t instanceof SkelAtom)
                return (SkelAtom) t;
            if ((flags & CacheModule.MASK_MODULE_NERR) == 0) {
                EngineMessage.checkInstantiated(t);
                throw new EngineMessage(EngineMessage.domainError(
                        ((flags & CacheModule.MASK_MODULE_ARRC) != 0 ? EngineMessage.OP_DOMAIN_ARRAY :
                                EngineMessage.OP_DOMAIN_PACKAGE), t), d);
            } else {
                return null;
            }
        }
    }

}
