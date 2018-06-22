package jekpro.reference.runtime;

import jekpro.frequent.basic.SpecialProxy;
import jekpro.frequent.standard.EngineCopy;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.PrologReader;
import jekpro.model.rope.Clause;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>Provides built-in predicates for the module quali.</p>
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
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SpecialQuali extends AbstractSpecial {
    public final static String OP_COLON = ":";
    public final static String OP_COLONCOLON = "::";

    private final static int SPECIAL_CALL_COLON = 0;
    private final static int SPECIAL_CALL_COLONCOLON = 1;
    private final static int SPECIAL_SYS_GET_CLASS = 2;

    /**
     * <p>Create a colon special.</p>
     *
     * @param i The index.
     */
    public SpecialQuali(int i) {
        super(i);
        switch (i) {
            case SPECIAL_CALL_COLON:
                subflags |= MASK_DELE_VIRT;
                break;
            case SPECIAL_CALL_COLONCOLON:
                subflags |= MASK_DELE_VIRT;
                break;
            case SPECIAL_SYS_GET_CLASS:
                break;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
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
        switch (id) {
            case SPECIAL_CALL_COLON:
                SkelCompound temp = (SkelCompound) en.skel;
                Display ref = en.display;
                Object obj = slashToClass(temp.args[0], ref, false, true, en);
                String fun;
                /* reference */
                if (!(obj instanceof AbstractSkel) &&
                        !(obj instanceof Number)) {
                    fun = SpecialProxy.classOrProxyName(obj);
                    if (fun == null)
                        throw new EngineMessage(EngineMessage.domainError(
                                EngineMessage.OP_DOMAIN_CLASS, temp.args[0]), ref);
                    /* atom */
                } else {
                    fun = ((SkelAtom) obj).fun;
                }
                SpecialQuali.colonToCallable(temp.args[1], ref, true, en);
                if (en.skel instanceof SkelCompound) {
                    SkelCompound sc2 = (SkelCompound) en.skel;
                    en.skel = new SkelCompound(CacheFunctor.getFunctor(sc2.sym, fun,
                            temp.sym, en), sc2.args, sc2.vars);
                } else if (en.skel instanceof SkelAtom) {
                    SkelAtom sa = (SkelAtom) en.skel;
                    en.skel = CacheFunctor.getFunctor(sa, fun, temp.sym, en);
                } else {
                    EngineMessage.checkInstantiated(en.skel);
                    throw new EngineMessage(EngineMessage.typeError(
                            EngineMessage.OP_TYPE_CALLABLE,
                            en.skel), en.display);
                }
                en.wrapGoal();
                Clause clause = en.store.foyer.CLAUSE_CONT;
                DisplayClause ref2 = new DisplayClause(clause.dispsize);
                ref2.addArgument(en.skel, en.display, en);
                ref2.setEngine(en);
                en.contskel = clause.getNextRaw(en);
                en.contdisplay = ref2;
                return true;
            case SPECIAL_CALL_COLONCOLON:
                temp = (SkelCompound) en.skel;
                ref = en.display;
                obj = slashToClass(temp.args[0], ref, true, true, en);
                /* reference */
                if (!(obj instanceof AbstractSkel) &&
                        !(obj instanceof Number)) {
                    obj = SpecialProxy.refClassOrProxy(obj);
                    if (obj == null)
                        throw new EngineMessage(EngineMessage.domainError(
                                EngineMessage.OP_DOMAIN_UNKNOWN_PROXY, temp.args[0]), ref);
                    fun = SpecialProxy.classOrProxyName(obj);
                    if (fun == null)
                        throw new EngineMessage(EngineMessage.domainError(
                                EngineMessage.OP_DOMAIN_CLASS, temp.args[0]), ref);
                    /* atom */
                } else {
                    fun = ((SkelAtom) obj).fun;
                }
                SpecialQuali.colonToCallable(temp.args[1], ref, true, en);
                if (en.skel instanceof SkelCompound) {
                    SkelCompound sc2 = (SkelCompound) en.skel;
                    Display d3 = en.display;

                    en.skel = temp.args[0];
                    en.display = ref;
                    en.deref();
                    Object recv = en.skel;
                    Display d2 = en.display;

                    boolean multi = SpecialQuali.prependCount(recv, d2,
                            sc2.args, d3, en);
                    en.skel = new SkelCompound(CacheFunctor.getFunctor(sc2.sym, fun,
                            temp.sym, en), SpecialQuali.prependAlloc(recv, d2,
                            sc2.args, d3, multi, en));
                } else if (en.skel instanceof SkelAtom) {
                    SkelAtom sa = (SkelAtom) en.skel;

                    en.skel = temp.args[0];
                    en.display = ref;
                    en.deref();
                    Object recv = en.skel;
                    Display d2 = en.display;

                    en.skel = new SkelCompound(CacheFunctor.getFunctor(sa, fun,
                            temp.sym, en), recv);
                    en.display = d2;
                } else {
                    EngineMessage.checkInstantiated(en.skel);
                    throw new EngineMessage(EngineMessage.typeError(
                            EngineMessage.OP_TYPE_CALLABLE,
                            en.skel), en.display);
                }
                en.wrapGoal();
                clause = en.store.foyer.CLAUSE_CONT;
                ref2 = new DisplayClause(clause.dispsize);
                ref2.addArgument(en.skel, en.display, en);
                ref2.setEngine(en);
                en.contskel = clause.getNextRaw(en);
                en.contdisplay = ref2;
                return true;
            case SPECIAL_SYS_GET_CLASS:
                temp = (SkelCompound) en.skel;
                ref = en.display;
                en.skel = temp.args[0];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkRef(en.skel, en.display);
                obj = SpecialProxy.refClassOrProxy(en.skel);
                if (obj == null)
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_UNKNOWN_PROXY, en.skel));
                if (!en.unifyTerm(temp.args[1], ref, obj, Display.DISPLAY_CONST))
                    return false;
                return en.getNext();
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
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
     * @param t    The slash skeleton.
     * @param d    The slash display.
     * @param comp The compound flag.
     * @param err  The error flag.
     * @param en   The engine.
     * @return The module or class, or null.
     * @throws EngineMessage Shit happens.
     */
    public static Object slashToClass(Object t, Display d,
                                      boolean comp,
                                      boolean err,
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
            SkelAtom sa = slashToPackage(temp.args[0], d, false, err, en);
            if (sa == null)
                return null;
            t = temp.args[1];
            en.skel = t;
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
            if (comp && (t instanceof SkelCompound)) {
                SkelCompound sc2 = (SkelCompound) t;
                t = CacheModule.getModule(sa, sc2.sym.fun, false,
                        temp.sym.scope, en);
            } else if (t instanceof SkelAtom) {
                SkelAtom sa2 = (SkelAtom) t;
                t = CacheModule.getModule(sa, sa2.fun, false,
                        temp.sym.scope, en);
            } else {
                if (err) {
                    EngineMessage.checkInstantiated(t);
                    throw new EngineMessage(EngineMessage.typeError(
                            (comp ? EngineMessage.OP_TYPE_CALLABLE :
                                    EngineMessage.OP_TYPE_ATOM), t), d);
                } else {
                    return null;
                }
            }
        } else if (!comp && t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 1 &&
                ((SkelCompound) t).sym.fun.equals(PrologReader.OP_SET)) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa = slashToPackage(temp.args[0], d, true, err, en);
            if (sa == null)
                return null;
            t = CacheModule.getModule(sa, null, false,
                    temp.sym.scope, en);
        } else if (!(t instanceof Number) &&
                !(t instanceof AbstractSkel)) {
            /* */
        } else {
            if (comp && (t instanceof SkelCompound)) {
                SkelCompound sc = (SkelCompound) t;
                t = CacheModule.getModule(sc.sym, null, true,
                        sc.sym.scope, en);
            } else if (t instanceof SkelAtom) {
                SkelAtom sa = (SkelAtom) t;
                t = CacheModule.getModule(sa, null, true,
                        sa.scope, en);
            } else {
                if (err) {
                    EngineMessage.checkInstantiated(t);
                    throw new EngineMessage(EngineMessage.domainError(
                            (comp ? EngineMessage.OP_DOMAIN_RECEIVER :
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
     * @param t   The slash skeleton.
     * @param d   The slash display.
     * @param set The set flag.
     * @param err The error flag.
     * @param en  The engine.
     * @return The package, or null.
     * @throws EngineMessage Shit happens.
     */
    public static SkelAtom slashToPackage(Object t, Display d,
                                          boolean set,
                                          boolean err,
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
            SkelAtom sa = slashToPackage(temp.args[0], d, false, err, en);
            if (sa == null)
                return null;
            en.skel = temp.args[1];
            en.display = d;
            en.deref();
            t = en.skel;
            if (t instanceof SkelAtom)
                return CachePackage.getPackage(sa, ((SkelAtom) t).fun);
            if (err) {
                EngineMessage.checkInstantiated(t);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_ATOM, t), d);
            } else {
                return null;
            }
        } else if (set && (t instanceof SkelCompound) &&
                ((SkelCompound) t).args.length == 1 &&
                ((SkelCompound) t).sym.fun.equals(PrologReader.OP_SET)) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa = slashToPackage(temp.args[0], d, true, err, en);
            if (sa == null)
                return null;
            return CachePackage.getPackage(sa, null);
        } else {
            if (t instanceof SkelAtom)
                return (SkelAtom) t;
            if (err) {
                EngineMessage.checkInstantiated(t);
                throw new EngineMessage(EngineMessage.domainError(
                        (set ? EngineMessage.OP_DOMAIN_ARRAY :
                                EngineMessage.OP_DOMAIN_PACKAGE), t), d);
            } else {
                return null;
            }
        }
    }

    /************************************************************/
    /* Callable Colon                                           */
    /************************************************************/

    /**
     * <p>Convert a colon to a callable.</p>
     * <p>A colon callable has the following syntax.</p>
     * <pre>
     *     colon --> module ":" colon
     *             | receiver "::" colon
     *             | term.
     * </pre>
     *
     * @param t  The slash skeleton.
     * @param d  The slash display.
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void colonToCallable(Object t, Display d,
                                       boolean comp,
                                       Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(OP_COLON)) {
            SkelCompound temp = (SkelCompound) t;
            Object obj = slashToClass(temp.args[0], d, false, true, en);
            String fun;
            /* reference */
            if (!(obj instanceof AbstractSkel) &&
                    !(obj instanceof Number)) {
                fun = SpecialProxy.classOrProxyName(obj);
                if (fun == null)
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_CLASS, temp.args[0]), d);
                /* atom */
            } else {
                fun = ((SkelAtom) obj).fun;
            }
            SpecialQuali.colonToCallable(temp.args[1], d, comp, en);
            if (en.skel instanceof SkelAtom) {
                SkelAtom sa = (SkelAtom) en.skel;
                en.skel = CacheFunctor.getFunctor(sa, fun, temp.sym, en);
            } else if (comp && en.skel instanceof SkelCompound) {
                SkelCompound sc2 = (SkelCompound) en.skel;
                en.skel = new SkelCompound(CacheFunctor.getFunctor(sc2.sym, fun,
                        temp.sym, en), sc2.args, sc2.vars);
            } else {
                EngineMessage.checkInstantiated(en.skel);
                throw new EngineMessage(EngineMessage.typeError(
                        (comp ? EngineMessage.OP_TYPE_CALLABLE :
                                EngineMessage.OP_TYPE_ATOM), en.skel), en.display);
            }
        } else if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(OP_COLONCOLON)) {
            SkelCompound temp = (SkelCompound) t;
            Object obj = slashToClass(temp.args[0], d, true, true, en);
            String fun;
            /* reference */
            if (!(obj instanceof AbstractSkel) &&
                    !(obj instanceof Number)) {
                obj = SpecialProxy.refClassOrProxy(obj);
                if (obj == null)
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_UNKNOWN_PROXY, temp.args[0]), d);
                fun = SpecialProxy.classOrProxyName(obj);
                if (fun == null)
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_CLASS, temp.args[0]), d);
                /* atom */
            } else {
                fun = ((SkelAtom) obj).fun;
            }
            SpecialQuali.colonToCallable(temp.args[1], d, comp, en);
            if (en.skel instanceof SkelAtom) {
                SkelAtom sa = (SkelAtom) en.skel;
                en.skel = temp.args[0];
                en.display = d;
                en.deref();
                Object recv = en.skel;
                Display d2 = en.display;
                en.skel = new SkelCompound(CacheFunctor.getFunctor(sa, fun,
                        temp.sym, en), recv);
                en.display = d2;
            } else if (comp && en.skel instanceof SkelCompound) {
                SkelCompound sc2 = (SkelCompound) en.skel;
                Display d3 = en.display;
                en.skel = temp.args[0];
                en.display = d;
                en.deref();
                Object recv = en.skel;
                Display d2 = en.display;
                boolean multi = SpecialQuali.prependCount(recv, d2,
                        sc2.args, d3, en);
                en.skel = new SkelCompound(CacheFunctor.getFunctor(sc2.sym, fun,
                        temp.sym, en), SpecialQuali.prependAlloc(recv, d2,
                        sc2.args, d3, multi, en));
            } else {
                EngineMessage.checkInstantiated(en.skel);
                throw new EngineMessage(EngineMessage.typeError(
                        (comp ? EngineMessage.OP_TYPE_CALLABLE :
                                EngineMessage.OP_TYPE_ATOM), en.skel), en.display);
            }
        }
    }

    /***************************************************************/
    /* Indicator Colon                                             */
    /***************************************************************/

    /**
     * <p>Convert a qualified indicator to an indicator.</p>
     * <p>The term is passed in skel and display.</p>
     * <p>The name is returned in skel.</p>
     * <p>A qualified indicator has the following syntax.</p>
     * <pre>
     *     indicator --> module ":" indicator
     *                 | name "/" length.
     * </pre>
     *
     * @param en The engine.
     * @return The length.
     * @throws EngineMessage The indicator is not wellformed.
     */
    public static Integer colonToIndicator(Object t, Display d, Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(OP_COLON)) {
            SkelCompound temp = (SkelCompound) t;
            Object obj = slashToClass(temp.args[0], d, false, true, en);
            String fun;
            /* reference */
            if (!(obj instanceof AbstractSkel) &&
                    !(obj instanceof Number)) {
                fun = SpecialProxy.classOrProxyName(obj);
                if (fun == null)
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_CLASS, temp.args[0]), d);
                /* atom */
            } else {
                fun = ((SkelAtom) obj).fun;
            }
            Integer arity = SpecialQuali.colonToIndicator(temp.args[1], d, en);
            SkelAtom sa = (SkelAtom) en.skel;
            en.skel = CacheFunctor.getFunctor(sa, fun, temp.sym, en);
            return arity;
        } else if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SLASH)) {
            SkelCompound sc = (SkelCompound) t;
            en.skel = sc.args[1];
            en.display = d;
            en.deref();
            EngineMessage.checkInstantiated(en.skel);
            Number num = EngineMessage.castInteger(en.skel, en.display);
            EngineMessage.checkNotLessThanZero(num);
            EngineMessage.castIntValue(num);
            en.skel = sc.args[0];
            en.display = d;
            en.deref();
            EngineMessage.checkInstantiated(en.skel);
            EngineMessage.castStringWrapped(en.skel, en.display);
            return (Integer) num;
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_PREDICATE_INDICATOR, t), d);
        }
    }

    /**
     * <p>Convert an indicator to a qualified indicator.</p>
     *
     * @param sa    The name.
     * @param arity The length.
     * @param en    The engine.
     * @return The colon
     * @throws EngineMessage Shit happens.
     */
    public static Object indicatorToColonSkel(SkelAtom sa, int arity,
                                              Engine en)
            throws EngineMessage {
        Object s;
        if (CacheFunctor.isQuali(sa.fun)) {
            s = Clause.moduleToSlashSkel(CacheFunctor.sepModule(sa.fun), sa.scope, en);
            int m = (sa.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
            SkelAtom sa2 = en.store.foyer.createAtom(OP_COLON, sa.scope, m);
            sa2.setPosition(sa.getPosition());

            Object t = new SkelCompound(en.store.foyer.ATOM_SLASH,
                    new SkelAtom(CacheFunctor.sepName(sa.fun)),
                    Integer.valueOf(arity));
            s = new SkelCompound(sa2, s, t);
        } else {
            s = new SkelCompound(en.store.foyer.ATOM_SLASH, sa,
                    Integer.valueOf(arity));
        }
        return s;
    }

    /***************************************************************/
    /* Prepend Quali                                               */
    /***************************************************************/

    /**
     * <p>Count the needed variable place holders.</p>
     * <p>The reused or new display is returned in the engine display.</p>
     *
     * @param t      The receiver skel.
     * @param d      The receiver display.
     * @param t2args The message arguments.
     * @param d2     The message display.
     * @param en     The engine.
     * @return True if new display is returned, otherwise false.
     */
    static boolean prependCount(Object t, Display d,
                                Object[] t2args, Display d2,
                                Engine en) {
        int countvar = 0;
        Display last = Display.DISPLAY_CONST;
        boolean multi = false;
        for (int i = -1; i < t2args.length; i++) {
            if (i != -1) {
                en.skel = t2args[i];
                en.display = d2;
                en.deref();
            } else {
                en.skel = t;
                en.display = d;
            }
            if (!EngineCopy.isGroundSkel(en.skel)) {
                countvar++;
                if (last == Display.DISPLAY_CONST) {
                    last = en.display;
                } else if (last != en.display) {
                    multi = true;
                }
            }
        }
        if (multi)
            last = new Display(countvar);
        en.display = last;
        return multi;
    }

    /**
     * <p>Copy the arguments.</p>
     * <p>The reused or new display is passed via the engine display</p>
     * <p>The reused or new display is returned in the engine display.</p>
     *
     * @param t      The receiver skel.
     * @param d      The receiver display.
     * @param t2args The message arguments.
     * @param d2     The message display.
     * @param multi  The multi flag.
     * @param en     The engine.
     * @return The copied arguments.
     */
    static Object[] prependAlloc(Object t, Display d,
                                 Object[] t2args, Display d2,
                                 boolean multi, Engine en) {
        Display d4 = en.display;
        Object[] args = new Object[t2args.length + 1];
        int countvar = 0;
        for (int i = -1; i < t2args.length; i++) {
            if (i != -1) {
                en.skel = t2args[i];
                en.display = d2;
                en.deref();
            } else {
                en.skel = t;
                en.display = d;
            }
            if (multi && !EngineCopy.isGroundSkel(en.skel)) {
                SkelVar sv = SkelVar.valueOf(countvar);
                countvar++;
                d4.bind[sv.id].bindVar(en.skel, en.display, en);
                args[i + 1] = sv;
            } else {
                args[i + 1] = en.skel;
            }
        }
        en.display = d4;
        return args;
    }

}
