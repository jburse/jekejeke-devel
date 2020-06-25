package jekpro.reference.runtime;

import jekpro.frequent.basic.SpecialProxy;
import jekpro.frequent.standard.SupervisorCall;
import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.PrologReader;
import jekpro.model.rope.Directive;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.*;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class SpecialQuali extends AbstractSpecial {
    public final static String OP_COLON = ":";
    public final static String OP_COLONCOLON = "::";

    private final static int SPECIAL_CALL_COLON = 0;
    private final static int SPECIAL_CALL_COLONCOLON = 1;
    private final static int SPECIAL_SYS_GET_CLASS = 3;
    private final static int SPECIAL_SYS_REPLACE_SITE = 4;

    /**
     * <p>Create a colon special.</p>
     *
     * @param i The index.
     */
    public SpecialQuali(int i) {
        super(i);
        switch (i) {
            case SPECIAL_CALL_COLON:
            case SPECIAL_CALL_COLONCOLON:
                subflags |= MASK_DELE_VIRT;
                break;
            case SPECIAL_SYS_GET_CLASS:
            case SPECIAL_SYS_REPLACE_SITE:
                break;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
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
        switch (id) {
            case SPECIAL_CALL_COLON:
                SkelCompound temp = (SkelCompound) en.skel;
                Display ref = en.display;
                Object obj = SpecialQuali.slashToClass(temp.args[0], ref, false, true, en);
                SkelAtom mod = modToAtom(obj, temp.args[0], ref, en);
                SpecialQuali.colonToCallable(temp.args[1], ref, true, en);
                SpecialQuali.colonToRoutine(mod, temp.sym, true, en);

                Directive dire = SupervisorCall.callGoal2(AbstractDefined.MASK_DEFI_TRAN, en);
                Display d3 = en.display;

                CallFrame ref2 = CallFrame.getFrame(d3, dire, en);
                en.contskel = dire;
                en.contdisplay = ref2;
                return true;
            case SPECIAL_CALL_COLONCOLON:
                temp = (SkelCompound) en.skel;
                ref = en.display;

                en.skel = temp.args[0];
                en.display = ref;
                en.deref();
                Object recv = en.skel;
                Display d2 = en.display;

                obj = SpecialQuali.slashToClass(recv, d2, true, true, en);
                mod = objToAtom(obj, recv, d2, en);
                SpecialQuali.colonToCallable(temp.args[1], ref, true, en);
                SpecialQuali.colonToMethod(mod, temp.sym, recv, d2, true, en);

                dire = SupervisorCall.callGoal2(AbstractDefined.MASK_DEFI_TRAN, en);
                d3 = en.display;

                ref2 = CallFrame.getFrame(d3, dire, en);
                en.contskel = dire;
                en.contdisplay = ref2;
                return true;
            case SPECIAL_SYS_GET_CLASS:
                temp = (SkelCompound) en.skel;
                ref = en.display;
                Object m = SpecialUniv.derefAndCastRef(temp.args[0], ref);
                obj = SpecialProxy.refClassOrProxy(m);
                if (obj == null)
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_UNKNOWN_PROXY, m));
                if (!en.unifyTerm(temp.args[1], ref, obj, Display.DISPLAY_CONST))
                    return false;
                return true;
            case SPECIAL_SYS_REPLACE_SITE:
                temp = (SkelCompound) en.skel;
                ref = en.display;
                en.skel = temp.args[2];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                obj = en.skel;
                d2 = en.display;

                en.skel = temp.args[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkCallable(en.skel, en.display);
                SkelAtom sa2 = StackElement.callableToName(en.skel);

                SkelAtom sa = StackElement.callableToName(obj);
                sa = makeAtom(sa.fun, en, sa2);
                obj = StackElement.callableFromName(obj, sa);
                if (!en.unifyTerm(temp.args[0], ref, obj, d2))
                    return false;
                return true;
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
     * <p>The result is return in skel and display of the engine.</p>
     * <p>A qualified callable has the following syntax.</p>
     * <pre>
     *     colon --> module ":" colon
     *             | receiver "::" colon
     *             | term.
     * </pre>
     *
     * @param t    The colon skeleton.
     * @param d    The colon display.
     * @param comp The compound flag.
     * @param en   The engine.
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
            Object obj = SpecialQuali.slashToClass(temp.args[0], d, false, true, en);
            SkelAtom mod = modToAtom(obj, temp.args[0], d, en);
            SpecialQuali.colonToCallable(temp.args[1], d, comp, en);
            SpecialQuali.colonToRoutine(mod, temp.sym, comp, en);
        } else if (comp && t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(OP_COLONCOLON)) {
            SkelCompound temp = (SkelCompound) t;

            en.skel = temp.args[0];
            en.display = d;
            en.deref();
            Object recv = en.skel;
            Display d2 = en.display;

            Object obj = SpecialQuali.slashToClass(recv, d2, true, true, en);
            SkelAtom mod = objToAtom(obj, recv, d2, en);
            SpecialQuali.colonToCallable(temp.args[1], d, comp, en);
            SpecialQuali.colonToMethod(mod, temp.sym, recv, d2, comp, en);
        }
    }

    /***************************************************************/
    /* Indicator Colon                                             */
    /***************************************************************/

    /**
     * <p>Convert a qualified indicator to an indicator.</p>
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
        try {
            en.skel = t;
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
            if (t instanceof SkelCompound &&
                    ((SkelCompound) t).args.length == 2 &&
                    ((SkelCompound) t).sym.fun.equals(OP_COLON)) {
                SkelCompound temp = (SkelCompound) t;
                Object obj = SpecialQuali.slashToClass(temp.args[0], d, false, true, en);
                SkelAtom mod = modToAtom(obj, temp.args[0], d, en);
                Integer arity = SpecialQuali.colonToIndicator(temp.args[1], d, en);
                SkelAtom sa = (SkelAtom) en.skel;
                en.skel = CacheFunctor.getFunctor(sa, mod, temp.sym, en);
                return arity;
            } else if (t instanceof SkelCompound &&
                    ((SkelCompound) t).args.length == 2 &&
                    ((SkelCompound) t).sym.fun.equals(Foyer.OP_SLASH)) {
                SkelCompound sc = (SkelCompound) t;
                Number num = SpecialEval.derefAndCastInteger(sc.args[1], d);
                SpecialEval.checkNotLessThanZero(num);
                SpecialEval.castIntValue(num);
                en.skel = SpecialUniv.derefAndCastStringWrapped(sc.args[0], d);
                return (Integer) num;
            } else {
                EngineMessage.checkInstantiated(t);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_PREDICATE_INDICATOR, t), d);
            }
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

    /**
     * <p>Convert an indicator to a qualified indicator.</p>
     *
     * @param sa    The name.
     * @param arity The length.
     * @param en    The engine.
     * @return The colon indictor
     * @throws EngineMessage Shit happens.
     */
    public static Object indicatorToColonSkel(SkelAtom sa, int arity,
                                              Engine en)
            throws EngineMessage {
        Object s;
        if (sa instanceof SkelAtomQuali) {
            SkelAtom mod = ((SkelAtomQuali) sa).getModule();
            AbstractSource src = (mod.scope != null ? mod.scope : en.store.user);
            s = SpecialDynamic.moduleToSlashSkel(mod.fun, src);
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

    /**
     * <p>Convert an indicator to a qualified indicator.</p>
     *
     * @param fun   The name.
     * @param scope The scope, non null.
     * @param arity The length.
     * @param en    The engine.
     * @return The colon indictor
     * @throws EngineMessage Shit happens.
     */
    public static Object indicatorToColonSkel(String fun,
                                              AbstractSource scope,
                                              int arity,
                                              Engine en)
            throws EngineMessage {
        Object s;
        if (CacheFunctor.isQuali(fun)) {
            String mod = CacheFunctor.sepModule(fun);
            s = SpecialDynamic.moduleToSlashSkel(mod, scope);
            SkelAtom sa2 = new SkelAtom(OP_COLON, scope);

            Object t = new SkelCompound(en.store.foyer.ATOM_SLASH,
                    new SkelAtom(CacheFunctor.sepName(fun), scope),
                    Integer.valueOf(arity));
            s = new SkelCompound(sa2, s, t);
        } else {
            s = new SkelCompound(en.store.foyer.ATOM_SLASH,
                    new SkelAtom(fun, scope),
                    Integer.valueOf(arity));
        }
        return s;
    }

    /****************************************************************/
    /* Name Helper                                                  */
    /****************************************************************/

    /**
     * <p>Retrieve the module name.</p>
     *
     * @param mod The object.
     * @param t   The slash skeleton.
     * @param d   The slash display.
     * @param en  The engine.
     * @return The nodule name.
     * @throws EngineMessage Shit happens.
     */
    static SkelAtom objToAtom(Object mod, Object t, Display d,
                              Engine en)
            throws EngineMessage {
        if (!(mod instanceof AbstractSkel) &&
                !(mod instanceof Number)) {
            /* reference */
            mod = SpecialProxy.refClassOrProxy(mod);
            if (mod == null)
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_UNKNOWN_PROXY, t), d);
            mod = SpecialProxy.classOrProxyName(mod, en);
            if (mod == null)
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_CLASS, t), d);
        } else {
            /* atom */
        }
        return (SkelAtom) mod;
    }

    /**
     * <p>Retrieve the module atom.</p>
     *
     * @param mod The object.
     * @param t   The slash skeleton.
     * @param d   The slash display.
     * @param en  The engine.
     * @return The nodule atom.
     * @throws EngineMessage Shit happens.
     */
    public static SkelAtom modToAtom(Object mod, Object t, Display d, Engine en)
            throws EngineMessage {
        if (!(mod instanceof AbstractSkel) &&
                !(mod instanceof Number)) {
            /* reference */
            mod = SpecialProxy.classOrProxyName(mod, en);
            if (mod == null)
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_CLASS, t), d);
        } else {
            /* atom */
        }
        return (SkelAtom) mod;
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
            en.skel = new SkelCompound(CacheFunctor.getFunctor(sc2.sym, mod,
                    sa2, en), sc2.args, sc2.var);
        } else if (en.skel instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) en.skel;
            en.skel = CacheFunctor.getFunctor(sa, mod, sa2, en);
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
            SkelAtom sa = CacheFunctor.getFunctor(sc2.sym, mod, sa2, en);
            boolean multi = SpecialQuali.prependCount(recv, d2,
                    sc2.args, d3, en);
            en.skel = SpecialQuali.prependAlloc(sa, recv, d2,
                    sc2.args, d3, multi, en);
        } else if (en.skel instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) en.skel;
            sa = CacheFunctor.getFunctor(sa, mod, sa2, en);
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
        if (multi) {
            last = new Display(countvar);
            last.vars = Display.VARS_MARKER;
        }
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

}
