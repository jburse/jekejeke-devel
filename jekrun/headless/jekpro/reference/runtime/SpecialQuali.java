package jekpro.reference.runtime;

import jekpro.frequent.basic.SpecialProxy;
import jekpro.frequent.standard.EngineCopy;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.PrologReader;
import jekpro.model.pretty.SourceLocal;
import jekpro.model.rope.Clause;
import jekpro.reference.bootload.ForeignPath;
import jekpro.reference.bootload.SpecialLoad;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>Provides built-in predicates for the module colon.</p>
 *
 * @author Copyright 2014-2018, XLOG Technologies GmbH, Switzerland
 * @version Jekejeke Prolog 0.9.2 (a fast and small prolog interpreter)
 */
public final class SpecialQuali extends AbstractSpecial {
    public static final char OP_CHAR_SEG = '.';
    public static final char OP_CHAR_SEP = '\b';
    public final static String OP_COLON = ":";
    public final static String OP_COLONCOLON = "::";

    private final static int SPECIAL_CALL_COLON = 3;
    private final static int SPECIAL_CALL_COLONCOLON = 4;

    /**
     * <p>Create a colon special.</p>
     *
     * @param i The index.
     */
    public SpecialQuali(int i) {
        super(i);
        subflags |= MASK_DELE_VIRT;
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
                SkelCompound t = (SkelCompound) en.skel;
                Display d = en.display;
                en.skel = t.args[0];
                en.display = d;
                en.deref();
                SpecialQuali.slashToClass(en, false);
                String fun;
                /* reference */
                if (!(en.skel instanceof AbstractSkel) &&
                        !(en.skel instanceof Number)) {
                    fun = SpecialProxy.classOrProxyName(en.skel);
                    if (fun == null)
                        throw new EngineMessage(EngineMessage.typeError(
                                EngineMessage.OP_TYPE_CLASS, en.skel));
                    /* atom */
                } else {
                    fun = ((SkelAtom) en.skel).fun;
                }
                en.skel = t.args[1];
                en.display = d;
                en.deref();
                SpecialQuali.colonToCallable(en);
                if (en.skel instanceof SkelCompound) {
                    SkelCompound sc2 = (SkelCompound) en.skel;
                    en.skel = new SkelCompound(CacheFunctor.getFunctor(sc2.sym, fun,
                            t.sym, en.store), sc2.args, sc2.vars);
                } else if (en.skel instanceof SkelAtom) {
                    SkelAtom sa = (SkelAtom) en.skel;
                    en.skel = CacheFunctor.getFunctor(sa, fun, t.sym, en.store);
                } else {
                    EngineMessage.checkInstantiated(en.skel);
                    throw new EngineMessage(EngineMessage.typeError(
                            EngineMessage.OP_TYPE_CALLABLE,
                            en.skel), en.display);
                }
                en.wrapGoal();
                Clause clause = en.store.foyer.CLAUSE_CONT;
                DisplayClause ref = new DisplayClause(clause.dispsize);
                ref.addArgument(en.skel, en.display, en);
                ref.setEngine(en);
                en.contskel = clause.getNextRaw(en);
                en.contdisplay = ref;
                return true;
            case SPECIAL_CALL_COLONCOLON:
                t = (SkelCompound) en.skel;
                d = en.display;
                en.skel = t.args[0];
                en.display = d;
                en.deref();
                Object recv = en.skel;
                Display d2 = en.display;
                SpecialQuali.slashToClass(en, true);
                /* reference */
                if (!(en.skel instanceof AbstractSkel) &&
                        !(en.skel instanceof Number)) {
                    en.skel = SpecialProxy.refClassOrProxy(en.skel);
                    if (en.skel == null)
                        throw new EngineMessage(EngineMessage.domainError(
                                EngineMessage.OP_DOMAIN_UNKNOWN_PROXY, en.skel), en.display);
                    fun = SpecialProxy.classOrProxyName(en.skel);
                    if (fun == null)
                        throw new EngineMessage(EngineMessage.typeError(
                                EngineMessage.OP_TYPE_CLASS, en.skel));
                    /* atom */
                } else {
                    fun = ((SkelAtom) en.skel).fun;
                }
                en.skel = t.args[1];
                en.display = d;
                en.deref();
                SpecialQuali.colonToCallable(en);
                if (en.skel instanceof SkelCompound) {
                    SkelCompound sc2 = (SkelCompound) en.skel;
                    Display d3 = en.display;
                    boolean multi = prependCount(recv, d2,
                            sc2.args, d3, en);
                    en.skel = new SkelCompound(CacheFunctor.getFunctor(sc2.sym, fun,
                            t.sym, en.store), prependAlloc(recv, d2,
                            sc2.args, d3, multi, en));
                } else if (en.skel instanceof SkelAtom) {
                    SkelAtom sa = (SkelAtom) en.skel;
                    en.skel = new SkelCompound(CacheFunctor.getFunctor(sa, fun,
                            t.sym, en.store), recv);
                    en.display = d2;
                } else {
                    EngineMessage.checkInstantiated(en.skel);
                    throw new EngineMessage(EngineMessage.typeError(
                            EngineMessage.OP_TYPE_CALLABLE,
                            en.skel), en.display);
                }
                en.wrapGoal();
                clause = en.store.foyer.CLAUSE_CONT;
                ref = new DisplayClause(clause.dispsize);
                ref.addArgument(en.skel, en.display, en);
                ref.setEngine(en);
                en.contskel = clause.getNextRaw(en);
                en.contdisplay = ref;
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /************************************************************/
    /* Callable Slash                                           */
    /************************************************************/

    /**
     * <p>Convert a module to a slash.</p>
     * <p>A module has the following external syntax.</p>
     * <pre>
     *     module --> package "/" atom.
     *              | "{" package "}"
     *              | atom.
     * </pre>
     *
     * @param fun   The module.
     * @param scope The scope.
     * @param en    The engine.
     * @return The skeleton.
     */
    public static Object moduleToSlashSkel(String fun,
                                           AbstractSource scope,
                                           Engine en)
            throws EngineMessage {
        /* shorten module name */
        AbstractSource src = (scope != null ? scope : en.store.user);
        fun = fun.replace(SpecialQuali.OP_CHAR_SEG, SourceLocal.OP_CHAR_OS);
        fun = Engine.unfindPrefix(fun, src, ForeignPath.MASK_MODL_AUTO);
        fun = fun.replace(SourceLocal.OP_CHAR_OS, SpecialQuali.OP_CHAR_SEG);

        if (SpecialQuali.isArray(fun)) {
            SkelAtom sa2 = new SkelAtom(PrologReader.OP_SET, src);

            Object t = SpecialLoad.packageToSlashSkel(sepComp(fun));
            return new SkelCompound(sa2, t);
        } else if (SpecialQuali.isStruct(fun)) {
            SkelAtom sa2 = new SkelAtom(Foyer.OP_SLASH, src);

            Object t = SpecialLoad.packageToSlashSkel(sepPack(fun));
            SkelAtom s = new SkelAtom(sepBase(fun));
            return new SkelCompound(sa2, t, s);
        } else {
            return new SkelAtom(fun, src);
        }
    }

    /**
     * <p>Convert a slash to a package.</p>
     * <p>A package has the following syntax.</p>
     * <pre>
     *     package --> package "/" atom
     *               | "{" package "}"
     *               | atom.
     * </pre>
     * <p>The argument is passed in skel and display.</p>
     * <p>The result is returned in skel and display.</p>
     * <p>The syntax is recursive.</p>
     *
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public static SkelAtom slashToPackage(Engine en)
            throws EngineMessage {
        Object t = en.skel;
        Display d = en.display;
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SLASH)) {
            SkelCompound temp = (SkelCompound) t;
            en.skel = temp.args[0];
            en.display = d;
            en.deref();
            SkelAtom sa = SpecialQuali.slashToPackage(en);
            en.skel = temp.args[1];
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
            EngineMessage.checkInstantiated(t);
            return CachePackage.getPackage(sa, EngineMessage.castString(t, d), temp.sym, en);
        } else if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 1 &&
                ((SkelCompound) t).sym.fun.equals(PrologReader.OP_SET)) {
            SkelCompound temp = (SkelCompound) t;
            en.skel = temp.args[0];
            en.display = d;
            en.deref();
            SkelAtom sa = SpecialQuali.slashToPackage(en);
            return CachePackage.getPackage(sa, null, temp.sym, en);
        } else {
            EngineMessage.checkInstantiated(t);
            return EngineMessage.castStringWrapped(t, d);
        }
    }

    /**
     * <p>Convert a slash callable to an object.</p>
     * <p>An slash callable has the following syntax.</p>
     * <pre>
     *     object --> package "/" callable
     *              | "{" package "}"
     *              | reference
     *              | callable.
     * </pre>
     * <p>The argument is passed in skel and display.</p>
     * <p>The result is returned in skel and display.</p>
     *
     * @param en   The engine.
     * @param comp The compound flag.
     * @throws EngineMessage Shit happens.
     */
    public static void slashToClass(Engine en, boolean comp)
            throws EngineMessage {
        Object t = en.skel;
        Display d = en.display;
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SLASH)) {
            SkelCompound temp = (SkelCompound) t;
            en.skel = temp.args[0];
            en.display = d;
            en.deref();
            SkelAtom sa = SpecialQuali.slashToPackage(en);
            en.skel = temp.args[1];
            en.display = d;
            en.deref();
            t = en.skel;
            d = en.display;
            if (comp && (t instanceof SkelCompound)) {
                SkelCompound sc2 = (SkelCompound) t;
                en.skel = CacheModule.getModule(sa, sc2.sym.fun, false,
                        temp.sym.scope, en);
            } else if (t instanceof SkelAtom) {
                SkelAtom sa2 = (SkelAtom) t;
                en.skel = CacheModule.getModule(sa, sa2.fun, false,
                        temp.sym.scope, en);
            } else {
                EngineMessage.checkInstantiated(t);
                throw new EngineMessage(EngineMessage.typeError(
                        (comp ? EngineMessage.OP_TYPE_CALLABLE :
                                EngineMessage.OP_TYPE_ATOM), t), d);
            }
        } else if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 1 &&
                ((SkelCompound) t).sym.fun.equals(PrologReader.OP_SET)) {
            SkelCompound temp = (SkelCompound) t;
            en.skel = temp.args[0];
            en.display = d;
            en.deref();
            SkelAtom sa = SpecialQuali.slashToPackage(en);
            en.skel = CacheModule.getModule(sa, null, false,
                    temp.sym.scope, en);
        } else if (!(t instanceof Number) &&
                !(t instanceof AbstractSkel)) {
            /* */
        } else {
            if (comp && (t instanceof SkelCompound)) {
                SkelCompound sc = (SkelCompound) t;
                en.skel = CacheModule.getModule(sc.sym, null, true,
                        sc.sym.scope, en);
            } else if (t instanceof SkelAtom) {
                SkelAtom sa = (SkelAtom) t;
                en.skel = CacheModule.getModule(sa, null, true,
                        sa.scope, en);
            } else {
                EngineMessage.checkInstantiated(t);
                throw new EngineMessage(EngineMessage.typeError(
                        (comp ? EngineMessage.OP_TYPE_CALLABLE :
                                EngineMessage.OP_TYPE_ATOM), t), d);
            }
        }
    }

    /************************************************************/
    /* Callable Colon                                           */
    /************************************************************/

    /**
     * <p>Convert a callable to a colon.</p>
     * <p>A colon callable has the following syntax.</p>
     * <pre>
     *     colon --> atom ":" callable
     *             | term.
     * </pre>
     * <p>The syntax is not recursive.</p>
     *
     * @param t  The callable.
     * @param en The engine.
     * @return The colon callable.
     * @throws EngineMessage Shit happens.
     */
    public static Object callableToColonSkel(Object t, Engine en)
            throws EngineMessage {
        if (t instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) t;
            SkelAtom sa = sc.sym;
            if (SpecialQuali.isQuali(sa.fun)) {
                t = moduleToSlashSkel(sepModule(sa.fun), sa.scope, en);
                Object s = new SkelCompound(
                        new SkelAtom(sepName(sa.fun)),
                        sc.args, sc.vars);

                int m = (sa.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
                SkelAtom sa2 = en.store.foyer.createAtom(OP_COLON, sa.scope, m);
                sa2.setPosition(sa.getPosition());
                return new SkelCompound(sa2, t, s);
            } else {
                return t;
            }
        } else if (t instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) t;
            if (SpecialQuali.isQuali(sa.fun)) {
                t = moduleToSlashSkel(sepModule(sa.fun), sa.scope, en);
                Object s = new SkelAtom(sepName(sa.fun));

                int m = (sa.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
                SkelAtom sa2 = en.store.foyer.createAtom(OP_COLON, sa.scope, m);
                sa2.setPosition(sa.getPosition());
                return new SkelCompound(sa2, t, s);
            } else {
                return t;
            }
        } else {
            return t;
        }
    }

    /**
     * <p>Convert a colon to a callable.</p>
     * <p>A colon callable has the following syntax.</p>
     * <pre>
     *     colon --> module ":" colon
     *             | object "::" colon
     *             | term.
     * </pre>
     * <p>The syntax is recursive.</p>
     * <p>The argument is passed in skel and display.</p>
     * <p>The result is returned in skel and display.</p>
     *
     * @param en The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void colonToCallable(Engine en)
            throws EngineMessage {
        Object t = en.skel;
        Display ref = en.display;
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(OP_COLON)) {
            SkelCompound temp = (SkelCompound) t;
            en.skel = temp.args[0];
            en.display = ref;
            en.deref();
            SpecialQuali.slashToClass(en, false);
            String fun;
            /* reference */
            if (!(en.skel instanceof AbstractSkel) &&
                    !(en.skel instanceof Number)) {
                fun = SpecialProxy.classOrProxyName(en.skel);
                if (fun == null)
                    throw new EngineMessage(EngineMessage.typeError(
                            EngineMessage.OP_TYPE_CLASS, en.skel));
                /* atom */
            } else {
                fun = ((SkelAtom) en.skel).fun;
            }
            en.skel = temp.args[1];
            en.display = ref;
            en.deref();
            SpecialQuali.colonToCallable(en);
            if (en.skel instanceof SkelCompound) {
                SkelCompound sc2 = (SkelCompound) en.skel;
                en.skel = new SkelCompound(CacheFunctor.getFunctor(sc2.sym, fun,
                        temp.sym, en.store), sc2.args, sc2.vars);
            } else if (en.skel instanceof SkelAtom) {
                SkelAtom sa = (SkelAtom) en.skel;
                en.skel = CacheFunctor.getFunctor(sa, fun, temp.sym, en.store);
            } else {
                EngineMessage.checkInstantiated(en.skel);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_CALLABLE,
                        en.skel), en.display);
            }
        } else if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(OP_COLONCOLON)) {
            SkelCompound temp = (SkelCompound) t;
            en.skel = temp.args[0];
            en.display = ref;
            en.deref();
            Object recv = en.skel;
            Display d2 = en.display;
            SpecialQuali.slashToClass(en, true);
            String fun;
            /* reference */
            if (!(en.skel instanceof AbstractSkel) &&
                    !(en.skel instanceof Number)) {
                en.skel = SpecialProxy.refClassOrProxy(en.skel);
                if (en.skel == null)
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_UNKNOWN_PROXY, en.skel), en.display);
                fun = SpecialProxy.classOrProxyName(en.skel);
                if (fun == null)
                    throw new EngineMessage(EngineMessage.typeError(
                            EngineMessage.OP_TYPE_CLASS, en.skel));
                /* atom */
            } else {
                fun = ((SkelAtom) en.skel).fun;
            }
            en.skel = temp.args[1];
            en.display = ref;
            en.deref();
            SpecialQuali.colonToCallable(en);
            if (en.skel instanceof SkelCompound) {
                SkelCompound sc2 = (SkelCompound) en.skel;
                Display d3 = en.display;
                boolean multi = SpecialQuali.prependCount(recv, d2,
                        sc2.args, d3, en);
                en.skel = new SkelCompound(CacheFunctor.getFunctor(sc2.sym, fun,
                        temp.sym, en.store), SpecialQuali.prependAlloc(recv, d2,
                        sc2.args, d3, multi, en));
            } else if (en.skel instanceof SkelAtom) {
                SkelAtom sa = (SkelAtom) en.skel;
                en.skel = new SkelCompound(CacheFunctor.getFunctor(sa, fun,
                        temp.sym, en.store), recv);
                en.display = d2;
            } else {
                EngineMessage.checkInstantiated(en.skel);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_CALLABLE,
                        en.skel), en.display);
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
     *     indicator --> module ":" name "/" length
     *                 | name "/" length.
     * </pre>
     *
     * @param en The engine.
     * @return The length.
     * @throws EngineMessage The indicator is not wellformed.
     */
    public static Integer colonToIndicator(Engine en)
            throws EngineMessage {
        Object t = en.skel;
        Display d = en.display;
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(OP_COLON)) {
            SkelCompound sc = (SkelCompound) t;
            en.skel = sc.args[0];
            en.display = d;
            en.deref();
            SpecialQuali.slashToClass(en, false);
            String fun;
            /* reference */
            if (!(en.skel instanceof AbstractSkel) &&
                    !(en.skel instanceof Number)) {
                fun = SpecialProxy.classOrProxyName(en.skel);
                if (fun == null)
                    throw new EngineMessage(EngineMessage.typeError(
                            EngineMessage.OP_TYPE_CLASS, en.skel));
                /* atom */
            } else {
                fun = ((SkelAtom) en.skel).fun;
            }
            en.skel = sc.args[1];
            en.display = d;
            en.deref();
            Integer arity = SpecialQuali.colonToIndicator(en);
            SkelAtom sa = ((SkelAtom) en.skel);
            en.skel = CacheFunctor.getFunctor(sa, fun, sc.sym, en.store);
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
        if (isQuali(sa.fun)) {
            Object t = moduleToSlashSkel(sepModule(sa.fun), sa.scope, en);
            Object s = new SkelCompound(en.store.foyer.ATOM_SLASH,
                    new SkelAtom(sepName(sa.fun)),
                    Integer.valueOf(arity));

            int m = (sa.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
            SkelAtom sa2 = en.store.foyer.createAtom(OP_COLON, sa.scope, m);
            sa2.setPosition(sa.getPosition());
            return new SkelCompound(sa2, t, s);
        } else {
            return new SkelCompound(en.store.foyer.ATOM_SLASH, sa,
                    Integer.valueOf(arity));
        }
    }

    /*************************************************************/
    /* Nested Paths                                              */
    /*************************************************************/

    /**
     * <p>Separate the outer from the nested path.</p>
     *
     * @param path The nested path.
     * @return The outer.
     */
    public static String sepOuter(String path) {
        return path.substring(0, path.lastIndexOf(SourceLocal.OP_CHAR_SYN));
    }

    /**
     * <p>Separate the inner from the nested path.</p>
     *
     * @param path The nested path.
     * @return The inner.
     */
    public static String sepInner(String path) {
        return path.substring(path.lastIndexOf(SourceLocal.OP_CHAR_SYN) + 1);
    }

    /**
     * <p>Check whether representation is nest.</p>
     *
     * @param path The path.
     * @return True if representation is nest, otherwise false.
     */
    public static boolean isNest(String path) {
        int k = path.lastIndexOf(OP_CHAR_SEG);
        return (path.indexOf(SourceLocal.OP_CHAR_SYN, k + 1) != -1);
    }

    /***************************************************************/
    /* Structured Paths                                            */
    /***************************************************************/

    /**
     * <p>Separate the package from the structured path.</p>
     *
     * @param path The structured path.
     * @return The package.
     */
    public static String sepPack(String path) {
        return path.substring(0, path.lastIndexOf(OP_CHAR_SEG));
    }

    /**
     * <p>Separate the base from the structured path.</p>
     *
     * @param path The structured path.
     * @return The base.
     */
    public static String sepBase(String path) {
        return path.substring(path.lastIndexOf(OP_CHAR_SEG) + 1);
    }

    /**
     * <p>Check whether a path is structured.</p>
     *
     * @param path The path.
     * @return True if the path is structured, otherwise false.
     */
    public static boolean isStruct(String path) {
        return (path.lastIndexOf(OP_CHAR_SEG) != -1);
    }

    /**
     * <p>Compose a structured path from a package and a base.</p>
     *
     * @param pack The package.
     * @param base The base.
     * @return The structured path.
     */
    public static String composeStruct(String pack, String base) {
        StringBuilder buf = new StringBuilder();
        buf.append(pack);
        buf.append(OP_CHAR_SEG);
        buf.append(base);
        return buf.toString();
    }

    /***************************************************************/
    /* Array Paths                                                 */
    /***************************************************************/

    /**
     * <p>Separate the component from the array path.</p>
     *
     * @param path The array path.
     * @return The component.
     */
    public static String sepComp(String path) {
        return path.substring(0, path.length() - Foyer.OP_NIL.length());
    }

    /**
     * <p>Check whether a path is an array.</p>
     *
     * @param path The path.
     * @return True if the path is an array, otherwise false.
     */
    public static boolean isArray(String path) {
        return path.endsWith(Foyer.OP_NIL);
    }

    /**
     * <p>Compose an array path from a component.</p>
     *
     * @param comp The component.
     * @return The array path.
     */
    public static String composeArray(String comp) {
        StringBuilder buf = new StringBuilder();
        buf.append(comp);
        buf.append(Foyer.OP_NIL);
        return buf.toString();
    }

    /***************************************************************/
    /* Qualified Names                                             */
    /***************************************************************/

    /**
     * <p>Separate the module name from the qualified name.</p>
     *
     * @param fun The qualified name.
     * @return The module name.
     */
    public static String sepModule(String fun) {
        return fun.substring(0, fun.lastIndexOf(OP_CHAR_SEP));
    }

    /**
     * <p>Separate the name from the qualified name.</p>
     *
     * @param fun The qualified name.
     * @return The name.
     */
    public static String sepName(String fun) {
        return fun.substring(fun.lastIndexOf(OP_CHAR_SEP) + 1);
    }

    /**
     * <p>Check whether a name is qualified.</p>
     *
     * @param fun The name.
     * @return True if the name is qualified, otherwise false.
     */
    public static boolean isQuali(String fun) {
        return (fun.lastIndexOf(OP_CHAR_SEP) != -1);
    }

    /**
     * <p>Compose a qualified name from a module name and a name.</p>
     *
     * @param mod The module name.
     * @param fun The name.
     * @return The qualified name.
     */
    public static String composeQuali(String mod, String fun) {
        StringBuilder buf = new StringBuilder();
        buf.append(mod);
        buf.append(OP_CHAR_SEP);
        buf.append(fun);
        return buf.toString();
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
        en.skel = t;
        en.display = d;
        en.deref();
        if (!EngineCopy.isGroundSkel(en.skel)) {
            countvar++;
            if (last == Display.DISPLAY_CONST) {
                last = en.display;
            } else if (last != en.display) {
                multi = true;
            }
        }
        for (int i = 0; i < t2args.length; i++) {
            en.skel = t2args[i];
            en.display = d2;
            en.deref();
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
        en.skel = t;
        en.display = d;
        en.deref();
        if (multi && !EngineCopy.isGroundSkel(en.skel)) {
            SkelVar sv = SkelVar.valueOf(countvar);
            countvar++;
            d4.bind[sv.id].bindVar(en.skel, en.display, en);
            en.skel = sv;
        }
        args[0] = en.skel;
        for (int i = 0; i < t2args.length; i++) {
            en.skel = t2args[i];
            en.display = d2;
            en.deref();
            if (multi && !EngineCopy.isGroundSkel(en.skel)) {
                SkelVar sv = SkelVar.valueOf(countvar);
                countvar++;
                d4.bind[sv.id].bindVar(en.skel, en.display, en);
                en.skel = sv;
            }
            args[i + 1] = en.skel;
        }
        en.display = d4;
        return args;
    }

}
