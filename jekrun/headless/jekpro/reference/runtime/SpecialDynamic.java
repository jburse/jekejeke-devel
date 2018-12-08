package jekpro.reference.runtime;

import jekpro.frequent.basic.SpecialProxy;
import jekpro.frequent.standard.EngineCopy;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.pretty.Foyer;
import jekpro.model.rope.LoadOpts;
import jekpro.model.rope.Operator;
import jekpro.reference.bootload.ForeignPath;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.MapEntry;

import java.io.IOException;

/**
 * <p>Provides built-in predicates for dynamic predicates.</p>
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
public final class SpecialDynamic extends AbstractSpecial {
    private final static int SPECIAL_SYS_ENSURE_SHARED_DYNAMIC = 0;
    private final static int SPECIAL_SYS_ENSURE_THREAD_LOCAL = 1;
    private final static int SPECIAL_CLAUSE = 2;
    private final static int SPECIAL_ASSERTA = 3;
    private final static int SPECIAL_ASSERTZ = 4;
    private final static int SPECIAL_ABOLISH_PREDICATE = 5;
    private final static int SPECIAL_ABOLISH_OPER = 6;

    /**
     * <p>Create a predicate special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialDynamic(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineException Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        switch (id) {
            case SPECIAL_SYS_ENSURE_SHARED_DYNAMIC:
                Object[] temp = ((SkelCompound) en.skel).args;
                BindCount[] ref = en.display;
                Predicate pick = Predicate.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_CRTE);
                SpecialDynamic.defineDynamic(pick, en);
                return en.getNextRaw();
            case SPECIAL_SYS_ENSURE_THREAD_LOCAL:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = Predicate.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_CRTE);
                SpecialDynamic.defineThreadLocal(pick, en);
                return en.getNextRaw();
            case SPECIAL_CLAUSE:
                return AbstractDefined.searchKnowledgebase(AbstractDefined.OPT_CHCK_ASSE, en);
            case SPECIAL_ASSERTA:
                AbstractDefined.enhanceKnowledgebase(AbstractDefined.OPT_PROM_DYNA |
                        AbstractDefined.OPT_CHCK_ASSE, en);
                return en.getNextRaw();
            case SPECIAL_ASSERTZ:
                AbstractDefined.enhanceKnowledgebase(AbstractDefined.OPT_PROM_DYNA |
                        AbstractDefined.OPT_CHCK_ASSE |
                        AbstractDefined.OPT_ACTI_BOTT, en);
                return en.getNextRaw();
            case SPECIAL_ABOLISH_PREDICATE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = Predicate.indicatorToPredicateDefined(temp[0], ref, en, 0);
                if (pick == null)
                    return en.getNextRaw();
                abolishPred(pick, en);
                return en.getNextRaw();
            case SPECIAL_ABOLISH_OPER:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                Operator oper = Operator.operToOperatorDefined(temp[0], ref, en, false);
                if (oper == null)
                    return en.getNextRaw();
                abolishOper(oper, en);
                return en.getNextRaw();
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /*************************************************************/
    /* Undefine Operation                                        */
    /*************************************************************/

    /**
     * <p>Abolish a predicate.</p>
     *
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void abolishPred(Predicate pick, Engine en)
            throws EngineMessage {
        MapEntry<AbstractSource, Integer>[] snapshot = pick.snapshotDefs();
        for (int i = 0; i < snapshot.length; i++) {
            MapEntry<AbstractSource, Integer> usage = snapshot[i];
            AbstractSource scope = usage.key;
            if (scope.getStore() != en.store)
                continue;
            if (scope.getBranch() != null)
                continue;
            pick.clearPredicate(scope);
        }
    }

    /**
     * <p>Abolish an operator.</p>
     *
     * @param oper The operator.
     * @param en   The engine.
     */
    private static void abolishOper(Operator oper, Engine en) {
        AbstractSource scope = oper.getScope();
        if (scope.getStore() != en.store)
            return;
        if (scope.getBranch() != null)
            return;
        oper.clearOper(scope);
    }

    /*************************************************************/
    /* Predicate Promotion                                       */
    /*************************************************************/

    /**
     * <p>Define predicate as dynamic.</p>
     *
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void defineDynamic(Predicate pick, Engine en)
            throws EngineMessage {
        AbstractDelegate fun = AbstractDefined.promoteDynamic(pick, en.store);
        if ((fun.subflags & AbstractDefined.MASK_DEFI_DYNA) != 0)
            return;
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_COERCE,
                EngineMessage.OP_PERMISSION_PROCEDURE,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /**
     * <p>Define thread local for a predicate.</p>
     *
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void defineThreadLocal(Predicate pick, Engine en)
            throws EngineMessage {
        AbstractDelegate fun = AbstractDefined.promoteThreadLocal(pick, en.store);
        if ((fun.subflags & AbstractDefined.MASK_DEFI_THLC) != 0)
            return;
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_COERCE,
                EngineMessage.OP_PERMISSION_PROCEDURE,
                SpecialQuali.indicatorToColonSkel(
                        pick.getFun(), pick.getSource().getStore().user,
                        pick.getArity(), en)));
    }

    /*************************************************************/
    /* Slash Notation Decode                                     */
    /*************************************************************/

    /**
     * <p>Convert a slash module or receiver to an object.</p>
     *
     * @param t    The slash skeleton.
     * @param comp The compound flag.
     * @param err  The error flag.
     * @param en   The engine.
     * @return The module or class, or null.
     * @throws EngineMessage Shit happens.
     * @see SpecialQuali#slashToClass
     */
    public static Object slashToClassSkel(Object t,
                                          boolean comp,
                                          boolean err,
                                          Engine en)
            throws EngineMessage {
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SLASH)) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa = slashToPackageSkel(temp.args[0], false, err);
            if (sa == null)
                return null;
            t = temp.args[1];
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
                    BindCount[] d = createDisplay(t);
                    throw new EngineMessage(EngineMessage.typeError(
                            (comp ? EngineMessage.OP_TYPE_CALLABLE :
                                    EngineMessage.OP_TYPE_ATOM), t), d);
                } else {
                    return null;
                }
            }
        } else if (!comp && t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 1 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SET)) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa = slashToPackageSkel(temp.args[0], true, err);
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
                    BindCount[] d = createDisplay(t);
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
     * <p>Convert a slash to a package.</p>
     *
     * @param t   The slash skeleton.
     * @param set The set flag.
     * @param err The error flag.
     * @return The package, or null.
     * @throws EngineMessage Shit happens.
     * @see SpecialQuali#slashToPackage
     */
    private static SkelAtom slashToPackageSkel(Object t,
                                               boolean set,
                                               boolean err)
            throws EngineMessage {
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SLASH)) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa = slashToPackageSkel(temp.args[0], false, err);
            if (sa == null)
                return null;
            t = temp.args[1];
            if (t instanceof SkelAtom)
                return CachePackage.getPackage(sa, ((SkelAtom) t).fun);
            if (err) {
                EngineMessage.checkInstantiated(t);
                BindCount[] d = createDisplay(t);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_ATOM, t), d);
            } else {
                return null;
            }
        } else if (set && (t instanceof SkelCompound) &&
                ((SkelCompound) t).args.length == 1 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SET)) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa = slashToPackageSkel(temp.args[0], true, err);
            if (sa == null)
                return null;
            return CachePackage.getPackage(sa, null);
        } else {
            if (t instanceof SkelAtom)
                return (SkelAtom) t;
            if (err) {
                EngineMessage.checkInstantiated(t);
                BindCount[] d = createDisplay(t);
                throw new EngineMessage(EngineMessage.domainError(
                        (set ? EngineMessage.OP_DOMAIN_ARRAY :
                                EngineMessage.OP_DOMAIN_PACKAGE), t), d);
            } else {
                return null;
            }
        }
    }

    /**
     * <p>Create a display for an error.</p>
     *
     * @param t The skeleton.
     * @return The display.
     */
    public static BindCount[] createDisplay(Object t) {
        int size = EngineCopy.displaySize(t);
        return (size != 0 ? BindCount.newBind(size) : BindCount.DISPLAY_CONST);
    }

    /*************************************************************/
    /* Slash Notation Encode                                     */
    /*************************************************************/

    /**
     * <p>Convert a module to a slash.</p>
     *
     * @param fun   The module.
     * @param scope The call-site, not null.
     * @param en    The engine.
     * @return The skeleton.
     */
    public static Object moduleToSlashSkel(String fun,
                                           AbstractSource scope,
                                           Engine en)
            throws EngineMessage {
        try {
            /* shorten module name */
            AbstractSource src = (scope != null ? scope : en.store.user);
            fun = fun.replace(CachePackage.OP_CHAR_SEG, CacheModule.OP_CHAR_OS);
            Object res = CacheModule.unfindPrefix(fun, src, ForeignPath.MASK_MODL_AUTO);

            if (res instanceof SkelAtom) {
                fun = ((SkelAtom) res).fun;
            } else if (res instanceof SkelCompound &&
                    ((SkelCompound) res).args.length == 1 &&
                    ((SkelCompound) res).sym.fun.equals(LoadOpts.OP_PREFIX_VERBATIM)) {
                SkelCompound sc = (SkelCompound) res;
                fun = ((SkelAtom) sc.args[0]).fun;
            } else {
                throw new IllegalArgumentException("illegal spec");
            }

            fun = fun.replace(CacheModule.OP_CHAR_OS, CachePackage.OP_CHAR_SEG);

            return packageToSlashSkel(fun, src);
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>Convert a package to a slash.</p>
     *
     * @param fun   The package.
     * @param scope The call-site, not null.
     * @return The skeleton.
     */
    public static Object packageToSlashSkel(String fun, AbstractSource scope) {
        if (CachePackage.isArray(fun)) {
            return new SkelCompound(new SkelAtom(Foyer.OP_SET, scope),
                    packageToSlashSkel(CachePackage.sepComp(fun), null));
        } else if (CachePackage.isStruct(fun)) {
            return new SkelCompound(new SkelAtom(Foyer.OP_SLASH, scope),
                    packageToSlashSkel(CachePackage.sepPack(fun), null),
                    new SkelAtom(CachePackage.sepBase(fun)));
        } else {
            return new SkelAtom(fun, scope);
        }
    }

    /*************************************************************/
    /* Colon Notation Decode                                     */
    /*************************************************************/

    /**
     * <p>Convert a colon to a callable.</p>
     * <p>A qualified callable has the following syntax.</p>
     * <pre>
     *     colon --> module ":" colon
     *             | receiver "::" colon
     *             | term.
     * </pre>
     *
     * @param t  The input skeleton.
     * @param en The engine.
     * @return The output skeleton.
     * @throws EngineMessage Shit happens.
     * @see SpecialQuali#colonToCallable
     */
    public static Object colonToCallableSkel(Object t, Engine en)
            throws EngineMessage {
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(SpecialQuali.OP_COLON)) {
            SkelCompound temp = (SkelCompound) t;
            Object mod = slashToClassSkel(temp.args[0], false, true, en);
            if (!(mod instanceof AbstractSkel) &&
                    !(mod instanceof Number)) {
                /* reference */
                mod = SpecialProxy.classOrProxyName(mod, en);
                if (mod == null) {
                    BindCount[] d = createDisplay(temp.args[0]);
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_CLASS, temp.args[0]), d);
                }
            } else {
                /* atom */
            }
            t = colonToCallableSkel(temp.args[1], en);
            if (t instanceof SkelCompound) {
                SkelCompound sc2 = (SkelCompound) t;
                t = new SkelCompound(CacheFunctor.getFunctor(sc2.sym,
                        (SkelAtom) mod, temp.sym, en), sc2.args, sc2.var);
            } else if (t instanceof SkelAtom) {
                SkelAtom sa = (SkelAtom) t;
                t = CacheFunctor.getFunctor(sa, (SkelAtom) mod, temp.sym, en);
            } else {
                EngineMessage.checkInstantiated(t);
                BindCount[] d = createDisplay(t);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_CALLABLE, t), d);
            }
        } else if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(SpecialQuali.OP_COLONCOLON)) {
            SkelCompound temp = (SkelCompound) t;
            Object mod = slashToClassSkel(temp.args[0], true, true, en);
            if (!(mod instanceof AbstractSkel) &&
                    !(mod instanceof Number)) {
                /* reference */
                mod = SpecialProxy.refClassOrProxy(mod);
                if (mod == null) {
                    BindCount[] d = createDisplay(temp.args[0]);
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_UNKNOWN_PROXY, temp.args[0]), d);
                }
                mod = SpecialProxy.classOrProxyName(mod, en);
                if (mod == null) {
                    BindCount[] d = createDisplay(temp.args[0]);
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_CLASS, temp.args[0]), d);
                }
            } else {
                /* atom */
            }
            t = colonToCallableSkel(temp.args[1], en);
            if (t instanceof SkelCompound) {
                SkelCompound sc2 = (SkelCompound) t;
                Object recv = temp.args[0];
                Object[] newargs = new Object[sc2.args.length + 1];
                newargs[0] = recv;
                System.arraycopy(sc2.args, 0, newargs, 1, sc2.args.length);
                t = new SkelCompound(CacheFunctor.getFunctor(sc2.sym,
                        (SkelAtom) mod, temp.sym, en), newargs);
            } else if (t instanceof SkelAtom) {
                SkelAtom sa = (SkelAtom) t;
                Object recv = temp.args[0];
                t = new SkelCompound(CacheFunctor.getFunctor(sa, (SkelAtom) mod,
                        temp.sym, en), recv);
            } else {
                EngineMessage.checkInstantiated(t);
                BindCount[] d = createDisplay(t);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_CALLABLE, t), d);
            }
        }
        return t;
    }

    /*************************************************************/
    /* Colon Notation Encode                                     */
    /*************************************************************/


    /**
     * <p>Convert a callable to a colon.</p>
     *
     * @param t  The callable.
     * @param en The engine.
     * @return The colon callable.
     * @throws EngineMessage Shit happens.
     */
    public static Object callableToColonSkel(Object t, Engine en)
            throws EngineMessage {
        if (t instanceof SkelCompound) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa = temp.sym;
            if (sa instanceof SkelAtomQuali) {
                t = temp.args[0];
                Object recv = slashToClassSkel(t, true, false, en);
                if (recv != null)
                    recv = objToAtom(recv, t, en);
                SkelAtom sa3 = new SkelAtom(CacheFunctor.sepName(sa.fun));
                SkelAtom mod = ((SkelAtomQuali) sa).getModule();
                if (recv == null || !mod.fun.equals(((SkelAtom) recv).fun) ||
                        mod.scope != ((SkelAtom) recv).scope) {
                    t = moduleToSlashSkel(mod.fun, mod.scope, en);
                    Object s = new SkelCompound(sa3, temp.args, temp.var);

                    int m = (sa.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
                    SkelAtom sa2 = en.store.foyer.createAtom(SpecialQuali.OP_COLON, sa.scope, m);
                    sa2.setPosition(sa.getPosition());
                    return new SkelCompound(sa2, t, s);

                } else {
                    t = temp.args[0];
                    Object s;
                    if (temp.args.length > 1) {
                        Object[] newargs = new Object[temp.args.length - 1];
                        System.arraycopy(temp.args, 1, newargs, 0, newargs.length);
                        s = new SkelCompound(sa3, newargs);
                    } else {
                        s = sa3;
                    }
                    int m = (sa.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
                    SkelAtom sa2 = en.store.foyer.createAtom(SpecialQuali.OP_COLONCOLON, sa.scope, m);
                    sa2.setPosition(sa.getPosition());
                    return new SkelCompound(sa2, t, s);
                }
            }
        } else if (t instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) t;
            if (sa instanceof SkelAtomQuali) {
                SkelAtom mod = ((SkelAtomQuali) sa).getModule();
                t = moduleToSlashSkel(mod.fun, mod.scope, en);
                Object s = new SkelAtom(CacheFunctor.sepName(sa.fun));

                int m = (sa.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
                SkelAtom sa2 = en.store.foyer.createAtom(SpecialQuali.OP_COLON, sa.scope, m);
                sa2.setPosition(sa.getPosition());
                return new SkelCompound(sa2, t, s);
            }
        }
        return t;
    }

    /**
     * <p>Convert a callable to a colon.</p>
     *
     * @param t     The callable.
     * @param scope The scope.
     * @param en    The engine.
     * @return The colon callable.
     * @throws EngineMessage Shit happens.
     */
    public static Object callableToColonSkel(Object t,
                                             AbstractSource scope,
                                             Engine en)
            throws EngineMessage {
        if (t instanceof SkelCompound) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa = temp.sym;
            if (CacheFunctor.isQuali(sa.fun)) {
                t = temp.args[0];
                Object recv = slashToClassSkel(t, true, false, en);
                if (recv != null)
                    recv = objToAtom(recv, t, en);
                SkelAtom sa3 = new SkelAtom(CacheFunctor.sepName(sa.fun));
                String mod = CacheFunctor.sepModule(sa.fun);
                if (recv == null || !mod.equals(((SkelAtom) recv).fun) ||
                        scope != ((SkelAtom) recv).scope) {
                    t = moduleToSlashSkel(mod, scope, en);
                    Object s = new SkelCompound(sa3, temp.args, temp.var);

                    int m = (sa.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
                    SkelAtom sa2 = en.store.foyer.createAtom(SpecialQuali.OP_COLON, sa.scope, m);
                    sa2.setPosition(sa.getPosition());
                    return new SkelCompound(sa2, t, s);

                } else {
                    t = temp.args[0];
                    Object s;
                    if (temp.args.length > 1) {
                        Object[] newargs = new Object[temp.args.length - 1];
                        System.arraycopy(temp.args, 1, newargs, 0, newargs.length);
                        s = new SkelCompound(sa3, newargs);
                    } else {
                        s = sa3;
                    }
                    int m = (sa.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
                    SkelAtom sa2 = en.store.foyer.createAtom(SpecialQuali.OP_COLONCOLON, sa.scope, m);
                    sa2.setPosition(sa.getPosition());
                    return new SkelCompound(sa2, t, s);
                }
            }
        } else if (t instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) t;
            if (CacheFunctor.isQuali(sa.fun)) {
                String mod = CacheFunctor.sepModule(sa.fun);
                t = moduleToSlashSkel(mod, scope, en);
                Object s = new SkelAtom(CacheFunctor.sepName(sa.fun));

                int m = (sa.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
                SkelAtom sa2 = en.store.foyer.createAtom(SpecialQuali.OP_COLON, sa.scope, m);
                sa2.setPosition(sa.getPosition());
                return new SkelCompound(sa2, t, s);
            }
        }
        return t;
    }

    /**
     * <p>Retrieve the module name.</p>
     *
     * @param mod The object.
     * @param t   The slash skeleton.
     * @param en  The engine.
     * @return The nodule name.
     * @throws EngineMessage Shit happens.
     */
    private static SkelAtom objToAtom(Object mod, Object t,
                                      Engine en)
            throws EngineMessage {
        if (!(mod instanceof AbstractSkel) &&
                !(mod instanceof Number)) {
            /* reference */
            mod = SpecialProxy.refClassOrProxy(mod);
            if (mod == null) {
                BindCount[] d = createDisplay(t);
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_UNKNOWN_PROXY, t), d);
            }
            mod = SpecialProxy.classOrProxyName(mod, en);
            if (mod == null) {
                BindCount[] d = createDisplay(t);
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_CLASS, t), d);
            }
        } else {
            /* atom */
        }
        return (SkelAtom) mod;
    }

}