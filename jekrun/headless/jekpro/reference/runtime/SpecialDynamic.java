package jekpro.reference.runtime;

import jekpro.frequent.basic.SpecialProxy;
import jekpro.frequent.experiment.SpecialRef;
import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.builtin.Branch;
import jekpro.model.inter.*;
import jekpro.model.molec.*;
import jekpro.model.pretty.*;
import jekpro.model.rope.Clause;
import jekpro.model.rope.LoadOpts;
import jekpro.model.rope.Operator;
import jekpro.reference.bootload.ForeignPath;
import jekpro.reference.reflect.SpecialOper;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.term.AbstractSkel;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.MapEntry;
import matula.util.data.MapHash;
import matula.util.data.MapHashLink;

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
    private final static int SPECIAL_SYS_ENSURE_GROUP_LOCAL = 2;
    private final static int SPECIAL_CLAUSE = 3;
    private final static int SPECIAL_ASSERTA = 4;
    private final static int SPECIAL_ASSERTZ = 5;
    private final static int SPECIAL_ABOLISH_PREDICATE = 6;
    private final static int SPECIAL_ABOLISH_OPERATOR = 7;
    private final static int SPECIAL_SYS_HAS_CLAUSE = 8;
    private final static int SPECIAL_SYS_LIST_CLAUSE = 9;

    /**
     * <p>Create a predicate special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialDynamic(int i) {
        super(i);
    }

    /**
     * <p>Logically evaluate a term in a list of goals for the first time.</p>
     * <p>The term is passed via the skel and display of the engine.</p>
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
                Display ref = en.display;
                Predicate pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_DEFI);
                SpecialDynamic.defineDynamic(pick, en);
                return true;
            case SPECIAL_SYS_ENSURE_THREAD_LOCAL:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_DEFI);
                SpecialDynamic.defineThreadLocal(pick, en);
                return true;
            case SPECIAL_SYS_ENSURE_GROUP_LOCAL:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_DEFI);
                SpecialDynamic.defineGroupLocal(pick, en);
                return true;
            case SPECIAL_CLAUSE:
                return searchKnowledgebase(
                        AbstractDefined.OPT_CHCK_ASSE, en);
            case SPECIAL_ASSERTA:
                enhanceKnowledgebase(AbstractDefined.OPT_PROM_DYNA |
                        AbstractDefined.OPT_CHCK_ASSE, en);
                return true;
            case SPECIAL_ASSERTZ:
                enhanceKnowledgebase(AbstractDefined.OPT_PROM_DYNA |
                        AbstractDefined.OPT_CHCK_ASSE | AbstractDefined.OPT_ACTI_BOTT, en);
                return true;
            case SPECIAL_ABOLISH_PREDICATE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToPredicateDefined(temp[0], ref, en, 0);
                if (pick == null)
                    return true;
                abolishPred(pick, en);
                return true;
            case SPECIAL_ABOLISH_OPERATOR:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                Operator oper = SpecialOper.operToOperatorDefined(temp[0], ref, en, 0);
                if (oper == null)
                    return true;
                abolishOper(oper, en);
                return true;
            case SPECIAL_SYS_HAS_CLAUSE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;

                pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                if (pick == null)
                    return false;
                if (!(pick.del instanceof AbstractDefined))
                    return false;

                SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(temp[1], ref);
                AbstractSource source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;
                if (pick.getDef(source) == null)
                    return false;

                if (!hasClause((AbstractDefined)pick.del, source, en))
                    return false;
                return true;
            case SPECIAL_SYS_LIST_CLAUSE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                if (pick == null)
                    return false;
                if (!(pick.del instanceof AbstractDefined))
                    return false;

                sa = SpecialUniv.derefAndCastStringWrapped(temp[1], ref);
                source = (sa.scope != null ? sa.scope : en.store.user);
                source = source.getStore().getSource(sa.fun);
                if (source == null)
                    return false;
                if (pick.getDef(source) == null)
                    return false;

                return ((AbstractDefined) pick.del).listFirst(source, temp, ref, en);
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Enhance the knowledge base by a new clause.</p>
     * <p>The term is passed via the engine skel and display.</p>
     * <p>UInderstood flags:</p>
     * <ul>
     * <li><b>OPT_ARGS_ASOP:</b> The term has assert options.</li>
     * <li><b>MASK_OPER_DYNA:</b> Predicate should be dynamic.</li>
     * <li><b>MASK_OPER_THRE:</b> Predicate should be thread_local.</li>
     * <li><b>OPT_ACTI_BOTT:</b> The clause should be added at the end.</li>
     * </ul>
     *
     * @param flags The flags.
     * @param en    The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static void enhanceKnowledgebase(int flags, Engine en)
            throws EngineMessage, EngineException {
        Object[] temp = ((SkelCompound) en.skel).args;
        Display ref = en.display;
        SupervisorCopy ec = en.visor.getCopy();
        if ((flags & AbstractDefined.OPT_ARGS_ASOP) != 0) {
            ec.vars = null;
            ec.anon = null;
            ec.flags = SupervisorCopy.MASK_COPY_SING;
            Object molec = ec.copyTermNew(temp[0], ref);
            MapHash<BindUniv, String> print =
                    SpecialRef.decodeAssertOptions(temp[1], ref, en);
            MapHashLink<String, SkelVar> vars =
                    SupervisorCopy.copyVarsUniv(ec.vars, print);
            MapHashLink<String, SkelVar> anon =
                    SupervisorCopy.copyVarsUniv(ec.anon, print);
            ec.vars = null;
            ec.anon = null;
            Object term = Clause.clauseToHead(molec, en);
            PrologReader.checkSingleton(term, anon, en);
            Clause clause = Clause.determineCompiled(flags, term, molec, en);
            clause.vars = vars;
            clause.assertRef(flags, en);
        } else {
            ec.vars = null;
            ec.flags = 0;
            Object molec = ec.copyTermNew(temp[0], ref);
            ec.vars = null;
            Object term = Clause.clauseToHead(molec, en);
            Clause clause = Clause.determineCompiled(flags, term, molec, en);
            clause.assertRef(flags, en);
        }
    }

    /**
     * <p>Search the knowledge base.</p>
     * <p>The search term is passed via the engine skel and display.</p>
     * <p>The following flags are recognized:</p>
     * <ul>
     * <li><b>OPT_ACTI_RETR:</b> Retract found clauses and one term argument.</li>
     * <li><b>MASK_OPER_CHWR:</b> Generate a write error instead of a read error.</li>
     * <li><b>MASK_OPER_DYNA:</b> Predicate should be accessible.</li>
     * <li><b>OPT_RSLT_CREF:</b> Return clause reference in last argument.</li>
     * <li><b>OPT_RSLT_FRME:</b> Return frame reference in last argument.</li>
     * </ul>
     *
     * @param flags The flags.
     * @param en    The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public static boolean searchKnowledgebase(int flags, Engine en)
            throws EngineMessage, EngineException {
        Object[] temp = ((SkelCompound) en.skel).args;
        Display ref = en.display;
        /* detect term and body */
        SpecialLogic.colonToCallable(temp[0], ref, true, en);
        Object head = en.skel;
        Display refhead = en.display;

        /* check predicate existence and visibility */
        CachePredicate cp = StackElement.callableToPredicate(head, en);
        if (cp == null || (cp.flags & CachePredicate.MASK_PRED_VISI) == 0) {
            EngineMessage.checkCallable(head, refhead);
            return false;
        }
        Predicate pick = cp.pick;
        /* check predicate modify/access */
        AbstractDelegate fun = pick.del;
        if ((flags & AbstractDefined.OPT_ACTI_WRIT) != 0) {
            switch (flags & AbstractDefined.OPT_CHCK_MASK) {
                case AbstractDefined.OPT_CHCK_DEFN:
                    AbstractDefined.checkDefinedWrite(fun, pick);
                    break;
                case AbstractDefined.OPT_CHCK_ASSE:
                    AbstractDefined.checkAssertableWrite(fun, pick);
                    break;
                default:
                    throw new IllegalArgumentException("illegal check");
            }
        } else {
            switch (flags & AbstractDefined.OPT_CHCK_MASK) {
                case AbstractDefined.OPT_CHCK_DEFN:
                    AbstractDefined.checkDefinedRead(fun, pick);
                    break;
                case AbstractDefined.OPT_CHCK_ASSE:
                    AbstractDefined.checkAssertableRead(fun, pick);
                    break;
                default:
                    throw new IllegalArgumentException("illegal check");
            }
        }
        /* find rope */
        return ((AbstractDefined) fun).searchFirst(head, refhead, temp, ref, flags, en);
    }


    /**
     * <p>Check whether the predicate has some clauses.</p>
     *
     * @param def   The abstract defined.
     * @param source The scope.
     * @param en     The engine.
     * @return True if the predicate has some clauses, otherwise false.
     * @throws EngineMessage Shit happens.
     */
    public static boolean hasClause(AbstractDefined def, AbstractSource source,
                                    Engine en)
            throws EngineMessage {
        Clause[] list = def.listClauses(en);
        for (int i = 0; i < list.length; i++) {
            Clause clause = list[i];
            SkelAtom sa = StackElement.callableToName(clause.head);
            if (source != sa.scope)
                continue;
            return true;
        }
        return false;
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
        AbstractSource src = pick.getSource();
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_COERCE,
                EngineMessage.OP_PERMISSION_PROCEDURE,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src.getFullName(), src.getStore().user)));
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
        AbstractSource src = pick.getSource();
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_COERCE,
                EngineMessage.OP_PERMISSION_PROCEDURE,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src.getFullName(), src.getStore().user)));
    }

    /**
     * <p>Define thread local for a predicate.</p>
     *
     * @param pick The predicate.
     * @param en   The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void defineGroupLocal(Predicate pick, Engine en)
            throws EngineMessage {
        AbstractDelegate fun = AbstractDefined.promoteGroupLocal(pick, en.store);
        if ((fun.subflags & AbstractDefined.MASK_DEFI_GRLC) != 0)
            return;
        AbstractSource src = pick.getSource();
        throw new EngineMessage(EngineMessage.permissionError(
                EngineMessage.OP_PERMISSION_COERCE,
                EngineMessage.OP_PERMISSION_PROCEDURE,
                SpecialPred.indicatorToColonSkel(pick.getFun(), pick.getArity(),
                        src.getFullName(), src.getStore().user)));
    }

    /*************************************************************/
    /* Slash Notation Decode                                     */
    /*************************************************************/

    /**
     * <p>Convert a slash module or receiver to an object.</p>
     *
     * @param t     The slash skeleton.
     * @param flags The flags.
     * @param en    The engine.
     * @return The module or class, or null.
     * @throws EngineMessage Shit happens.
     * @see EvaluableLogic#slashToClass
     */
    public static Object slashToClassSkel(Object t,
                                          int flags,
                                          Engine en)
            throws EngineMessage {
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SLASH)) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa = slashToPackageSkel(temp.args[0], flags);
            if (sa == null)
                return null;
            t = temp.args[1];
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
                    Display d = AbstractSkel.createDisplay(t);
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
            SkelAtom sa = slashToPackageSkel(temp.args[0],
                    (flags | CacheModule.MASK_MODULE_ARRC));
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
                    Display d = AbstractSkel.createDisplay(t);
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
     * <p>Convert a slash to a package.</p>
     *
     * @param t     The slash skeleton.
     * @param flags The flags.
     * @return The package, or null.
     * @throws EngineMessage Shit happens.
     * @see EvaluableLogic#slashToPackage
     */
    private static SkelAtom slashToPackageSkel(Object t,
                                               int flags)
            throws EngineMessage {
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SLASH)) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa = slashToPackageSkel(temp.args[0],
                    (flags & ~CacheModule.MASK_MODULE_ARRC));
            if (sa == null)
                return null;
            t = temp.args[1];
            if (t instanceof SkelAtom)
                return CachePackage.getPackage(sa, ((SkelAtom) t).fun);
            if ((flags & CacheModule.MASK_MODULE_NERR) == 0) {
                EngineMessage.checkInstantiated(t);
                Display d = AbstractSkel.createDisplay(t);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_ATOM, t), d);
            } else {
                return null;
            }
        } else if ((flags & CacheModule.MASK_MODULE_ARRC) != 0 && (t instanceof SkelCompound) &&
                ((SkelCompound) t).args.length == 1 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SET)) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa = slashToPackageSkel(temp.args[0], flags);
            if (sa == null)
                return null;
            return CachePackage.getPackage(sa, null);
        } else {
            if (t instanceof SkelAtom)
                return (SkelAtom) t;
            if ((flags & CacheModule.MASK_MODULE_NERR) == 0) {
                EngineMessage.checkInstantiated(t);
                Display d = AbstractSkel.createDisplay(t);
                throw new EngineMessage(EngineMessage.domainError(
                        ((flags & CacheModule.MASK_MODULE_ARRC) != 0 ? EngineMessage.OP_DOMAIN_ARRAY :
                                EngineMessage.OP_DOMAIN_PACKAGE), t), d);
            } else {
                return null;
            }
        }
    }

    /*************************************************************/
    /* Slash Notation Encode                                     */
    /*************************************************************/

    /**
     * <p>Convert a module to a slash.</p>
     *
     * @param fun The module.
     * @param src The call-site, non null.
     * @return The skeleton.
     */
    public static Object moduleToSlashSkel(String fun,
                                           AbstractSource src)
            throws EngineMessage {
        try {
            /* shorten module name */
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
     * @param scope The call-site, non null.
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
     * @see SpecialLogic#colonToCallable
     */
    public static Object colonToCallableSkel(Object t, Engine en)
            throws EngineMessage {
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(EvaluableLogic.OP_COLON)) {
            SkelCompound temp = (SkelCompound) t;
            Object mod = slashToClassSkel(temp.args[0], 0, en);
            if (!(mod instanceof AbstractSkel) &&
                    !(mod instanceof Number)) {
                /* reference */
                mod = SpecialProxy.classOrProxyName(mod, en);
                if (mod == null) {
                    Display d = AbstractSkel.createDisplay(temp.args[0]);
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_CLASS, temp.args[0]), d);
                }
            } else {
                /* atom */
            }
            t = colonToCallableSkel(temp.args[1], en);
            if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                SkelAtom sa = CacheFunctor.getModFunc(sc.sym,
                        (SkelAtom) mod, temp.sym, en);
                SkelCompound sc2 = new SkelCompound(sc.args, sa);
                sc2.var = sc.var;
                t = sc2;
            } else if (t instanceof SkelAtom) {
                SkelAtom sa = (SkelAtom) t;
                t = CacheFunctor.getModFunc(sa, (SkelAtom) mod, temp.sym, en);
            } else {
                EngineMessage.checkInstantiated(t);
                Display d = AbstractSkel.createDisplay(t);
                throw new EngineMessage(EngineMessage.typeError(
                        EngineMessage.OP_TYPE_CALLABLE, t), d);
            }
        } else if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(EvaluableLogic.OP_COLONCOLON)) {
            SkelCompound temp = (SkelCompound) t;
            Object mod = slashToClassSkel(temp.args[0], CacheModule.MASK_MODULE_CMPD, en);
            if (!(mod instanceof AbstractSkel) &&
                    !(mod instanceof Number)) {
                /* reference */
                mod = SpecialProxy.refClassOrProxy(mod);
                if (mod == null) {
                    Display d = AbstractSkel.createDisplay(temp.args[0]);
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_UNKNOWN_PROXY, temp.args[0]), d);
                }
                mod = SpecialProxy.classOrProxyName(mod, en);
                if (mod == null) {
                    Display d = AbstractSkel.createDisplay(temp.args[0]);
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
                t = new SkelCompound(CacheFunctor.getModFunc(sc2.sym,
                        (SkelAtom) mod, temp.sym, en), newargs);
            } else if (t instanceof SkelAtom) {
                SkelAtom sa = (SkelAtom) t;
                Object recv = temp.args[0];
                t = new SkelCompound(CacheFunctor.getModFunc(sa, (SkelAtom) mod,
                        temp.sym, en), recv);
            } else {
                EngineMessage.checkInstantiated(t);
                Display d = AbstractSkel.createDisplay(t);
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
                Object recv = slashToClassSkel(t, CacheModule.MASK_MODULE_CMPD |
                        CacheModule.MASK_MODULE_NERR, en);
                if (recv != null)
                    recv = objToAtom(recv, t, en);
                SkelAtom sa3 = new SkelAtom(sa.fun, sa.scope);
                SkelAtom mod = ((SkelAtomQuali) sa).getModule();
                if (recv == null || !mod.fun.equals(((SkelAtom) recv).fun) ||
                        mod.scope != ((SkelAtom) recv).scope) {
                    AbstractSource src = (mod.scope != null ? mod.scope : en.store.user);
                    t = moduleToSlashSkel(mod.fun, src);
                    SkelCompound s = new SkelCompound(temp.args, sa3);
                    s.var = temp.var;

                    int m = (sa.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
                    SkelAtom sa2 = en.store.foyer.createAtom(EvaluableLogic.OP_COLON, sa.scope, m);
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
                    SkelAtom sa2 = en.store.foyer.createAtom(EvaluableLogic.OP_COLONCOLON, sa.scope, m);
                    sa2.setPosition(sa.getPosition());
                    return new SkelCompound(sa2, t, s);
                }
            }
        } else if (t instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) t;
            if (sa instanceof SkelAtomQuali) {
                SkelAtom mod = ((SkelAtomQuali) sa).getModule();
                AbstractSource src = (mod.scope != null ? mod.scope : en.store.user);
                t = moduleToSlashSkel(mod.fun, src);
                Object s = new SkelAtom(sa.fun, sa.scope);

                int m = (sa.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
                SkelAtom sa2 = en.store.foyer.createAtom(EvaluableLogic.OP_COLON, sa.scope, m);
                sa2.setPosition(sa.getPosition());
                return new SkelCompound(sa2, t, s);
            }
        }
        return t;
    }

    /**
     * <p>Convert a callable to a colon.</p>
     *
     * @param t   The callable.
     * @param mod The full name.
     * @param en  The engine.
     * @return The colon callable.
     * @throws EngineMessage Shit happens.
     */
    public static Object callableToColonSkel(Object t,
                                             String mod,
                                             Engine en)
            throws EngineMessage {
        AbstractSource scope = en.store.user;
        if (t instanceof SkelCompound) {
            SkelCompound temp = (SkelCompound) t;
            SkelAtom sa = temp.sym;
            if (!Branch.OP_USER.equals(mod)) {
                t = temp.args[0];
                Object recv = slashToClassSkel(t, CacheModule.MASK_MODULE_CMPD |
                        CacheModule.MASK_MODULE_NERR, en);
                if (recv != null)
                    recv = objToAtom(recv, t, en);
                SkelAtom sa3 = new SkelAtom(sa.fun);
                if (recv == null || !mod.equals(((SkelAtom) recv).fun) ||
                        scope != ((SkelAtom) recv).scope) {
                    t = moduleToSlashSkel(mod, scope);
                    SkelCompound s = new SkelCompound(temp.args, sa3);
                    s.var = temp.var;

                    int m = (sa.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
                    SkelAtom sa2 = en.store.foyer.createAtom(EvaluableLogic.OP_COLON, sa.scope, m);
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
                    SkelAtom sa2 = en.store.foyer.createAtom(EvaluableLogic.OP_COLONCOLON, sa.scope, m);
                    sa2.setPosition(sa.getPosition());
                    return new SkelCompound(sa2, t, s);
                }
            }
        } else if (t instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) t;
            if (!Branch.OP_USER.equals(mod)) {
                t = moduleToSlashSkel(mod, scope);
                Object s = new SkelAtom(sa.fun);

                int m = (sa.getPosition() != null ? SkelAtom.MASK_ATOM_POSI : 0);
                SkelAtom sa2 = en.store.foyer.createAtom(EvaluableLogic.OP_COLON, sa.scope, m);
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
                Display d = AbstractSkel.createDisplay(t);
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_UNKNOWN_PROXY, t), d);
            }
            mod = SpecialProxy.classOrProxyName(mod, en);
            if (mod == null) {
                Display d = AbstractSkel.createDisplay(t);
                throw new EngineMessage(EngineMessage.domainError(
                        EngineMessage.OP_DOMAIN_CLASS, t), d);
            }
        } else {
            /* atom */
        }
        return (SkelAtom) mod;
    }

}
