package jekpro.model.builtin;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Operator;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.bootload.ForeignPath;
import jekpro.reference.reflect.SpecialOper;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.reflect.SpecialSource;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.foreign.LookupBinary;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.system.AbstractRuntime;

import java.io.IOException;
import java.lang.reflect.Constructor;

/**
 * <p>Provides built-in predicates for the module special.</p>
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
public final class SpecialSpecial extends AbstractSpecial {
    final static String OP_SET_PREDICATE_PROPERTY = "set_predicate_property";

    public final static String OP_NAME_CONSTRUCTOR = "new";

    public final static Class[] SIG_INT = new Class[]{Integer.TYPE};

    private final static int SPECIAL_SYS_CONTEXT_PROPERTY = 0;
    private final static int SPECIAL_SET_SOURCE_PROPERTY = 1;
    private final static int SPECIAL_RESET_SOURCE_PROPERTY = 2;
    private final static int SPECIAL_SYS_OP = 3;
    private final static int SPECIAL_SET_OPER_PROPERTY = 4;
    private final static int SPECIAL_SET_PREDICATE_PROPERTY = 5;
    private final static int SPECIAL_RESET_PREDICATE_PROPERTY = 6;
    private final static int SPECIAL_SYS_SPECIAL = 7;
    private final static int SPECIAL_SYS_CHECK_STYLE_PREDICATE = 8;
    private final static int SPECIAL_SYS_CHECK_STYLE_OPER = 9;

    /**
     * <p>Create a special special.</p>
     *
     * @param i The id of the special.
     */
    private SpecialSpecial(int i) {
        super(i);
    }

    /**
     * Register the special specials.
     *
     * @param scope The scope.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    public static void registerSpecials(AbstractSource scope, Engine en)
            throws EngineMessage, EngineException {
        SpecialSpecial.registerProvable(new SkelAtom("sys_context_property", scope), 2,
                new SpecialSpecial(SPECIAL_SYS_CONTEXT_PROPERTY), en);
        SpecialSpecial.registerProvable(new SkelAtom("set_source_property", scope), 2,
                new SpecialSpecial(SPECIAL_SET_SOURCE_PROPERTY), en);
        SpecialSpecial.registerProvable(new SkelAtom("reset_source_property", scope), 2,
                new SpecialSpecial(SPECIAL_RESET_SOURCE_PROPERTY), en);
        SpecialSpecial.registerProvable(new SkelAtom("sys_op", scope), 3,
                new SpecialSpecial(SPECIAL_SYS_OP), en);
        SpecialSpecial.registerProvable(new SkelAtom("set_oper_property", scope), 2,
                new SpecialSpecial(SPECIAL_SET_OPER_PROPERTY), en);
        SpecialSpecial.registerProvable(new SkelAtom(OP_SET_PREDICATE_PROPERTY, scope), 2,
                new SpecialSpecial(SPECIAL_SET_PREDICATE_PROPERTY), en);
        SpecialSpecial.registerProvable(new SkelAtom("reset_predicate_property", scope), 2,
                new SpecialSpecial(SPECIAL_RESET_PREDICATE_PROPERTY), en);
        SpecialSpecial.registerProvable(new SkelAtom("sys_special", scope), 3,
                new SpecialSpecial(SPECIAL_SYS_SPECIAL), en);
        SpecialSpecial.registerProvable(new SkelAtom("sys_check_style_predicate", scope), 1,
                new SpecialSpecial(SPECIAL_SYS_CHECK_STYLE_PREDICATE), en);
        SpecialSpecial.registerProvable(new SkelAtom("sys_check_style_oper", scope), 1,
                new SpecialSpecial(SPECIAL_SYS_CHECK_STYLE_OPER), en);
    }

    /**
     * <p>Define a built-in based on name and length.</p>
     * <p>The store is passed via the engine.</p>
     *
     * @param sa    The predicate name.
     * @param arity The predicate length.
     * @param del   The delegate.
     * @param en    The engine.
     * @throws EngineMessage Shit happens.
     */
    private static void registerProvable(SkelAtom sa, int arity,
                                         AbstractDelegate del,
                                         Engine en)
            throws EngineMessage, EngineException {
        CachePredicate cp = CachePredicate.getPredicateDefined(sa, arity, en, true);
        Predicate pick = cp.pick;
        SpecialSpecial.definePredicate(pick, del, en);
        Predicate.checkPredicateDecl(pick, sa, en);
    }

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
        boolean f1 = ((pick.getBits() & Predicate.MASK_PRED_VIRT) != 0);
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
     * <p>Logically evaluate a goal in a list of goals for the first time.</p>
     * <p>The goal is passed via the skel and display of the engine.</p>
     * <p>The continuation is passed via the r and u of the engine.</p>
     * <p>The new continuation is returned via the skel and display of the engine.</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        try {
            switch (id) {
                case SPECIAL_SYS_CONTEXT_PROPERTY:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    en.skel = temp[0];
                    en.display = ref;
                    en.deref();
                    EngineMessage.checkCallable(en.skel, en.display);
                    SkelAtom sa = SpecialBody.callableToName(en.skel);
                    String fun = (sa.scope != null ? sa.scope.getPath() : "");
                    if (!en.unifyTerm(temp[1], ref, new SkelAtom(fun), Display.DISPLAY_CONST))
                        return false;
                    return en.getNext();
                case SPECIAL_SET_SOURCE_PROPERTY:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    fun = SpecialUniv.derefAndCastString(temp[0], ref);
                    AbstractSource source = en.store.getSource(fun);
                    AbstractSource.checkExistentSource(source, fun);

                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    EngineMessage.checkCallable(en.skel, en.display);
                    SpecialSource.addSrcProp(en.skel, en.display, source, en);
                    return en.getNextRaw();
                case SPECIAL_RESET_SOURCE_PROPERTY:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    fun = SpecialUniv.derefAndCastString(temp[0], ref);
                    source = en.store.getSource(fun);
                    AbstractSource.checkExistentSource(source, fun);

                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    EngineMessage.checkCallable(en.skel, en.display);
                    SpecialSource.removeSrcProp(en.skel, en.display, source, en);
                    return en.getNextRaw();
                case SPECIAL_SYS_OP:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    Number num = SpecialEval.derefAndCastInteger(temp[0], ref);
                    SpecialEval.checkNotLessThanZero(num);
                    int level = SpecialEval.castIntValue(num);
                    SpecialOper.checkOperatorLevel(level);
                    String modestr = SpecialUniv.derefAndCastString(temp[1], ref);
                    int leftright = SpecialOper.atomToLeftRight(modestr);
                    int type = SpecialOper.atomToType(modestr);

                    SpecialQuali.colonToCallable(temp[2], ref, false, en);
                    if (en.skel instanceof SkelAtom) {
                        sa = (SkelAtom) en.skel;
                    } else {
                        EngineMessage.checkInstantiated(en.skel);
                        throw new EngineMessage(EngineMessage.typeError(
                                EngineMessage.OP_TYPE_ATOM, en.skel), en.display);
                    }
                    defineOperator(level, leftright, type, sa, en);
                    return en.getNextRaw();
                case SPECIAL_SET_OPER_PROPERTY:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    Operator op = SpecialOper.operToOperator(temp[0], ref, en);
                    Operator.checkExistentOperator(op, temp[0], ref);

                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    EngineMessage.checkCallable(en.skel, en.display);
                    SpecialOper.addOperProp(en.skel, en.display, op, en);
                    return en.getNextRaw();
                case SPECIAL_SET_PREDICATE_PROPERTY:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    Predicate pick = SpecialPred.indicatorToPredicate(temp[0], ref, en);
                    Predicate.checkExistentPredicate(pick, temp[0], ref);

                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    EngineMessage.checkCallable(en.skel, en.display);
                    SpecialPred.addPredProp(en.skel, en.display, pick, en);
                    return en.getNextRaw();
                case SPECIAL_RESET_PREDICATE_PROPERTY:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    pick = SpecialPred.indicatorToPredicate(temp[0], ref, en);
                    Predicate.checkExistentPredicate(pick, temp[0], ref);

                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    EngineMessage.checkCallable(en.skel, en.display);
                    SpecialPred.removePredProp(en.skel, en.display, pick, en);
                    return en.getNextRaw();
                case SPECIAL_SYS_SPECIAL:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    Class clazz = SpecialSpecial.nameToClass(temp[1], ref, en);

                    en.skel = temp[2];
                    en.display = ref;
                    en.deref();
                    num = SpecialEval.derefAndCastInteger(en.skel, en.display);
                    SpecialEval.checkNotLessThanZero(num);
                    SpecialEval.castIntValue(num);
                    Constructor con = SpecialSpecial.getDeclaredConstructor(clazz, SIG_INT);
                    if (!en.store.foyer.getFactory().validateExceptionTypes(con.getExceptionTypes(), en))
                        throw new EngineMessage(en.skel);
                    Object value = en.store.foyer.getFactory().newInstance(con, new Object[]{num});
                    if (!(value instanceof AbstractSpecial))
                        throw new EngineMessage(EngineMessage.typeError(
                                EngineMessage.OP_TYPE_SPECIAL,
                                new SkelAtom(AbstractRuntime.classToString(value.getClass()))));
                    AbstractDelegate del = (AbstractSpecial) value;
                    /* create the builtin */
                    pick = Predicate.indicatorToPredicateDefined(temp[0], ref, en, true);
                    SpecialSpecial.definePredicate(pick, del, en);
                    return en.getNextRaw();
                case SPECIAL_SYS_CHECK_STYLE_PREDICATE:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    pick = SpecialPred.indicatorToPredicate(temp[0], ref, en);
                    sa = (SkelAtom) en.skel;
                    Predicate.checkExistentPredicate(pick, temp[0], ref);
                    Predicate.checkPredicateDecl(pick, sa, en);
                    return en.getNextRaw();
                case SPECIAL_SYS_CHECK_STYLE_OPER:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    op = SpecialOper.operToOperator(temp[0], ref, en);
                    sa = (SkelAtom) en.skel;
                    Operator.checkExistentOperator(op, temp[0], ref);
                    Operator.checkOperDecl(op, sa, en);
                    return en.getNextRaw();
                default:
                    throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
            }
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

    /**************************************************************/
    /* Class Name Conversion                                      */
    /**************************************************************/

    /**
     * <p>Convert an structured path to a class.</p>
     *
     * @param t  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The class.
     * @throws EngineMessage Shit happens.
     */
    public static Class nameToClass(Object t, Display d, Engine en)
            throws EngineMessage {
        try {
            /* slash syntax */
            Object obj = SpecialQuali.slashToClass(t, d, false, true, en);
            SkelAtom sa = SpecialQuali.modToAtom(obj, t, d, en);

            /* find key */
            AbstractSource scope = (sa.scope != null ? sa.scope : en.store.user);
            String path = sa.fun.replace(CachePackage.OP_CHAR_SEG, CacheModule.OP_CHAR_OS);
            String key = CacheSubclass.findKey(path, scope, ForeignPath.MASK_MODL_FRGN, null);
            if (key == null) {
                throw new EngineMessage(EngineMessage.existenceError(
                        EngineMessage.OP_EXISTENCE_SOURCE_SINK, t), d);
            }

            /* key to class */
            Class<?> clazz = LookupBinary.keyToClass(key, en.store);
            if (clazz == null) {
                throw new EngineMessage(EngineMessage.existenceError(
                        EngineMessage.OP_EXISTENCE_CLASS, t), d);
            }

            return clazz;
        } catch (IOException x) {
            throw EngineMessage.mapIOException(x);
        }
    }

    /**
     * <p>A class back to a structured path.</p>
     *
     * @param clazz  The class.
     * @param source The source.
     * @param en     The engine.
     * @return The class.
     * @throws EngineMessage Shit happens.
     */
    public static Object classToName(Class clazz, AbstractSource source,
                                     Engine en)
            throws EngineMessage {
        String fun = AbstractRuntime.classToString(clazz);
        return Clause.moduleToSlashSkel(fun, source, en);
    }

    /**
     * <p>A class back to a structured path.</p>
     *
     * @param clazz The class.
     * @return The class.
     */
    public static Object classToName(Class clazz) {
        String fun = AbstractRuntime.classToString(clazz);
        return Clause.packageToSlashSkel(fun, null);
    }

    /**************************************************************/
    /* Constructor Lookup                                         */
    /**************************************************************/

    /**
     * <p>Retrieve a declared constructor.</p>
     *
     * @param decl  The declaring class.
     * @param paras The formal parameters.
     * @return The constructor.
     * @throws EngineMessage Shit happens.
     */
    public static Constructor getDeclaredConstructor(Class decl,
                                                     Class[] paras)
            throws EngineMessage {
        try {
            return decl.getDeclaredConstructor(paras);
        } catch (NoSuchMethodException x) {
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_CONSTRUCTOR,
                    constructorToCallable(paras)));
        } catch (NoClassDefFoundError x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_LINK,
                    EngineMessage.OP_PERMISSION_CLASS,
                    new SkelAtom(x.getMessage())));
        }
    }

    /**
     * <p>Method to callable.</p>
     *
     * @param paras  The parameter types.
     * @param source The source.
     * @param en     The engine.
     * @return The callable.
     */
    public static Object constructorToCallable(Class[] paras,
                                               AbstractSource source,
                                               Engine en)
            throws EngineMessage {
        SkelAtom help = new SkelAtom(OP_NAME_CONSTRUCTOR);
        if (paras.length == 0)
            return help;
        Object[] args = new Object[paras.length];
        for (int i = 0; i < paras.length; i++)
            args[i] = SpecialSpecial.classToName(paras[i], source, en);
        return new SkelCompound(help, args);
    }

    /**
     * <p>Method to callable.</p>
     *
     * @param paras The parameter types.
     * @return The callable.
     */
    public static Object constructorToCallable(Class[] paras) {
        SkelAtom help = new SkelAtom(OP_NAME_CONSTRUCTOR);
        if (paras.length == 0)
            return help;
        Object[] args = new Object[paras.length];
        for (int i = 0; i < paras.length; i++)
            args[i] = SpecialSpecial.classToName(paras[i]);
        return new SkelCompound(help, args);
    }

    /**************************************************************/
    /* Bootstrap Syntax Operator Definition                       */
    /**************************************************************/
    /**
     * <p>Define, redefine or undefined an operator.</p>
     * <p>If mode!=0 then the operator is defined or redefined.</p>
     * <p>If mode=0 then the operator is undefined.</p>
     *
     * @param level     The operator level.
     * @param leftright The mode.
     * @param type      The type.
     * @param sa        The name.
     * @param en        The engine.
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    private static void defineOperator(int level, int leftright,
                                       int type, SkelAtom sa,
                                       Engine en)
            throws EngineMessage, EngineException {
        Operator op = OperatorSearch.getOperDefined(sa, type, en, true);
        op.setLevel(level);
        op.resetBit(Operator.MASK_OPER_MODE);
        op.setBit(leftright);
        op.setPosition(sa.getPosition());
    }

}
