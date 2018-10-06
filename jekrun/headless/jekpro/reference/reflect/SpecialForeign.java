package jekpro.reference.reflect;

import jekpro.model.builtin.SpecialSpecial;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.AbstractSource;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * <p>Provides built-in predicates for the module foreign.</p>
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
public final class SpecialForeign extends AbstractSpecial {
    public final static Class[] VOID_TYPES = new Class[0];

    private final static int SPECIAL_SYS_FOREIGN = 0;
    private final static int SPECIAL_SYS_FOREIGN_CONSTRUCTOR = 1;
    private final static int SPECIAL_SYS_FOREIGN_GETTER = 2;
    private final static int SPECIAL_SYS_FOREIGN_SETTER = 3;
    private final static int SPECIAL_SYS_FOREIGN_FUN = 4;
    private final static int SPECIAL_SYS_FOREIGN_CONST = 5;

    /**
     * <p>Create a foreign special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialForeign(int i) {
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
     * @throws EngineMessage   Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        switch (id) {
            case SPECIAL_SYS_FOREIGN:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Integer arity = SpecialQuali.colonToIndicator(temp[0], ref, en);
                Class clazz = SpecialSpecial.nameToClass(temp[1], ref, en);
                String name = SpecialForeign.methodName(temp[2], ref, en);
                if (SpecialSpecial.OP_NAME_CONSTRUCTOR.equals(name))
                    throw new EngineMessage(EngineMessage.typeError(
                            EngineMessage.OP_TYPE_METHOD, temp[2]), ref);
                Class[] paras = SpecialForeign.formalParameters(temp[2], ref, en);
                Method mth = SpecialForeign.getDeclaredMethod(clazz, name, paras);
                if (!en.store.foyer.getFactory().createMethod(mth, en, true))
                    throw new EngineMessage(en.skel);
                AbstractDelegate del = (AbstractDelegate) en.skel;
                if (arity.intValue() != del.getArity())
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_ARITY_MISMATCH,
                            Integer.valueOf(del.getArity())));
                /* create the builtin */
                Predicate pick = Predicate.indicatorToPredicateDefined(temp[0], ref, en, true);
                SpecialSpecial.definePredicate(pick, del, en);
                return en.getNextRaw();
            case SPECIAL_SYS_FOREIGN_CONSTRUCTOR:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                arity = SpecialQuali.colonToIndicator(temp[0], ref, en);
                clazz = SpecialSpecial.nameToClass(temp[1], ref, en);
                name = SpecialForeign.methodName(temp[2], ref, en);
                if (!SpecialSpecial.OP_NAME_CONSTRUCTOR.equals(name))
                    throw new EngineMessage(EngineMessage.typeError(
                            EngineMessage.OP_TYPE_CONSTRUCTOR, temp[2]), ref);
                paras = SpecialForeign.formalParameters(temp[2], ref, en);
                Constructor cstr = SpecialSpecial.getDeclaredConstructor(clazz, paras);
                if (!en.store.foyer.getFactory().createConstructor(cstr, en))
                    throw new EngineMessage(en.skel);
                del = (AbstractDelegate) en.skel;
                if (arity.intValue() != del.getArity())
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_ARITY_MISMATCH,
                            Integer.valueOf(del.getArity())));
                /* create the builtin */
                pick = Predicate.indicatorToPredicateDefined(temp[0], ref, en, true);
                SpecialSpecial.definePredicate(pick, del, en);
                return en.getNextRaw();
            case SPECIAL_SYS_FOREIGN_GETTER:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                arity = SpecialQuali.colonToIndicator(temp[0], ref, en);
                clazz = SpecialSpecial.nameToClass(temp[1], ref, en);
                name = SpecialUniv.derefAndCastString(temp[2], ref);
                Field fld = SpecialForeign.getDeclaredField(clazz, name);
                if (!en.store.foyer.getFactory().createField(fld, en, AbstractFactory.FIELD_GET_PRED))
                    throw new EngineMessage(en.skel);
                del = (AbstractDelegate) en.skel;
                if (arity.intValue() != del.getArity())
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_ARITY_MISMATCH,
                            Integer.valueOf(del.getArity())));
                /* create the builtin */
                pick = Predicate.indicatorToPredicateDefined(temp[0], ref, en, true);
                SpecialSpecial.definePredicate(pick, del, en);
                return en.getNextRaw();
            case SPECIAL_SYS_FOREIGN_SETTER:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                arity = SpecialQuali.colonToIndicator(temp[0], ref, en);
                clazz = SpecialSpecial.nameToClass(temp[1], ref, en);
                name = SpecialUniv.derefAndCastString(temp[2], ref);
                fld = SpecialForeign.getDeclaredField(clazz, name);
                if (!en.store.foyer.getFactory().createField(fld, en, AbstractFactory.FIELD_SET))
                    throw new EngineMessage(en.skel);
                del = (AbstractDelegate) en.skel;
                if (arity.intValue() != del.getArity())
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_ARITY_MISMATCH,
                            Integer.valueOf(del.getArity())));
                /* create the builtin */
                pick = Predicate.indicatorToPredicateDefined(temp[0], ref, en, true);
                SpecialSpecial.definePredicate(pick, del, en);
                return en.getNextRaw();
            case SPECIAL_SYS_FOREIGN_FUN:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                arity = SpecialQuali.colonToIndicator(temp[0], ref, en);
                clazz = SpecialSpecial.nameToClass(temp[1], ref, en);
                name = SpecialForeign.methodName(temp[2], ref, en);
                paras = SpecialForeign.formalParameters(temp[2], ref, en);
                mth = SpecialForeign.getDeclaredMethod(clazz, name, paras);
                if (!en.store.foyer.getFactory().createMethod(mth, en, false))
                    throw new EngineMessage(en.skel);
                del = (AbstractDelegate) en.skel;
                if (arity.intValue() != del.getArity())
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_ARITY_MISMATCH,
                            Integer.valueOf(del.getArity())));
                /* create the builtin */
                pick = Predicate.indicatorToPredicateDefined(temp[0], ref, en, true);
                SpecialSpecial.definePredicate(pick, del, en);
                return en.getNextRaw();
            case SPECIAL_SYS_FOREIGN_CONST:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                arity = SpecialQuali.colonToIndicator(temp[0], ref, en);
                clazz = SpecialSpecial.nameToClass(temp[1], ref, en);
                name = SpecialUniv.derefAndCastString(temp[2], ref);
                fld = SpecialForeign.getDeclaredField(clazz, name);
                if (!en.store.foyer.getFactory().createField(fld, en, AbstractFactory.FIELD_GET_EVAL))
                    throw new EngineMessage(en.skel);
                del = (AbstractDelegate) en.skel;
                if (arity.intValue() != del.getArity())
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_ARITY_MISMATCH,
                            Integer.valueOf(del.getArity())));
                /* create the builtin */
                pick = Predicate.indicatorToPredicateDefined(temp[0], ref, en, true);
                SpecialSpecial.definePredicate(pick, del, en);
                return en.getNextRaw();
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /***********************************************************************/
    /* Foreign Predicate Parameters                                        */
    /***********************************************************************/

    /**
     * <p>Convert a callable to the method name.</p>
     *
     * @param t  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The method name.
     * @throws EngineMessage Validation error.
     */
    private static String methodName(Object t, Display d, Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        if (t instanceof SkelAtom) {
            SkelAtom sa = (SkelAtom) t;
            return sa.fun;
        } else if (t instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) t;
            return sc.sym.fun;
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_CALLABLE, t), d);
        }
    }

    /**
     * <p>Convert a callable to the formal parameters.</p>
     *
     * @param t  The term skeleton.
     * @param d  The term display.
     * @param en The engine.
     * @return The formal parameters.
     * @throws EngineMessage Validation error.
     */
    private static Class[] formalParameters(Object t, Display d, Engine en)
            throws EngineMessage {
        en.skel = t;
        en.display = d;
        en.deref();
        t = en.skel;
        d = en.display;
        if (t instanceof SkelAtom) {
            return VOID_TYPES;
        } else if (t instanceof SkelCompound) {
            SkelCompound sc = (SkelCompound) t;
            Class[] paras = new Class[sc.args.length];
            for (int i = 0; i < paras.length; i++)
                paras[i] = SpecialSpecial.nameToClass(sc.args[i], d, en);
            return paras;
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_CALLABLE, t), d);
        }
    }

    /**
     * <p>Retrieve a declared method.</p>
     *
     * @param decl  The declaring class.
     * @param name  The method name.
     * @param paras The formal parameters.
     * @return The method.
     * @throws EngineMessage Shit happens.
     */
    public static Method getDeclaredMethod(Class decl, String name,
                                           Class[] paras)
            throws EngineMessage {
        try {
            return decl.getDeclaredMethod(name, paras);
        } catch (NoSuchMethodException x) {
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_METHOD,
                    methodToCallable(name, paras)));
        } catch (NoClassDefFoundError x) {
            throw new EngineMessage(EngineMessage.permissionError(
                    EngineMessage.OP_PERMISSION_LINK,
                    EngineMessage.OP_PERMISSION_CLASS,
                    new SkelAtom(x.getMessage())));
        }
    }

    /**
     * <p>Retrieve a declared field.</p>
     *
     * @param decl The declaring class.
     * @param name The field name.
     * @return The method.
     * @throws EngineMessage Shit happens.
     */
    public static Field getDeclaredField(Class decl, String name)
            throws EngineMessage {
        try {
            return decl.getDeclaredField(name);
        } catch (NoSuchFieldException x) {
            throw new EngineMessage(EngineMessage.existenceError(
                    EngineMessage.OP_EXISTENCE_FIELD,
                    new SkelAtom(name)));
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
     * @param name   The name.
     * @param paras  The parameters.
     * @param source The source.
     * @param en     The engine.
     * @return The callable.
     * @throws EngineMessage Shit happens.
     */
    public static Object methodToCallable(String name, Class[] paras,
                                          AbstractSource source, Engine en)
            throws EngineMessage {
        SkelAtom help = new SkelAtom(name);
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
     * @param name  The name.
     * @param paras The parameters.
     * @return The callable.
     */
    public static Object methodToCallable(String name, Class[] paras) {
        SkelAtom help = new SkelAtom(name);
        if (paras.length == 0)
            return help;
        Object[] args = new Object[paras.length];
        for (int i = 0; i < paras.length; i++)
            args[i] = SpecialSpecial.classToName(paras[i]);
        return new SkelCompound(help, args);
    }

}
