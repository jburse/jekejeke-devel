package jekpro.model.builtin;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.bootload.ForeignPath;
import jekpro.reference.reflect.SpecialForeign;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.runtime.EvaluableLogic;
import jekpro.reference.runtime.SpecialLogic;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.array.Types;
import jekpro.tools.foreign.AutoClass;
import jekpro.tools.foreign.LookupBinary;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.config.AbstractRuntime;

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
    public final static int SPECIAL_SYS_SPECIAL = 0;
    public final static int SPECIAL_SYS_CHECK_STYLE_BODY = 1;
    private final static int SPECIAL_SET_PREDICATE_PROPERTY = 2;
    private final static int SPECIAL_RESET_PREDICATE_PROPERTY = 3;
    private final static int SPECIAL_SYS_CHECK_STYLE_HEAD = 4;
    private final static int SPECIAL_SYS_NEUTRAL_PREDICATE = 5;

    /**
     * <p>Create a special special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialSpecial(int i) {
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
     * @throws EngineMessage   Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineException, EngineMessage {
        try {
            switch (id) {
                case SPECIAL_SYS_SPECIAL:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    Class clazz = nameToClass(temp[1], ref, en);

                    en.skel = temp[2];
                    en.display = ref;
                    en.deref();
                    Number num = SpecialEval.derefAndCastInteger(en.skel, en.display);
                    SpecialEval.checkNotLessThanZero(num);
                    SpecialEval.castIntValue(num);
                    Constructor con = SpecialForeign.getDeclaredConstructor(clazz, Integer.TYPE);
                    AbstractFactory factory = en.store.foyer.getFactory();
                    if (!factory.getReflection().validateExceptionTypes(con.getExceptionTypes(), en))
                        throw new EngineMessage(en.skel);
                    Object value = AutoClass.invokeNew(con, num);
                    if (!(value instanceof AbstractSpecial))
                        throw new EngineMessage(EngineMessage.typeError(
                                EngineMessage.OP_TYPE_SPECIAL,
                                new SkelAtom(AbstractRuntime.classToString(value.getClass()))));
                    AbstractDelegate del = (AbstractSpecial) value;
                    Predicate pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                            ref, en, CachePredicate.MASK_CACH_DEFI);
                    Predicate.definePredicate(pick, del, en);
                    return true;
                case SPECIAL_SYS_CHECK_STYLE_BODY:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    pick = SpecialPred.indicatorToPredicate(temp[0], ref, en);
                    SkelAtom sa = (SkelAtom) en.skel;
                    Predicate.checkExistentPredicate(pick, temp[0], ref);
                    Predicate.checkPredicateBody(pick, sa, en);
                    return true;
                case SPECIAL_SET_PREDICATE_PROPERTY:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    pick = SpecialPred.indicatorToPredicate(temp[0], ref, en);
                    Predicate.checkExistentPredicate(pick, temp[0], ref);

                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    EngineMessage.checkCallable(en.skel, en.display);
                    SpecialPred.setPredProp(pick, en.skel, en.display, en);
                    return true;
                case SPECIAL_RESET_PREDICATE_PROPERTY:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    pick = SpecialPred.indicatorToPredicate(temp[0], ref, en);
                    Predicate.checkExistentPredicate(pick, temp[0], ref);

                    en.skel = temp[1];
                    en.display = ref;
                    en.deref();
                    EngineMessage.checkCallable(en.skel, en.display);
                    SpecialPred.resetPredProp(pick, en.skel, en.display, en);
                    return true;
                case SPECIAL_SYS_CHECK_STYLE_HEAD:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    pick = SpecialPred.indicatorToPredicate(temp[0], ref, en);
                    sa = (SkelAtom) en.skel;
                    Predicate.checkExistentPredicate(pick, temp[0], ref);
                    Predicate.checkPredicateHead(pick, sa, en);
                    return true;
                case SPECIAL_SYS_NEUTRAL_PREDICATE:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    SpecialPred.indicatorToPredicateDefined(temp[0],
                            ref, en, CachePredicate.MASK_CACH_DEFI);
                    return true;
                default:
                    throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
            }
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
        }
    }


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
            Object obj = EvaluableLogic.slashToClass(t, d, CacheModule.MASK_MODULE_NAUT, en);
            SkelAtom sa = SpecialLogic.modToAtom(obj, t, d, en);

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

}
