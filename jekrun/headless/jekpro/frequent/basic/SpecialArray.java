package jekpro.frequent.basic;

import jekpro.model.builtin.SpecialModel;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.runtime.SpecialQuali;
import jekpro.tools.array.AbstractDelegate;
import jekpro.tools.array.AbstractFactory;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides built-in predicates for the module array.</p>
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
public final class SpecialArray extends AbstractSpecial {
    private final static int SPECIAL_SYS_FOREIGN_DIMENSION = 0;
    private final static int SPECIAL_SYS_FOREIGN_ELEMENT = 1;
    private final static int SPECIAL_SYS_FOREIGN_UPDATE = 2;
    private final static int SPECIAL_SYS_FOREIGN_LENGTH = 3;

    /**
     * <p>Create a foreign special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialArray(int i) {
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
        switch (id) {
            case SPECIAL_SYS_FOREIGN_DIMENSION:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Integer arity = SpecialQuali.colonToIndicator(temp[0], ref, en);
                Class clazz = SpecialModel.nameToClass(temp[1], ref, en);
                AbstractFactory factory = en.store.foyer.getFactory();
                if (!factory.getReflection().createArray(clazz, en, AbstractFactory.ARRAY_NEW))
                    throw new EngineMessage(en.skel);
                AbstractDelegate del = (AbstractDelegate) en.skel;
                if (arity.intValue() != del.getArity())
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_ARITY_MISMATCH,
                            Integer.valueOf(del.getArity())));
                /* create the builtin */
                Predicate pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_DEFI);
                Predicate.definePredicate(pick, del, en);
                return true;
            case SPECIAL_SYS_FOREIGN_ELEMENT:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                arity = SpecialQuali.colonToIndicator(temp[0], ref, en);
                clazz = SpecialModel.nameToClass(temp[1], ref, en);
                factory = en.store.foyer.getFactory();
                if (!factory.getReflection().createArray(clazz, en, AbstractFactory.ARRAY_GET))
                    throw new EngineMessage(en.skel);
                del = (AbstractDelegate) en.skel;
                if (arity.intValue() != del.getArity())
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_ARITY_MISMATCH,
                            Integer.valueOf(del.getArity())));
                /* create the builtin */
                pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_DEFI);
                Predicate.definePredicate(pick, del, en);
                return true;
            case SPECIAL_SYS_FOREIGN_UPDATE:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                arity = SpecialQuali.colonToIndicator(temp[0], ref, en);
                clazz = SpecialModel.nameToClass(temp[1], ref, en);
                factory = en.store.foyer.getFactory();
                if (!factory.getReflection().createArray(clazz, en, AbstractFactory.ARRAY_SET))
                    throw new EngineMessage(en.skel);
                del = (AbstractDelegate) en.skel;
                if (arity.intValue() != del.getArity())
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_ARITY_MISMATCH,
                            Integer.valueOf(del.getArity())));
                /* create the builtin */
                pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_DEFI);
                Predicate.definePredicate(pick, del, en);
                return true;
            case SPECIAL_SYS_FOREIGN_LENGTH:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                arity = SpecialQuali.colonToIndicator(temp[0], ref, en);
                clazz = SpecialModel.nameToClass(temp[1], ref, en);
                factory = en.store.foyer.getFactory();
                if (!factory.getReflection().createArray(clazz, en, AbstractFactory.ARRAY_LENGTH))
                    throw new EngineMessage(en.skel);
                del = (AbstractDelegate) en.skel;
                if (arity.intValue() != del.getArity())
                    throw new EngineMessage(EngineMessage.domainError(
                            EngineMessage.OP_DOMAIN_ARITY_MISMATCH,
                            Integer.valueOf(del.getArity())));
                /* create the builtin */
                pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_DEFI);
                Predicate.definePredicate(pick, del, en);
                return true;
            default:
                throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

}
