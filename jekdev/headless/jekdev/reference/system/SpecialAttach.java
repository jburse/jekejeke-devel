package jekdev.reference.system;

import jekdev.model.builtin.SupervisorTrace;
import jekdev.reference.inspection.SpecialProvable;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.molec.CachePredicate;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.PositionKey;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import matula.util.data.ListArray;

/**
 * <p>Provides a special predicates for thread attach.</p>
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
public final class SpecialAttach extends AbstractSpecial {
    private final static int SPECIAL_TSPY = 0;
    private final static int SPECIAL_TNOSPY = 1;
    private final static int SPECIAL_SYS_TSPYING = 2;
    private final static int SPECIAL_SYS_TBREAK = 3;
    private final static int SPECIAL_SYS_TNOBREAK = 4;
    private final static int SPECIAL_SYS_TBREAKING = 5;

    /**
     * <p>Create a engine trace attach.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialAttach(int i) {
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
     * @throws EngineMessage   Shit happens.
     * @throws EngineException Shit happens.
     */
    public final boolean moniFirst(Engine en)
            throws EngineMessage, EngineException {
        switch (id) {
            case SPECIAL_TSPY:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Predicate pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                SpecialProvable.checkExistentProvable(pick, temp[0], ref);
                ((SupervisorTrace) en.visor).addThreadSpyPoint(pick.getArity(), pick.getFun());
                return true;
            case SPECIAL_SYS_TSPYING:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                if (!en.unifyTerm(currentThreadSpyPoints(en), Display.DISPLAY_CONST, temp[0], ref))
                    return false;
                return true;
            case SPECIAL_TNOSPY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                        ref, en, CachePredicate.MASK_CACH_UCHK);
                SpecialProvable.checkExistentProvable(pick, temp[0], ref);
                ((SupervisorTrace) en.visor).removeThreadSpyPoint(pick.getArity(), pick.getFun());
                return true;
            case SPECIAL_SYS_TBREAK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                String key = SpecialUniv.derefAndCastString(temp[0], ref);
                Number num = SpecialEval.derefAndCastInteger(temp[1], ref);
                SpecialEval.checkNotLessThanZero(num);
                int line = SpecialEval.castIntValue(num);
                PositionKey pk = new PositionKey(key, line);
                ((SupervisorTrace) en.visor).addThreadBreakPoint(pk);
                return true;
            case SPECIAL_SYS_TNOBREAK:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                key = SpecialUniv.derefAndCastString(temp[0], ref);
                num = SpecialEval.derefAndCastInteger(temp[1], ref);
                SpecialEval.checkNotLessThanZero(num);
                line = SpecialEval.castIntValue(num);
                pk = new PositionKey(key, line);
                ((SupervisorTrace) en.visor).removeThreadBreakPoint(pk);
                return true;
            case SPECIAL_SYS_TBREAKING:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                if (!en.unifyTerm(currentThreadBreakPoints(en), Display.DISPLAY_CONST, temp[0], ref))
                    return false;
                return true;
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /**************************************************************/
    /* Thread Spypoint Enumeration                                */
    /**************************************************************/

    /**
     * <p>Create a prolog list with the spy points.</p>
     *
     * @param en The engine.
     * @return The prolog list of the spy points.
     * @throws EngineMessage Shit happens.
     */
    private static Object currentThreadSpyPoints(Engine en)
            throws EngineMessage {
        Object res = en.store.foyer.ATOM_NIL;
        ListArray<StoreKey> spypoints = ((SupervisorTrace) en.visor).snapshotThreadSpyPoints();
        for (int i = 0; i < spypoints.size; i++) {
            StoreKey sk = spypoints.get(i);
            Object decl = SpecialPred.indicatorToColonSkel(sk.getFun(), en.store.user,
                    sk.getArity(), en);
            res = new SkelCompound(en.store.foyer.ATOM_CONS, decl, res);
        }
        return res;
    }

    /**************************************************************/
    /* Breakpoint Enumeration                                     */
    /**************************************************************/

    /**
     * <p>Create a prolog list with the break points.</p>
     *
     * @param en The engine.
     * @return The prolog list of the break points.
     */
    private static Object currentThreadBreakPoints(Engine en) {
        Object res = en.store.foyer.ATOM_NIL;
        ListArray<PositionKey> breakpoints = ((SupervisorTrace) en.visor).snapshotThreadBreakPoints();
        for (int i = 0; i < breakpoints.size(); i++) {
            PositionKey pk = breakpoints.get(i);
            Object decl = new SkelCompound(en.store.foyer.ATOM_SUB,
                    new SkelAtom(pk.getOrigin()), Integer.valueOf(pk.getLineNo()));
            res = new SkelCompound(en.store.foyer.ATOM_CONS, decl, res);
        }
        return res;
    }

}