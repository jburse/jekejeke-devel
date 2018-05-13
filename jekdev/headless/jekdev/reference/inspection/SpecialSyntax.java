package jekdev.reference.inspection;

import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.rope.Operator;
import jekpro.reference.reflect.SpecialOper;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

/**
 * <p>This module provides built-ins for direct operator access.</p>
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
public final class SpecialSyntax extends AbstractSpecial {
    private static final String OP_EXISTENCE_SYNTAX = "syntax";

    private final static int SPECIAL_SYS_CURRENT_SYNTAX_CHK = 1;
    private final static int SPECIAL_SYS_SYNTAX_PROPERTY = 2;

    private final static int SPECIAL_SET_SYNTAX_PROPERTY = 4;
    private final static int SPECIAL_RESET_SYNTAX_PROPERTY = 5;

    /**
     * <p>Create a syntax direct access builtin.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialSyntax(int i) {
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
            case SPECIAL_SYS_CURRENT_SYNTAX_CHK:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                Operator op = Operator.operToSyntax(temp[0], ref, en);
                if (op == null)
                    return false;
                return en.getNextRaw();
            case SPECIAL_SYS_SYNTAX_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                op = Operator.operToSyntax(temp[0], ref, en);
                SkelAtom sa = (SkelAtom) en.skel;
                if (op == null)
                    return false;
                SpecialOper.operToProperties(op, sa, en);
                if (!en.unifyTerm(temp[1], ref, en.skel, en.display))
                    return false;
                return en.getNext();
            case SPECIAL_SET_SYNTAX_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                op = Operator.operToSyntax(temp[0], ref, en);
                sa = (SkelAtom) en.skel;
                SpecialSyntax.checkExistentSyntax(op, temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialOper.addOperProp(en.skel, en.display, op, sa, en);
                return en.getNextRaw();
            case SPECIAL_RESET_SYNTAX_PROPERTY:
                temp = ((SkelCompound) en.skel).args;
                ref = en.display;
                op = Operator.operToSyntax(temp[0], ref, en);
                sa = (SkelAtom) en.skel;
                SpecialSyntax.checkExistentSyntax(op, temp[0], ref);
                en.skel = temp[1];
                en.display = ref;
                en.deref();
                EngineMessage.checkInstantiated(en.skel);
                EngineMessage.checkCallable(en.skel, en.display);
                SpecialOper.removeOperProp(en.skel, en.display, op, sa, en);
                return en.getNextRaw();
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }


    /**
     * <p>Assure that the operator is existent.</p>
     *
     * @param op The operator.
     * @param t  The skel.
     * @param d  The display.
     * @throws EngineMessage Shit happens.
     */
    private static void checkExistentSyntax(Operator op, Object t, Display d)
            throws EngineMessage {
        if (op == null)
            throw new EngineMessage(EngineMessage.existenceError(
                    SpecialSyntax.OP_EXISTENCE_SYNTAX, t), d);
    }

}
