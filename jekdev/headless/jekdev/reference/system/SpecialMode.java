package jekdev.reference.system;

import jekdev.model.bugger.GoalTrace;
import jekdev.model.bugger.SupervisorTrace;
import jekdev.model.pretty.FoyerTrace;
import jekdev.model.pretty.StoreTrace;
import jekdev.reference.debug.SpecialDefault;
import jekpro.model.inter.*;
import jekpro.model.molec.*;
import jekpro.model.pretty.AbstractSource;
import jekpro.model.rope.Clause;
import jekpro.model.rope.Directive;
import jekpro.model.rope.Goal;
import jekpro.model.rope.Intermediate;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides a special predicates for instrumented code.</p>
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
public final class SpecialMode extends AbstractSpecial {
    public final static String OP_CALL = "call";
    public final static String OP_FAIL = "fail";
    public final static String OP_EXIT = "exit";
    public final static String OP_REDO = "redo";
    public final static String OP_HEAD = "head";
    public final static String OP_CHOP = "chop";

    public final static int CODE_CALL = 0;
    public final static int CODE_FAIL = 1;
    public final static int CODE_EXIT = 2;
    public final static int CODE_REDO = 3;
    public final static int CODE_HEAD = 4;
    public final static int CODE_CHOP = 5;

    private final static int SPECIAL_SYS_IGNORE = 0;
    private final static int SPECIAL_SYS_NOTRACE_CHK = 1;
    private final static int SPECIAL_SYS_PORT_SHOW = 2;
    private final static int SPECIAL_SYS_CUT_CHK = 3;
    private final static int SPECIAL_SYS_GOAL_CHK = 4;
    private final static int SPECIAL_SYS_GOAL_CUT = 5;
    private final static int SPECIAL_SYS_CLAUSE_CHK = 6;

    /**
     * <p>Create a mode mask special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialMode(int i) {
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
            case SPECIAL_SYS_IGNORE:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();
                if (!SpecialMode.invokeIgnore(en))
                    return false;
                return true;
            case SPECIAL_SYS_NOTRACE_CHK:
                temp = ((SkelCompound) en.skel).args;
                int port = ((Integer) temp[0]).intValue();

                CallFrame u2 = getPortDisplay(port, en);
                Intermediate r2 = getGoalSkel(port, u2);
                u2 = u2.contdisplay;

                StackElement.callGoal(r2, u2, en);
                SkelAtom sa = StackElement.callableToName(en.skel);
                if (sa != null && sa.scope != null &&
                        (sa.scope.getBits() & AbstractSource.MASK_SRC_NOTR) != 0)
                    return true;

                return false;
            case SPECIAL_SYS_PORT_SHOW:
                temp = ((SkelCompound) en.skel).args;
                port = ((Integer) temp[0]).intValue();

                if (!SpecialMode.isDebug(en))
                    return true;

                int tflags = en.visor.flags & SpecialDefault.MASK_MODE_VIBL;
                int flags = ((StoreTrace) en.store).flags & SpecialDefault.MASK_MODE_VIBL;
                if (!SpecialMode.isPort((tflags != 0 ? tflags : flags) >> 24, port))
                    return true;

                u2 = getPortDisplay(port, en);
                r2 = getGoalSkel(port, u2);
                u2 = u2.contdisplay;

                tflags = en.visor.flags & SpecialDefault.MASK_MODE_DEBG;
                flags = ((StoreTrace) en.store).flags & SpecialDefault.MASK_MODE_DEBG;
                switch (tflags != SpecialDefault.MASK_DEBG_INHR ? tflags : flags) {
                    case SpecialDefault.MASK_DEBG_STIN:
                        break;
                    case SpecialDefault.MASK_DEBG_STVR:
                    case SpecialDefault.MASK_DEBG_STOT:
                        SupervisorTrace visortrace = (SupervisorTrace) en.visor;
                        StackElement frame = visortrace.getSkipFrame();
                        if (frame != null && frame.contskel == r2 &&
                                frame.contdisplay == u2) {
                            visortrace.setSkipFrame(null);
                            break;
                        }
                        return true;
                    case SpecialDefault.MASK_DEBG_DBON:
                        StoreTrace storetrace = (StoreTrace) en.store;
                        visortrace = (SupervisorTrace) en.visor;

                        StackElement.callGoal(r2, u2, en);
                        sa = StackElement.callableToName(en.skel);
                        if (sa != null && sa.getPosition() != null &&
                                (visortrace.containsThreadBreakPoint(sa.getPosition()) ||
                                        storetrace.containsBreakPoint(sa.getPosition()))) {
                            break;
                        }

                        CachePredicate cp = StackElement.callableToPredicate(en.skel, en);
                        if (cp != null && (cp.flags & CachePredicate.MASK_PRED_VISI) != 0 &&
                                (visortrace.containsThreadSpyPoint(cp.pick.getArity(), cp.pick.getFun()) ||
                                        storetrace.containsSpyPoint(cp.pick.getArity(), cp.pick.getFun()))) {
                            break;
                        }
                        return true;
                    default:
                        throw new IllegalArgumentException("illegal mode");
                }
                StackElement val = new StackElement();
                val.contskel = r2;
                val.contdisplay = u2;
                en.skel = new SkelCompound(((FoyerTrace)
                        en.store.foyer).ATOM_TRACE_GOAL, portToAtom(port, en), val);
                en.display = Display.DISPLAY_CONST;
                if (!SpecialMode.invokeBoth(en))
                    return false;
                return true;
            case SPECIAL_SYS_CUT_CHK:
                temp = ((SkelCompound) en.skel).args;
                port = ((Integer) temp[0]).intValue();

                u2 = getPortDisplay(port, en);
                u2 = u2.contdisplay;

                port = en.number - 1;
                if (isCutChoice(port, u2))
                    return false;
                return true;
            case SPECIAL_SYS_GOAL_CHK:
                temp = ((SkelCompound) en.skel).args;
                port = ((Integer) temp[0]).intValue();

                u2 = getPortDisplay(port, en);
                r2 = getGoalSkel(port, u2);
                u2 = u2.contdisplay;

                AbstractChoice choice = en.choices.next;
                if (!isGoalChoice(choice, r2, u2))
                    return false;
                return true;
            case SPECIAL_SYS_GOAL_CUT:
                en.window = en.contdisplay;
                en.fault = null;
                en.cutChoices(en.number - 1);
                en.window = null;
                if (en.fault != null)
                    throw en.fault;
                return true;
            case SPECIAL_SYS_CLAUSE_CHK:
                temp = ((SkelCompound) en.skel).args;
                port = ((Integer) temp[0]).intValue();

                u2 = getPortDisplay(port, en);
                u2 = u2.contdisplay;

                choice = en.choices.next;
                if (isClauseChoice(choice, u2))
                    return false;
                return true;
            default:
                throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
        }
    }

    /**
     * <p>Check whether the clause has previous choice points.</p>
     *
     * @param u2 The current clause display,
     * @return True if there are previous choice points, otherwise false.
     */
    private static boolean isCutChoice(int num, CallFrame u2) {
        while ((u2.flags & Directive.MASK_DIRE_NOBR) != 0)
            u2 = u2.contdisplay;
        if (u2.number >= num)
            return false;
        return true;
    }

    /**
     * <p>Check whether the choice point is a call instrumentation choice point.</p>
     *
     * @param choice The choice point.
     * @param u2     The current clause display,
     * @return True if the choice point is current clause, otherwise false.
     */
    private static boolean isGoalChoice(AbstractChoice choice,
                                        Intermediate r2, CallFrame u2) {
        if (!(choice instanceof ChoiceDefined))
            return false;
        CallFrame u3 = ((ChoiceDefined) choice).newdisp.contdisplay;
        if (u3 == null ||
                u3.contdisplay != u2 ||
                u3.contskel.next != r2)
            return false;
        return true;
    }

    /**
     * <p>Check whether the choice point is a current clause choice point.</p>
     *
     * @param choice The choice point.
     * @param u2     The current clause display,
     * @return True if the choice point is current clause, otherwise false.
     */
    private static boolean isClauseChoice(AbstractChoice choice,
                                          CallFrame u2) {
        if (!(choice instanceof ChoiceDefined))
            return false;
        if (((ChoiceDefined) choice).newdisp != u2)
            return false;
        return true;
    }

    /**
     * <p>Invoke a goal ignoring flag changes.</p>
     * <p>The goal is passed via the skeleton and display of the engine</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    private static boolean invokeBoth(Engine en)
            throws EngineException, EngineMessage {
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        boolean backignore = en.visor.setIgnore(false);
        boolean backverify = en.visor.setVerify(false);
        int snap = en.number;
        try {
            boolean multi = en.wrapGoal();
            Display ref = en.display;
            Directive dire = en.store.foyer.CLAUSE_CALL;
            Display d2 = new Display(dire.size);
            d2.bind[0].bindUniv(en.skel, ref, en);
            if (multi)
                ref.remTab(en);
            CallFrame ref2 = CallFrame.getFrame(d2, dire, en);
            en.contskel = dire;
            en.contdisplay = ref2;
            if (!en.runLoop(snap, true)) {
                en.visor.setVerify(backverify);
                en.visor.setIgnore(backignore);
                return false;
            }
        } catch (EngineMessage x) {
            en.contskel = r;
            en.contdisplay = u;
            en.visor.setVerify(backverify);
            en.visor.setIgnore(backignore);
            throw x;
        } catch (EngineException x) {
            en.contskel = r;
            en.contdisplay = u;
            en.visor.setVerify(backverify);
            en.visor.setIgnore(backignore);
            throw x;
        }
        en.contskel = r;
        en.contdisplay = u;
        if (en.number != snap) {
            /* create choice point */
            en.choices = new ChoiceBoth(en.choices, snap, r, u);
            en.number++;
        }
        en.visor.setVerify(backverify);
        en.visor.setIgnore(backignore);
        return true;
    }

    /**
     * <p>Invoke a goal ignoring flag changes.</p>
     * <p>The goal is passed via the skeleton and display of the engine</p>
     *
     * @param en The engine.
     * @return True if the predicate succeeded, otherwise false
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    private static boolean invokeIgnore(Engine en)
            throws EngineException, EngineMessage {
        Intermediate r = en.contskel;
        CallFrame u = en.contdisplay;
        boolean backignore = en.visor.setIgnore(false);
        int snap = en.number;
        try {
            boolean multi = en.wrapGoal();
            Display ref = en.display;
            Directive dire = en.store.foyer.CLAUSE_CALL;
            Display d2 = new Display(dire.size);
            d2.bind[0].bindUniv(en.skel, ref, en);
            if (multi)
                ref.remTab(en);
            CallFrame ref2 = CallFrame.getFrame(d2, dire, en);
            en.contskel = dire;
            en.contdisplay = ref2;
            if (!en.runLoop(snap, true)) {
                en.visor.setIgnore(backignore);
                return false;
            }
        } catch (EngineMessage x) {
            en.contskel = r;
            en.contdisplay = u;
            en.visor.setIgnore(backignore);
            throw x;
        } catch (EngineException x) {
            en.contskel = r;
            en.contdisplay = u;
            en.visor.setIgnore(backignore);
            throw x;
        }
        en.contskel = r;
        en.contdisplay = u;
        if (en.number != snap) {
            /* create choice point */
            en.choices = new ChoiceIgnore(en.choices, snap, r, u);
            en.number++;
        }
        en.visor.setIgnore(backignore);
        return true;
    }

    /****************************************************************/
    /* Port Handling                                                */
    /****************************************************************/

    /**
     * <p>Check whether port is leashed.</p>
     *
     * @param flags The flags.
     * @param port  The port.
     * @return True if the port is leashed, otherwise false.
     */
    public static boolean isPort(int flags, int port) {
        switch (port) {
            case CODE_CALL:
                return ((flags & SpecialDefault.MASK_PORT_CALL) == 0);
            case CODE_FAIL:
                return ((flags & SpecialDefault.MASK_PORT_FAIL) == 0);
            case CODE_EXIT:
                return ((flags & SpecialDefault.MASK_PORT_EXIT) == 0);
            case CODE_REDO:
                return ((flags & SpecialDefault.MASK_PORT_REDO) == 0);
            case CODE_HEAD:
                return ((flags & SpecialDefault.MASK_PORT_HEAD) != 0);
            case CODE_CHOP:
                return ((flags & SpecialDefault.MASK_PORT_CHOP) != 0);
            default:
                throw new IllegalArgumentException("illegal port");
        }
    }

    /**
     * <p>Navigate to the goal.</p>
     *
     * @param port The port.
     * @param en   The engine-
     */
    private static CallFrame getPortDisplay(int port, Engine en) {
        switch (port) {
            case CODE_CALL:
            case CODE_EXIT:
            case CODE_HEAD:
                return en.contdisplay;
            case CODE_FAIL:
            case CODE_REDO:
            case CODE_CHOP:
                return en.contdisplay.contdisplay;
            default:
                throw new IllegalArgumentException("illegal port");
        }
    }

    /**
     * <p>Navigate to the goal.</p>
     *
     * @param port The port.
     * @param u    The display clause-
     */
    private static Intermediate getGoalSkel(int port, CallFrame u) {
        switch (port) {
            case CODE_CALL:
            case CODE_FAIL:
                return u.contskel.next;
            case CODE_EXIT:
            case CODE_REDO:
            case CODE_HEAD:
            case CODE_CHOP:
                return ((Goal)u.contskel).back;
            default:
                throw new IllegalArgumentException("illegal port");
        }
    }

    /**
     * <p>Convert an integer port to a atom port.</p>
     *
     * @param port The integer port.
     * @return The atom port.
     */
    public static SkelAtom portToAtom(int port, Engine en) {
        switch (port) {
            case CODE_CALL:
                return ((FoyerTrace) en.store.foyer).ATOM_CALL;
            case CODE_FAIL:
                return ((FoyerTrace) en.store.foyer).ATOM_FAIL;
            case CODE_EXIT:
                return ((FoyerTrace) en.store.foyer).ATOM_EXIT;
            case CODE_REDO:
                return ((FoyerTrace) en.store.foyer).ATOM_REDO;
            case CODE_HEAD:
                return ((FoyerTrace) en.store.foyer).ATOM_HEAD;
            case CODE_CHOP:
                return ((FoyerTrace) en.store.foyer).ATOM_CHOP;
            default:
                throw new IllegalArgumentException("illegal port");
        }
    }

    /**
     * <p>Convert a atom port to an integer port.</p>
     *
     * @param m The atom port skeleton.
     * @param d The atom port display.
     * @return The integer port.
     */
    public static int atomToPort(Object m, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        if (OP_CALL.equals(fun)) {
            return CODE_CALL;
        } else if (OP_FAIL.equals(fun)) {
            return CODE_FAIL;
        } else if (OP_EXIT.equals(fun)) {
            return CODE_EXIT;
        } else if (OP_REDO.equals(fun)) {
            return CODE_REDO;
        } else if (OP_HEAD.equals(fun)) {
            return CODE_HEAD;
        } else if (OP_CHOP.equals(fun)) {
            return CODE_CHOP;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    EngineMessage.OP_DOMAIN_FLAG_VALUE, m), d);
        }
    }


    /**
     * <p>Check whether we are in debug mode.</p>
     * <p>Can be overridden by subclasses.</p>
     *
     * @return True in debug mode, otherwise false.
     */
    public static boolean isDebug(Engine en) {
        int tflags = en.visor.flags & SpecialDefault.MASK_MODE_DEBG;
        int flags = ((StoreTrace) en.store).flags & SpecialDefault.MASK_MODE_DEBG;
        if ((tflags != SpecialDefault.MASK_DEBG_INHR ? tflags : flags) != 0 &&
                (en.visor.flags & SpecialDefault.MASK_DEBG_NOFL) == 0)
            return true;
        return false;
    }

}
