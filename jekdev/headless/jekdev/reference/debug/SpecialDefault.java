package jekdev.reference.debug;

import jekdev.model.pretty.StoreTrace;
import jekdev.reference.inspection.SpecialFrame;
import jekdev.reference.inspection.SpecialProvable;
import jekdev.reference.system.SpecialMode;
import jekpro.model.builtin.AbstractFlag;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.inter.Predicate;
import jekpro.model.inter.StackElement;
import jekpro.model.molec.*;
import jekpro.model.pretty.Foyer;
import jekpro.model.pretty.StoreKey;
import jekpro.reference.reflect.SpecialPred;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.array.Types;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>Provides a special predicate for default debugger.</p>
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
public final class SpecialDefault extends AbstractSpecial {
    public final static String OP_STEP_IN = "step_in";
    public final static String OP_STEP_OVER = "step_over";
    public final static String OP_STEP_OUT = "step_out";
    public final static String OP_INHERIT = "inherit";

    private final static int SPECIAL_SPY = 0;
    private final static int SPECIAL_NOSPY = 1;
    private final static int SPECIAL_SYS_SPYING = 2;
    private final static int SPECIAL_SYS_NOTRACE_FRAME = 3;
    private final static int SPECIAL_SYS_LEASHED_PORT = 4;

    public final static int MASK_MODE_DEBG = 0x00000F00;
    public final static int MASK_MODE_LESH = 0x00FF0000;
    public final static int MASK_MODE_VIBL = 0xFF000000;

    public final static int MASK_DEBG_NOFL = 0x00000010;

    public final static int MASK_DEBG_DBOF = 0x00000000;
    public final static int MASK_DEBG_STIN = 0x00000100;
    public final static int MASK_DEBG_STVR = 0x00000200;
    public final static int MASK_DEBG_STOT = 0x00000300;
    public final static int MASK_DEBG_DBON = 0x00000400;
    public final static int MASK_DEBG_INHR = 0x00000F00;

    public final static int MASK_PORT_CALL = 0x00000001;
    public final static int MASK_PORT_EXIT = 0x00000002;
    public final static int MASK_PORT_REDO = 0x00000004;
    public final static int MASK_PORT_FAIL = 0x00000008;
    public final static int MASK_PORT_HEAD = 0x00000010;
    public final static int MASK_PORT_CHOP = 0x00000020;
    public final static int MASK_PORT_MASK = 0x0000000F;

    /**
     * <p>Create a default debugger special.</p>
     *
     * @param i The built-in ID.
     */
    public SpecialDefault(int i) {
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
        try {
            switch (id) {
                case SPECIAL_SPY:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    Predicate pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                            ref, en, CachePredicate.MASK_CACH_UCHK);
                    SpecialProvable.checkExistentProvable(pick, temp[0], ref);
                    ((StoreTrace) en.store).addSpyPoint(pick.getFun(), pick.getArity(), pick.getSource().getFullName());
                    return true;
                case SPECIAL_NOSPY:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    pick = SpecialPred.indicatorToPredicateDefined(temp[0],
                            ref, en, CachePredicate.MASK_CACH_UCHK);
                    SpecialProvable.checkExistentProvable(pick, temp[0], ref);
                    ((StoreTrace) en.store).removeSpyPoint(pick.getFun(), pick.getArity(), pick.getSource().getFullName());
                    return true;
                case SPECIAL_SYS_SPYING:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (!BindUniv.unifyTerm(currentSpyPoints(en), Display.DISPLAY_CONST, temp[0], ref, en))
                        return false;
                    return true;
                case SPECIAL_SYS_NOTRACE_FRAME:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    StackElement frame = SpecialFrame.derefAndCastStackElement(temp[0], ref);
                    if (!sysNotraceFrame(frame, en))
                        return false;
                    return true;
                case SPECIAL_SYS_LEASHED_PORT:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    int port = SpecialMode.atomToPort(temp[0], ref);
                    if (!sysLeashedPort(port, en))
                        return false;
                    return true;
                default:
                    throw new IllegalArgumentException(OP_ILLEGAL_SPECIAL);
            }
        } catch (RuntimeException x) {
            throw Types.mapThrowable(x);
        }
    }

    /**************************************************************/
    /* Spypoint Enumeration                                       */
    /**************************************************************/

    /**
     * <p>Create a prolog list with the spy points.</p>
     *
     * @param en The engine.
     * @return The prolog list of the spy points.
     * @throws EngineMessage Shit happens.
     */
    private static Object currentSpyPoints(Engine en)
            throws EngineMessage {
        Object res = en.store.foyer.ATOM_NIL;
        StoreKey[] spypoints = ((StoreTrace) en.store).snapshotSpyPoints();
        for (int i = 0; i < spypoints.length; i++) {
            StoreKey sk = spypoints[i];
            Object decl = SpecialPred.indicatorToColonSkel(
                    sk.getFun(), sk.getArity(), sk.getModule(), en.store.user);
            res = new SkelCompound(en.store.foyer.ATOM_CONS, decl, res);
        }
        return res;
    }

    /**
     * <p>Check whether the frame is a notrace.</p>
     *
     * @param frame The frame.
     * @param en    The engine.
     * @return True if the frame is notrace, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    private static boolean sysNotraceFrame(StackElement frame, Engine en)
            throws EngineException, EngineMessage {
        StackElement.callGoal(frame.contskel, frame.contdisplay, en);
        CachePredicate cp = StackElement.callableToPredicate(en.skel, en);
        if (cp != null && (cp.flags & CachePredicate.MASK_PRED_VISI) != 0 &&
                (cp.pick.getBits() & Predicate.MASK_PRED_NOTR) != 0)
            return true;
        return false;
    }

    /**
     * <p>Check whether the port is leashed.</p>
     *
     * @param port The port.
     * @param en   The engine.
     * @return True if the port is leashed, otherwise false.
     */
    private static boolean sysLeashedPort(int port, Engine en) {
        int tflags = en.visor.flags & SpecialDefault.MASK_MODE_LESH;
        int flags = ((StoreTrace) en.store).flags & SpecialDefault.MASK_MODE_LESH;
        return SpecialMode.isPort((tflags != 0 ? tflags : flags) >> 16, port);
    }

    /*******************************************************************/
    /* Mode Conversion                                                 */
    /*******************************************************************/

    /**
     * <p>Convert a debug mode to an atom.</p>
     *
     * @param m The mode.
     * @return The atom.
     */
    public static Object modeToAtom(int m) {
        switch (m) {
            case MASK_DEBG_INHR:
                return new SkelAtom(OP_INHERIT);
            case MASK_DEBG_DBOF:
                return new SkelAtom(AbstractFlag.OP_OFF);
            case MASK_DEBG_STIN:
                return new SkelAtom(OP_STEP_IN);
            case MASK_DEBG_STVR:
                return new SkelAtom(OP_STEP_OVER);
            case MASK_DEBG_STOT:
                return new SkelAtom(OP_STEP_OUT);
            case MASK_DEBG_DBON:
                return new SkelAtom(AbstractFlag.OP_ON);
            default:
                throw new IllegalArgumentException("illegal mode");
        }
    }

    /**
     * <p>Convert an atom to a debug mode.</p>
     *
     * @param m The atom skeleton.
     * @param d The atom display.
     * @return The debug mode.
     */
    public static int atomToMode(Object m, Display d)
            throws EngineMessage {
        String fun = SpecialUniv.derefAndCastString(m, d);
        if (fun.equals(OP_INHERIT)) {
            return MASK_DEBG_INHR;
        } else if (fun.equals(AbstractFlag.OP_OFF)) {
            return MASK_DEBG_DBOF;
        } else if (fun.equals(OP_STEP_IN)) {
            return MASK_DEBG_STIN;
        } else if (fun.equals(OP_STEP_OVER)) {
            return MASK_DEBG_STVR;
        } else if (fun.equals(OP_STEP_OUT)) {
            return MASK_DEBG_STOT;
        } else if (fun.equals(AbstractFlag.OP_ON)) {
            return MASK_DEBG_DBON;
        } else {
            throw new EngineMessage(EngineMessage.domainError(
                    "debug_mode", m), d);
        }
    }

    /*******************************************************************/
    /* Port List Conversion                                            */
    /*******************************************************************/

    /**
     * <p>Convert leash bits to leash atom list.</p>
     *
     * @param en    The engine.
     * @param flags The bits.
     * @return The atom list.
     */
    public static Object portsToList(Engine en, int flags) {
        Foyer foyer = en.store.foyer;
        Object res = foyer.ATOM_NIL;
        if ((flags & MASK_PORT_FAIL) == 0)
            res = new SkelCompound(foyer.ATOM_CONS, new SkelAtom(SpecialMode.OP_FAIL), res);
        if ((flags & MASK_PORT_REDO) == 0)
            res = new SkelCompound(foyer.ATOM_CONS, new SkelAtom(SpecialMode.OP_REDO), res);
        if ((flags & MASK_PORT_EXIT) == 0)
            res = new SkelCompound(foyer.ATOM_CONS, new SkelAtom(SpecialMode.OP_EXIT), res);
        if ((flags & MASK_PORT_CALL) == 0)
            res = new SkelCompound(foyer.ATOM_CONS, new SkelAtom(SpecialMode.OP_CALL), res);
        if ((flags & MASK_PORT_HEAD) != 0)
            res = new SkelCompound(foyer.ATOM_CONS, new SkelAtom(SpecialMode.OP_HEAD), res);
        if ((flags & MASK_PORT_CHOP) != 0)
            res = new SkelCompound(foyer.ATOM_CONS, new SkelAtom(SpecialMode.OP_CHOP), res);
        return res;
    }

    /**
     * <p>Convert leash atom list to leash bits.</p>
     *
     * @param t The skeleton.
     * @param d The display.
     * @return The bits.
     * @throws EngineMessage Shit happens.
     */
    public static int listToPorts(Object t, Display d) throws EngineMessage {
        int flags = MASK_PORT_MASK;
        BindUniv b;
        while (t instanceof SkelVar &&
                (b = d.bind[((SkelVar) t).id]).display != null) {
            t = b.skel;
            d = b.display;
        }
        while ((t instanceof SkelCompound) &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_CONS) &&
                ((SkelCompound) t).args.length == 2) {
            SkelCompound sc = (SkelCompound) t;
            Display ref = d;
            String fun = SpecialUniv.derefAndCastString(sc.args[0], d);
            if (fun.equals(SpecialMode.OP_CALL)) {
                flags &= ~MASK_PORT_CALL;
            } else if (fun.equals(SpecialMode.OP_EXIT)) {
                flags &= ~MASK_PORT_EXIT;
            } else if (fun.equals(SpecialMode.OP_REDO)) {
                flags &= ~MASK_PORT_REDO;
            } else if (fun.equals(SpecialMode.OP_FAIL)) {
                flags &= ~MASK_PORT_FAIL;
            } else if (fun.equals(SpecialMode.OP_HEAD)) {
                flags |= MASK_PORT_HEAD;
            } else if (fun.equals(SpecialMode.OP_CHOP)) {
                flags |= MASK_PORT_CHOP;
            } else {
                throw new EngineMessage(EngineMessage.domainError("leash", t));
            }
            t = sc.args[1];
            d = ref;
            while (t instanceof SkelVar &&
                    (b = d.bind[((SkelVar) t).id]).display != null) {
                t = b.skel;
                d = b.display;
            }
        }
        if (t instanceof SkelAtom &&
                ((SkelAtom) t).fun.equals(Foyer.OP_NIL)) {
            /* do nothing */
        } else {
            EngineMessage.checkInstantiated(t);
            throw new EngineMessage(EngineMessage.typeError(
                    EngineMessage.OP_TYPE_LIST, t), d);
        }
        return flags;
    }

}
