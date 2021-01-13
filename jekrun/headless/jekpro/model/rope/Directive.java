package jekpro.model.rope;

import jekpro.frequent.standard.SupervisorCall;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

/**
 * <p>The class provides a directive node.</p>
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
public class Directive extends Intermediate {
    public final static int MASK_DIRE_MORE = 0x00000001;
    public final static int MASK_DIRE_LTGC = 0x00000002;
    public final static int MASK_DIRE_SOFT = 0x00000004;

    public final static int MASK_DIRE_PUSH = MASK_DIRE_LTGC |
            AbstractDefined.MASK_DEFI_NSTK;

    public final static int MASK_DIRE_CALL = MASK_DIRE_MORE |
            MASK_DIRE_LTGC | MASK_DIRE_SOFT | AbstractDefined.MASK_DEFI_NOBR |
            AbstractDefined.MASK_DEFI_CALL | AbstractDefined.MASK_DEFI_NBCV |
            AbstractDefined.MASK_DEFI_NIST | AbstractDefined.MASK_DEFI_NHED;

    public final static int MASK_FIXUP_MOVE = 0x00000001;
    public final static int MASK_FIXUP_MARK = 0x00000002;

    public final static int TYPE_CTRL_BEGN = 0;
    public final static int TYPE_CTRL_CMMT = 1;
    public final static int TYPE_CTRL_SBGN = 2;
    public final static int TYPE_CTRL_SCMT = 3;
    public final static int TYPE_CTRL_NONE = 4;

    public Intermediate last;

    /**
     * <p>Create a directive.</p>
     *
     * @param copt The directive option flags.
     */
    public Directive(int copt) {
        flags = copt & Directive.MASK_DIRE_CALL;
    }

    /**
     * <p>Create a directive.</p>
     *
     * @param copt The clause option flags.
     * @param en   The engine.
     * @return The clause.
     */
    public static Directive createDirective(int copt, Engine en) {
        if ((copt & AbstractDefined.MASK_DEFI_NIST) == 0) {
            return en.store.foyer.createDirective(copt);
        } else {
            return new Directive(copt);
        }
    }

    /**
     * <p>Resolve the current term.</p>
     *
     * @param en The engine.
     * @return True if success, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public boolean resolveNext(Engine en)
            throws EngineException, EngineMessage {
        throw new IllegalArgumentException("not supported");
    }

    /**
     * <p>Convert a body to intermediate form.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param b     The body skeleton.
     * @param en    The engine.
     * @param close The close flag.
     * @throws EngineMessage Shit happens.
     */
    public void bodyToInterSkel(Object b, Engine en, boolean close)
            throws EngineMessage {
        Goal.bodyToInterSkel(this, b, en);
        if (close)
            addInter(Success.DEFAULT, MASK_FIXUP_MARK);
    }

    /**
     * <p>Convert a body to intermediate form.</p>
     * <p>Can be overridden by sub classes.</p>
     *
     * @param b     The body skeleton.
     * @param c     The body display.
     * @param en    The engine.
     * @param close The close flag.
     * @throws EngineMessage Shit happens.
     */
    public void bodyToInter(Object b, Display c, Engine en, boolean close)
            throws EngineMessage {
        SupervisorCall ec = en.visor.getCall();
        ec.bodyToInter(this, b, c, en);
        if (close)
            addInter(Success.DEFAULT, MASK_FIXUP_MARK);
    }

    /**************************************************************/
    /* Convert from Intermediate Form                             */
    /**************************************************************/

    /**
     * <p>Convert the intermediate form into a term.</p>
     * <p>Will skip begin and commit nodes.</p>
     *
     * @param last The conversion end.
     * @param en   The store.
     * @return The skeleton.
     */
    public static Object interToBodySkel(Intermediate temp, Intermediate last,
                                         Engine en) {
        SkelCompound back = null;
        Object t = null;
        if (last != null) {
            do {
                temp = temp.next;
                Object left = ((Goal) temp).term;
                if (isAlter(left) || isGuard(left)) {
                    left = Directive.alterToDisjSkel(left, en);
                } else if (isSequen(left)) {
                    SkelCompound sc = (SkelCompound) left;
                    left = Directive.interToBranchSkel((Directive) sc.args[0], en);
                } else if (controlType(left) != TYPE_CTRL_NONE) {
                    continue;
                }
                if (t != null) {
                    Object[] args = new Object[2];
                    args[0] = t;
                    args[1] = back;
                    back = new SkelCompound(args, en.store.foyer.ATOM_COMMA);
                }
                t = left;
            } while (temp != last);
        }
        if (t == null)
            t = en.store.foyer.ATOM_TRUE;
        while (back != null) {
            SkelCompound jack = (SkelCompound) back.args[back.args.length - 1];
            back.args[back.args.length - 1] = t;
            back.var = SkelCompound.makeExtra(back.args);
            t = back;
            back = jack;
        }
        return t;
    }

    /**
     * <p>Convert an alternative to a disjunction.</p>
     *
     * @param term The alternative skeleton.
     * @param en   The engine.
     * @return The disjunction skeleton.
     */
    private static Object alterToDisjSkel(Object term, Engine en) {
        SkelCompound back = null;
        while (isAlter(term)) {
            SkelCompound sc = (SkelCompound) term;
            Object left = Directive.interToBranchSkel((Directive) sc.args[0], en);
            Object[] args = new Object[2];
            args[0] = left;
            args[1] = back;
            back = new SkelCompound(args, en.store.foyer.ATOM_SEMICOLON);
            term = sc.args[1];
        }
        Object t;
        if (isGuard(term)) {
            SkelCompound sc = (SkelCompound) term;
            t = Directive.interToBranchSkel((Directive) sc.args[0], en);
        } else {
            t = Directive.interToBodySkel(((Directive) term), ((Directive) term).last, en);
        }
        while (back != null) {
            SkelCompound jack = (SkelCompound) back.args[back.args.length - 1];
            back.args[back.args.length - 1] = t;
            back.var = SkelCompound.makeExtra(back.args);
            t = back;
            back = jack;
        }
        return t;
    }

    /**
     * <p>Convert the intermediate form into a term.</p>
     *
     * @param en The store.
     * @return The skeleton.
     */
    private static Object interToBranchSkel(Directive dire, Engine en) {
        int type = TYPE_CTRL_NONE;
        if (dire.last != null && (type = controlType(((Goal) dire.next).term)) == TYPE_CTRL_BEGN) {
            Intermediate split = findSplit(dire, dire.last);
            Object left = Directive.interToBodySkel(dire, split, en);
            Object right = Directive.interToBodySkel(split, dire.last, en);
            return new SkelCompound(en.store.foyer.ATOM_CONDITION, left, right);
        } else if (dire.last != null && type == TYPE_CTRL_SBGN) {
            Intermediate split = findSoftSplit(dire, dire.last);
            Object left = Directive.interToBodySkel(dire, split, en);
            Object right = Directive.interToBodySkel(split, dire.last, en);
            return new SkelCompound(en.store.foyer.ATOM_SOFT_CONDITION, left, right);
        } else {
            return Directive.interToBodySkel(dire, dire.last, en);
        }
    }

    /**
     * <p>Find the split inside a sequence.</p>
     *
     * @param temp The conversion start.
     * @param last The conversion end.
     * @return The split, or null.
     */
    private static Intermediate findSplit(Intermediate temp, Intermediate last) {
        if (last != null) {
            do {
                Intermediate back = temp;
                temp = back.next;
                if (controlType(((Goal) temp).term) == TYPE_CTRL_CMMT)
                    return back;
            } while (temp != last);
        }
        return null;
    }

    /**
     * <p>Find the split inside a sequence.</p>
     *
     * @param temp The conversion start.
     * @param last The conversion end.
     * @return The split, or null.
     */
    private static Intermediate findSoftSplit(Intermediate temp, Intermediate last) {
        if (last != null) {
            do {
                Intermediate back = temp;
                temp = back.next;
                if (controlType(((Goal) temp).term) == TYPE_CTRL_SCMT)
                    return back;
            } while (temp != last);
        }
        return null;
    }

    /**
     * <p>Check whether the given term is an alternative.</p>
     *
     * @param term The term.
     * @return True if the term is an alterantive.
     */
    public static boolean isAlter(Object term) {
        if (term instanceof SkelCompound &&
                ((SkelCompound) term).args.length == 2 &&
                ((SkelCompound) term).sym.fun.equals(Foyer.OP_SYS_ALTER)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Check whether the given term is an alternative.</p>
     *
     * @param term The term.
     * @return True if the term is an alterantive.
     */
    public static boolean isGuard(Object term) {
        if (term instanceof SkelCompound &&
                ((SkelCompound) term).args.length == 1 &&
                ((SkelCompound) term).sym.fun.equals(Foyer.OP_SYS_GUARD)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Check whether the given term is a sequent.</p>
     *
     * @param term The term.
     * @return True if the term is a sequent.
     */
    public static boolean isSequen(Object term) {
        if (term instanceof SkelCompound &&
                ((SkelCompound) term).args.length == 1 &&
                ((SkelCompound) term).sym.fun.equals(Foyer.OP_SYS_SEQUEN)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Determine the control type.</p>
     *
     * @param term The term.
     * @return The control type.
     */
    public static int controlType(Object term) {
        if (term instanceof SkelAtom &&
                ((SkelAtom) term).fun.equals(Foyer.OP_SYS_BEGIN)) {
            return TYPE_CTRL_BEGN;
        } else if (term instanceof SkelAtom &&
                ((SkelAtom) term).fun.equals(Foyer.OP_SYS_COMMIT)) {
            return TYPE_CTRL_CMMT;
        } else if (term instanceof SkelAtom &&
                ((SkelAtom) term).fun.equals(Foyer.OP_SYS_SOFT_BEGIN)) {
            return TYPE_CTRL_SBGN;
        } else if (term instanceof SkelAtom &&
                ((SkelAtom) term).fun.equals(Foyer.OP_SYS_SOFT_COMMIT)) {
            return TYPE_CTRL_SCMT;
        } else {
            return TYPE_CTRL_NONE;
        }
    }

    /**************************************************************/
    /* Builder Utilities                                          */
    /**************************************************************/

    /**
     * <p>Add a goal to the directive.</p>
     *
     * @param inter The intermediate.
     * @param mask  The flag.
     */
    public final void addInter(Intermediate inter, int mask) {
        if (last == null) {
            next = inter;
        } else {
            Object term = ((Goal) last).term;
            if (isAlter(term) || isGuard(term)) {
                while (isAlter(term)) {
                    SkelCompound sc = (SkelCompound) term;
                    ((Directive) sc.args[0]).addInter(inter, mask & MASK_FIXUP_MARK);
                    term = sc.args[1];
                }
                if (isGuard(term)) {
                    SkelCompound sc = (SkelCompound) term;
                    ((Directive) sc.args[0]).addInter(inter, mask & MASK_FIXUP_MARK);
                } else {
                    ((Directive) term).addInter(inter, mask & MASK_FIXUP_MARK);
                }
            } else if (isSequen(term)) {
                SkelCompound sc = (SkelCompound) term;
                ((Directive) sc.args[0]).addInter(inter, mask & MASK_FIXUP_MARK);
            }
            last.next = inter;
            if ((mask & MASK_FIXUP_MARK) != 0) {
                if ((flags & AbstractDefined.MASK_DEFI_NSTK) == 0)
                    last.flags |= Goal.MASK_GOAL_CEND;
            }
        }
        if ((mask & MASK_FIXUP_MOVE) != 0)
            last = inter;
    }

    /**
     * <p>Add a goal to the directive.</p>
     *
     * @param inter The intermediate.
     * @param flags The flag.
     */
    public void addInterTrace(Intermediate inter, int flags) {

    }

}