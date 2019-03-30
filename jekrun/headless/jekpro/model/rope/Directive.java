package jekpro.model.rope;

import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.tools.array.AbstractDelegate;
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
    public final static int MASK_DIRE_NIST = 0x00000001;

    public final static int MASK_DIRE_NOBR = 0x00000010;
    public final static int MASK_DIRE_STOP = 0x00000020;
    public final static int MASK_DIRE_NBDY = 0x00000040;
    public final static int MASK_DIRE_NLST = 0x00000080;

    public final static int MASK_DIRE_CALL = MASK_DIRE_NOBR |
            MASK_DIRE_STOP | MASK_DIRE_NBDY | MASK_DIRE_NLST;

    public final static int MASK_FIXUP_MOVE = 0x00000001;
    public final static int MASK_FIXUP_MARK = 0x00000002;

    public int size;
    public Intermediate last;

    /**
     * <p>Create a directive.</p>
     *
     * @param copt The directive option flags.
     */
    public Directive(int copt) {
        if ((copt & AbstractDefined.MASK_DEFI_NLST) != 0)
            flags |= MASK_DIRE_NLST;
        if ((copt & AbstractDefined.MASK_DEFI_STOP) != 0)
            flags |= MASK_DIRE_STOP;
        if ((copt & AbstractDefined.MASK_DEFI_NBDY) != 0)
            flags |= MASK_DIRE_NBDY;
        if ((copt & AbstractDelegate.MASK_DELE_NOBR) != 0)
            flags |= MASK_DIRE_NOBR;
        if ((copt & AbstractDefined.MASK_DEFI_NIST) != 0)
            flags |= MASK_DIRE_NIST;
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
     * @param body  The term list, or null.
     * @param en    The engine.
     * @param close The close flag.
     */
    public void bodyToInter(Object body, Engine en, boolean close) {
        Goal.bodyToInter(this, body, en);
        if (close)
            addInter(Success.DEFAULT, MASK_FIXUP_MARK);
    }

    /**************************************************************/
    /* Convert from Intermediate Form                             */
    /**************************************************************/

    /**
     * <p>Convert the intermediate form into a term.</p>
     *
     * @param en The store.
     * @return The vector.
     */
    public Object interToBody(Engine en) {
        return interToBody(this, last, en);
    }

    /**
     * <p>Convert the intermediate form into a term.</p>
     *
     * @param en The store.
     * @return The vector.
     */
    public Object interToBranch(Engine en) {
        return interToBranch(this, last, en);
    }

    /**
     * <p>Convert the intermediate form into a term.</p>
     * <p>Will skip begin and commit nodes.</p>
     *
     * @param last The conversion end.
     * @param en   The store.
     * @return The vector.
     */
    private static Object interToBody(Intermediate temp, Intermediate last,
                                      Engine en) {
        SkelCompound back = null;
        Object t = null;
        if (last != null) {
            do {
                temp = temp.next;
                Object left = temp.term;
                if (isAlternative(left)) {
                    left = alternativeToDisjunction(left, en);
                } else if (isBegin(left) || isCommit(left)) {
                    continue;
                }
                if (t != null) {
                    Object[] args = new Object[2];
                    args[0] = t;
                    args[1] = back;
                    back = new SkelCompound(en.store.foyer.ATOM_COMMA, args, null);
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
     * <p>Convert the intermediate form into a term.</p>
     * <p>Will split a sequence.</p>
     *
     * @param temp The conversion start.
     * @param last The conversion end.
     * @param en   The store.
     * @return The vector.
     */
    private static Object interToBranch(Intermediate temp, Intermediate last,
                                        Engine en) {
        if (last != null && isBegin(temp.next.term)) {
            Intermediate split = findSplit(temp, last);
            Object left = interToBody(temp, split, en);
            Object right = interToBody(split, last, en);
            return new SkelCompound(en.store.foyer.ATOM_TESTING, left, right);
        } else {
            return interToBody(temp, last, en);
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
                if (isCommit(temp.term))
                    return back;
            } while (temp != last);
        }
        return null;
    }

    /**
     * <p>Convert an alternative to a disjunction.</p>
     *
     * @param term The alternative.
     * @param en   The engine.
     * @return The disjunction.
     */
    private static Object alternativeToDisjunction(Object term, Engine en) {
        SkelCompound back = null;
        do {
            SkelCompound sc = (SkelCompound) term;
            Object left = ((Directive) sc.args[0]).interToBranch(en);
            Object[] args = new Object[2];
            args[0] = left;
            args[1] = back;
            back = new SkelCompound(en.store.foyer.ATOM_SEMICOLON, args, null);
            term = sc.args[1];
        } while (isAlternative(term));
        Object t = ((Directive) term).interToBranch(en);
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
     * <p>Check whether the given term is an alternative.</p>
     *
     * @param term The term.
     * @return True if the term is an alterantive.
     */
    public static boolean isAlternative(Object term) {
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
    public static boolean isBegin(Object term) {
        if (term instanceof SkelAtom &&
                ((SkelAtom) term).fun.equals(Foyer.OP_SYS_BEGIN)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Check whether the given term is a commit.</p>
     *
     * @param term The term.
     * @return True if the term is a commit.
     */
    public static boolean isCommit(Object term) {
        if (term instanceof SkelAtom &&
                ((SkelAtom) term).fun.equals(Foyer.OP_SYS_COMMIT)) {
            return true;
        } else {
            return false;
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
            if (isAlternative(last.term)) {
                Object term = last.term;
                do {
                    SkelCompound sc = (SkelCompound) term;
                    ((Directive) sc.args[0]).addInter(inter, mask & MASK_FIXUP_MARK);
                    term = sc.args[1];
                } while (isAlternative(term));
                ((Directive) term).addInter(inter, mask & MASK_FIXUP_MARK);
            }
            last.next = inter;
            if ((mask & MASK_FIXUP_MARK) != 0) {
                if ((flags & Directive.MASK_DIRE_STOP) == 0)
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