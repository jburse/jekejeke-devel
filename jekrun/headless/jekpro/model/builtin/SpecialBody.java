package jekpro.model.builtin;

import jekpro.frequent.standard.SupervisorCall;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.CallFrame;
import jekpro.model.molec.Display;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.model.pretty.Foyer;
import jekpro.model.rope.Directive;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;

/**
 * <p>Provides built-in predicates for body execution.</p>
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
public final class SpecialBody extends AbstractSpecial {
    private final static int SPECIAL_CALL = 0;
    private final static int SPECIAL_SYS_ALTER = 1;
    private final static int SPECIAL_SYS_GUARD = 2;
    private final static int SPECIAL_SYS_SEQUEN = 3;
    private final static int SPECIAL_SYS_BEGIN = 4;
    private final static int SPECIAL_SYS_COMMIT = 5;
    private final static int SPECIAL_SYS_SOFT_BEGIN = 6;
    private final static int SPECIAL_SYS_SOFT_COMMIT = 7;

    public final static int TYPE_ALTR_DISJ = 0;
    public final static int TYPE_ALTR_COND = 1;
    public final static int TYPE_ALTR_SOFT = 2;
    public final static int TYPE_ALTR_NONE = 3;

    public final static int TYPE_SEQN_CONJ = 0;
    public final static int TYPE_SEQN_TRUE = 1;
    public final static int TYPE_SEQN_NONE = 2;

    public final static int TYPE_CTRL_BEGN = 0;
    public final static int TYPE_CTRL_CMMT = 1;
    public final static int TYPE_CTRL_SBGN = 2;
    public final static int TYPE_CTRL_SCMT = 3;
    public final static int TYPE_CTRL_NONE = 4;

    /**
     * <p>Create a body special.</p>
     *
     * @param i The id of the special.
     */
    public SpecialBody(int i) {
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
            case SPECIAL_CALL:
                Object[] temp = ((SkelCompound) en.skel).args;
                Display ref = en.display;
                en.skel = temp[0];
                en.display = ref;
                en.deref();

                Directive dire = SupervisorCall.callGoal(0, en);
                Display d2 = en.display;

                CallFrame ref2 = CallFrame.getFrame(d2, dire, en);
                en.contskel = dire;
                en.contdisplay = ref2;
                return true;
            case SPECIAL_SYS_ALTER:
                temp = ((SkelCompound) en.skel).args;
                en.choices = new ChoiceAlter(en.choices, temp[1], en.contskel,
                        en.contdisplay, en.bind);
                en.number++;
                en.contskel = (Directive) temp[0];
                return true;
            case SPECIAL_SYS_GUARD:
                temp = ((SkelCompound) en.skel).args;
                en.choices = new ChoiceAlter(en.choices, null, en.contskel,
                        en.contdisplay, en.bind);
                en.number++;
                en.contskel = (Directive) temp[0];
                return true;
            case SPECIAL_SYS_SEQUEN:
                temp = ((SkelCompound) en.skel).args;
                en.contskel = (Directive) temp[0];
                return true;
            case SPECIAL_SYS_BEGIN:
                ChoiceAlter cp = (ChoiceAlter) en.choices;
                ref2 = en.contdisplay;
                if ((ref2.flags & AbstractDefined.MASK_DEFI_NOBR) != 0) {
                    cp.flags |= ChoiceAlter.MASK_CALT_BACK;
                    ref2.flags &= ~AbstractDefined.MASK_DEFI_NOBR;
                }
                cp.number = ref2.number;
                ref2.number = en.number;
                return true;
            case SPECIAL_SYS_COMMIT:
                ref2 = en.contdisplay;
                en.fault = null;
                en.cutChoices(ref2.number - 1);
                if (en.fault != null)
                    throw en.fault;
                return true;
            case SPECIAL_SYS_SOFT_BEGIN:
                ref2 = en.contdisplay;
                ref2 = new CallFrame(ref2.disp, en);
                ref2.flags |= Directive.MASK_DIRE_PUSH;
                en.contdisplay = ref2;
                return true;
            case SPECIAL_SYS_SOFT_COMMIT:
                ref2 = en.contdisplay;
                en.contdisplay = ref2.contdisplay;
                if (ref2.number >= en.number) {
                    en.fault = null;
                    en.cutChoices(ref2.number - 1);
                    if (en.fault != null)
                        throw en.fault;
                } else {
                    ref2 = en.contdisplay;
                    ref2.flags |= Directive.MASK_DIRE_SOFT;
                }
                return true;
            default:
                throw new IllegalArgumentException(
                        AbstractSpecial.OP_ILLEGAL_SPECIAL);
        }
    }

    /**************************************************************/
    /* Body Iterator                                              */
    /**************************************************************/

    /**
     * <p>Check whether the body has no goal.</p>
     *
     * @param t The body skeleton.
     * @return True if the body has no goal, false otherwise.
     */
    public static boolean noBody(Object t) {
        return (t instanceof SkelAtom &&
                ((SkelAtom) t).fun.equals(Foyer.OP_TRUE));
    }

    /**
     * <p>Convert a body to a first goal.</p>
     *
     * @param t The body skeleton.
     * @return The goal skeleton.
     */
    public static Object bodyToGoalSkel(Object t) {
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_COMMA)) {
            SkelCompound sc = (SkelCompound) t;
            return sc.args[0];
        } else {
            return t;
        }
    }

    /**
     * <p>Convert a body to a rest body.</p>
     *
     * @param t  The body skeleton.
     * @param en The engine.
     * @return The body skeleton.
     */
    public static Object bodyToRestSkel(Object t, Engine en) {
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_COMMA)) {
            SkelCompound sc = (SkelCompound) t;
            return sc.args[1];
        } else {
            return en.store.foyer.ATOM_TRUE;
        }
    }
    
    /**************************************************************/
    /* Body Checkers                                              */
    /**************************************************************/

    /**
     * <p>Determine the alter type.</p>
     *
     * @param t The goal skeleton.
     * @return The alter type.
     */
    public static int alterType(Object t) {
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SEMICOLON)) {
            return TYPE_ALTR_DISJ;
        } else if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_CONDITION)) {
            return TYPE_ALTR_COND;
        } else if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_SOFT_CONDITION)) {
            return TYPE_ALTR_SOFT;
        } else {
            return TYPE_ALTR_NONE;
        }
    }

    /**
     * <p>Determine the sequen type.</p>
     *
     * @param t The goal skeleton.
     * @return The sequen type.
     */
    public static int sequenType(Object t) {
        if (t instanceof SkelCompound &&
                ((SkelCompound) t).args.length == 2 &&
                ((SkelCompound) t).sym.fun.equals(Foyer.OP_COMMA)) {
            return TYPE_SEQN_CONJ;
        } else if (t instanceof SkelAtom &&
                ((SkelAtom) t).fun.equals(Foyer.OP_TRUE)) {
            return TYPE_SEQN_TRUE;
        } else {
            return TYPE_SEQN_NONE;
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

}
