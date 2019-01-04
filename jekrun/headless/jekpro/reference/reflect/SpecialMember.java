package jekpro.reference.reflect;

import jekpro.frequent.standard.EngineCopy;
import jekpro.model.inter.AbstractSpecial;
import jekpro.model.inter.Engine;
import jekpro.model.molec.*;
import jekpro.reference.arithmetic.SpecialEval;
import jekpro.reference.structure.SpecialUniv;
import jekpro.tools.term.SkelAtom;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>Provides built-in predicates for the module member.</p>
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
public final class SpecialMember extends AbstractSpecial {
    private final static int SPECIAL_UNIFY = 0;
    private final static int SPECIAL_VAR = 1;
    private final static int SPECIAL_NONVAR = 2;
    private final static int SPECIAL_GROUND = 3;
    private final static int SPECIAL_SYS_FUNCTOR_TO_TERM = 4;
    private final static int SPECIAL_SYS_TERM_TO_FUNCTOR = 5;

    /**
     * <p>Create a zygote special.</p>
     *
     * @param i The id.
     */
    public SpecialMember(int i) {
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
                case SPECIAL_UNIFY:
                    Object[] temp = ((SkelCompound) en.skel).args;
                    Display ref = en.display;
                    if (!en.unifyTerm(temp[1], ref, temp[0], ref))
                        return false;
                    return en.getNext();
                case SPECIAL_VAR:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (!isVar(temp[0], ref))
                        return false;
                    return en.getNextRaw();
                case SPECIAL_NONVAR:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (isVar(temp[0], ref))
                        return false;
                    return en.getNextRaw();
                case SPECIAL_GROUND:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    if (!isGround(temp[0], ref))
                        return false;
                    return en.getNextRaw();
                case SPECIAL_SYS_FUNCTOR_TO_TERM:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    Number num = SpecialEval.derefAndCastInteger(temp[1], ref);
                    SpecialEval.checkNotLessThanZero(num);
                    int arity = SpecialEval.castIntValue(num);

                    Display d;
                    boolean multi;
                    if (arity != 0) {
                        SkelAtom sa = SpecialUniv.derefAndCastStringWrapped(temp[0], ref);
                        SkelVar[] vars = SkelVar.valueOfArray(arity);
                        Object[] args = new Object[arity];
                        System.arraycopy(vars, 0, args, 0, arity);
                        en.skel = new SkelCompound(sa, args, (arity > 1 ? vars : vars[0]));
                        d = new Display(Display.newBind(arity));
                        multi = true;
                    } else {
                        en.skel = temp[0];
                        en.display = ref;
                        en.deref();
                        if (!(en.skel instanceof SkelVar) && !(en.skel instanceof SkelCompound)) {
                            /* ok */
                        } else {
                            EngineMessage.checkInstantiated(en.skel);
                            throw new EngineMessage(EngineMessage.typeError(
                                    EngineMessage.OP_TYPE_ATOMIC, en.skel), en.display);
                        }
                        d = Display.DISPLAY_CONST;
                        multi = false;
                    }
                    if (!en.unifyTerm(temp[2], ref, en.skel, d))
                        return false;
                    if (multi)
                        BindCount.remTab(d.bind, en);
                    return en.getNext();
                case SPECIAL_SYS_TERM_TO_FUNCTOR:
                    temp = ((SkelCompound) en.skel).args;
                    ref = en.display;
                    en.skel = temp[0];
                    en.display = ref;
                    en.deref();
                    Object obj;
                    if (en.skel instanceof SkelCompound) {
                        SkelCompound sc = (SkelCompound) en.skel;
                        obj = sc.sym;
                        num = Integer.valueOf(sc.args.length);
                    } else {
                        EngineMessage.checkInstantiated(en.skel);
                        obj = en.skel;
                        num = Integer.valueOf(0);
                    }
                    if (!en.unifyTerm(temp[1], ref, obj, en.display))
                        return false;
                    if (!en.unifyTerm(temp[2], ref, num, Display.DISPLAY_CONST))
                        return false;
                    return en.getNext();
                default:
                    throw new IllegalArgumentException(AbstractSpecial.OP_ILLEGAL_SPECIAL);
            }
        } catch (ClassCastException x) {
            throw new EngineMessage(
                    EngineMessage.representationError(x.getMessage()));
        }
    }

    /**
     * <p>Check whether the argument is a variable.</p>
     *
     * @param alfa The term skeleton.
     * @param d1   The term display.
     * @return True if a variable, otherwise false.
     */
    private static boolean isVar(Object alfa, Display d1) {
        for (; ; ) {
            if (alfa instanceof SkelVar) {
                // combined check and deref
                BindVar b1;
                if ((b1 = d1.bind[((SkelVar) alfa).id]).display != null) {
                    alfa = b1.skel;
                    d1 = b1.display;
                    continue;
                }
                return true;
            } else {
                return false;
            }
        }
    }

    /**
     * <p>Check whether the given term is ground.</p>
     * <p>Tail recursive solution.</p>
     *
     * @param t The term skel.
     * @param d The term display.
     * @return True if the term is ground, otherwise false.
     */
    private static boolean isGround(Object t, Display d) {
        for (; ; ) {
            Object var = EngineCopy.getVar(t);
            if (var == null)
                return true;
            SkelVar v;
            if (var instanceof SkelVar) {
                v = (SkelVar) var;
            } else {
                SkelVar[] temp = (SkelVar[]) var;
                int j = 0;
                for (; j < temp.length - 1; j++) {
                    v = temp[j];
                    BindVar b = d.bind[v.id];
                    if (b.display != null) {
                        if (!isGround(b.skel, b.display))
                            return false;
                    } else {
                        return false;
                    }
                }
                v = temp[j];
            }
            BindVar b = d.bind[v.id];
            if (b.display != null) {
                t = b.skel;
                d = b.display;
            } else {
                return false;
            }
        }
    }

}

