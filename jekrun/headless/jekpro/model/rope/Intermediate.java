package jekpro.model.rope;

import jekpro.model.builtin.SpecialBody;
import jekpro.model.inter.AbstractDefined;
import jekpro.model.inter.Engine;
import jekpro.model.molec.EngineException;
import jekpro.model.molec.EngineMessage;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>The class provides the base class for intermediate code.</p>
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
public abstract class Intermediate {
    public static final int UNIFY_SKIP = -4;
    public static final int UNIFY_TERM = -3;
    public static final int UNIFY_STRG = -2;
    public static final int UNIFY_WEAK = -1;

    final static int[][] cacheUnifyTerm = new int[8][];

    public Intermediate next;
    public int flags;

    /**
     * <p>Initialize the immutable cache.</p>
     */
    static {
        for (int i = 0; i < 8; i++) {
            int[] array = new int[i];
            for (int j = 0; j < i; j++)
                array[j] = Intermediate.UNIFY_TERM;
            Intermediate.cacheUnifyTerm[i] = array;
        }
    }

    /**
     * <p>Retrieve a term unify array.</p>
     *
     * @param n The length.
     * @return The term unify array.
     */
    static int[] valueOfTerm(int n) {
        if (n < 8)
            return cacheUnifyTerm[n];
        int[] array = new int[n];
        for (int j = 0; j < n; j++)
            array[j] = UNIFY_TERM;
        return array;
    }


    /**
     * <p>Retrieve the next term depending on debug mode.</p>
     * <p>Should be implemented by subclasses.</p>
     *
     * @param en The engine.
     * @return The next term.
     */
    public Intermediate getNextRaw(Engine en) {
        return next;
    }

    /**
     * <p>Resolve the current term.</p>
     *
     * @param en The engine.
     * @return True if success, otherwise false.
     * @throws EngineException Shit happens.
     * @throws EngineMessage   Shit happens.
     */
    public abstract boolean resolveNext(Engine en)
            throws EngineException, EngineMessage;

    /*************************************************************/
    /* Head Structure                                            */
    /*************************************************************/

    /**
     * <p>Set the structure and minarg of the variables in the given term.</p>
     *
     * @param molec  The head skeleton.
     * @param flags  The clause flags.
     * @param helper The helper.
     */
    public static void setHead(Object molec, int flags,
                               SkelVarOptimizable[] helper) {
        if (!(molec instanceof SkelCompound))
            return;
        SkelCompound mc = (SkelCompound) molec;
        for (int i = mc.args.length - 1; i >= 0; i--) {
            Object a = mc.args[i];
            if (a instanceof SkelVar) {
                SkelVarOptimizable ov = helper[((SkelVar) a).id];
                if ((flags & AbstractDefined.MASK_DEFI_NEXV) == 0) {
                    ov.minarg = i;
                } else {
                    ov.flags |= SkelVarOptimizable.MASK_VAR_HSTR;
                }
            } else if (a instanceof SkelCompound) {
                Object var = ((SkelCompound) a).var;
                if (var == null)
                    continue;
                if (var instanceof SkelVar) {
                    SkelVarOptimizable ov = helper[((SkelVar) var).id];
                    ov.flags |= SkelVarOptimizable.MASK_VAR_HSTR;
                } else {
                    SkelVar[] temp = (SkelVar[]) var;
                    for (int j = 0; j < temp.length; j++) {
                        SkelVarOptimizable ov = helper[temp[j].id];
                        ov.flags |= SkelVarOptimizable.MASK_VAR_HSTR;
                    }
                }
            }
        }
    }

    /**
     * <p>Collect the unify arguments.</p>
     *
     * @param molec  The head skeleton.
     * @param helper The helper.
     * @return The unify arguments.
     */
    static int[] unifyArgsTerm(Object molec, SkelVarOptimizable[] helper) {
        if (!(molec instanceof SkelCompound))
            return null;
        SkelCompound mc = (SkelCompound) molec;
        if (helper.length == 0)
            return valueOfTerm(mc.args.length);
        int i = mc.args.length - 1;
        for (; i >= 0; i--) {
            Object a = mc.args[i];
            if (!(a instanceof SkelVar))
                break;
            SkelVarOptimizable ov = helper[((SkelVar) a).id];
            if ((ov.flags & SkelVarOptimizable.MASK_VAR_HSTR) == 0) {
                if (ov.minarg != i) {
                    break;
                } else if ((ov.flags & SkelVarOptimizable.MASK_VAR_GSTR) != 0) {
                    break;
                } else if ((ov.flags & SkelVarOptimizable.MASK_VAR_BODY) != 0) {
                    break;
                } else {
                    /* */
                }
            } else {
                break;
            }
        }
        if (!(i >= 0))
            return null;
        int[] intargs = new int[i + 1];
        for (; i >= 0; i--) {
            Object a = mc.args[i];
            if (!(a instanceof SkelVar)) {
                intargs[i] = UNIFY_TERM;
                continue;
            }
            SkelVarOptimizable ov = helper[((SkelVar) a).id];
            if ((ov.flags & SkelVarOptimizable.MASK_VAR_HSTR) == 0) {
                if (ov.minarg != i) {
                    intargs[i] = ov.minarg;
                } else if ((ov.flags & SkelVarOptimizable.MASK_VAR_GSTR) != 0) {
                    intargs[i] = UNIFY_STRG;
                } else if ((ov.flags & SkelVarOptimizable.MASK_VAR_BODY) != 0) {
                    intargs[i] = UNIFY_WEAK;
                } else {
                    intargs[i] = UNIFY_SKIP;
                }
            } else {
                intargs[i] = UNIFY_TERM;
            }
        }
        return intargs;
    }

    /*************************************************************/
    /* Body Structure                                            */
    /*************************************************************/

    /**
     * <p>Set the goals structure flag.</p>
     *
     * @param b      The body skeleton.
     * @param flags  The clause flags.
     * @param helper The helper.
     * @param en     The engine.
     */
    public static void setBody(Object b, int flags,
                               SkelVarOptimizable[] helper, Engine en) {
        for (; ; ) {
            if (!Goal.noBody(b)) {
                Object t = Goal.bodyToGoalSkel(b);
                if (SpecialBody.alterType(t) != SpecialBody.TYPE_ALTR_NONE) {
                    setDisj(t, flags, helper, en);
                } else if (SpecialBody.sequenType(t) != SpecialBody.TYPE_SEQN_NONE) {
                    setBody(t, flags, helper, en);
                } else {
                    if (t instanceof SkelVar) {
                        setVariable((SkelVar)t, flags, helper);
                    } else {
                        setGoal(t, flags, helper);
                    }
                }
            } else {
                break;
            }
            b = Goal.bodyToRestSkel(b, en);
        }
    }

    /**
     * <p>Set the goals structure flag.</p>
     *
     * @param t      The disjunction skeleton.
     * @param flags  The clause flags.
     * @param helper The helper.
     * @param en     The engine.
     */
    private static void setDisj(Object t, int flags,
                                SkelVarOptimizable[] helper, Engine en) {
        L1:
        for (; ; ) {
            switch (SpecialBody.alterType(t)) {
                case SpecialBody.TYPE_ALTR_DISJ:
                    SkelCompound sc = (SkelCompound) t;
                    Object b = sc.args[0];
                    switch (SpecialBody.alterType(b)) {
                        case SpecialBody.TYPE_ALTR_COND:
                        case SpecialBody.TYPE_ALTR_SOFT:
                            SkelCompound sc2 = (SkelCompound) b;
                            setBody(sc2.args[0], flags, helper, en);
                            setBody(sc2.args[1], flags, helper, en);
                            break;
                        default:
                            setBody(b, flags, helper, en);
                            break;
                    }
                    t = sc.args[1];
                    break;
                case SpecialBody.TYPE_ALTR_COND:
                case SpecialBody.TYPE_ALTR_SOFT:
                    sc = (SkelCompound) t;
                    setBody(sc.args[0], flags, helper, en);
                    setBody(sc.args[1], flags, helper, en);
                    break L1;
                default:
                    setBody(t, flags, helper, en);
                    break L1;
            }
        }
    }

    /**
     * <p>Set the goal structure flag.</p>
     *
     * @param molec  The goal skeleton.
     * @param flags  The clause flags.
     * @param helper The helper.
     */
    private static void setGoal(Object molec, int flags,
                                SkelVarOptimizable[] helper) {
        if (!(molec instanceof SkelCompound))
            return;
        SkelCompound mc = (SkelCompound) molec;
        for (int i = mc.args.length - 1; i >= 0; i--) {
            Object a = mc.args[i];
            if (a instanceof SkelVar) {
                setVariable((SkelVar) a, flags, helper);
            } else if (a instanceof SkelCompound) {
                Object var = ((SkelCompound) a).var;
                if (var == null)
                    continue;
                if (var instanceof SkelVar) {
                    SkelVarOptimizable ov = helper[((SkelVar) var).id];
                    ov.flags |= SkelVarOptimizable.MASK_VAR_GSTR;
                } else {
                    SkelVar[] temp = (SkelVar[]) var;
                    for (int j = 0; j < temp.length; j++) {
                        SkelVarOptimizable ov = helper[temp[j].id];
                        ov.flags |= SkelVarOptimizable.MASK_VAR_GSTR;
                    }
                }
            }
        }
    }

    /**
     * <p>Set the variable structure flag.</p>
     *
     * @param mv  The variable skeleton.
     * @param flags  The clause flags.
     * @param helper The helper.
     */
    private static void setVariable(SkelVar mv, int flags,
                                    SkelVarOptimizable[] helper) {
        SkelVarOptimizable ov = helper[mv.id];
        if ((flags & AbstractDefined.MASK_DEFI_NWKV) == 0) {
            ov.flags |= SkelVarOptimizable.MASK_VAR_BODY;
        } else {
            ov.flags |= SkelVarOptimizable.MASK_VAR_GSTR;
        }
    }


}
