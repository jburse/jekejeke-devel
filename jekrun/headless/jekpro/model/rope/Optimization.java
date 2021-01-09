package jekpro.model.rope;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.model.inter.AbstractDefined;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelCompoundLineable;
import jekpro.tools.term.SkelVar;

/**
 * <p>The class used to analyse a variable.</p>
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
public final class Optimization {
    private final static Optimization[] VAR_VOID = new Optimization[0];

    public static final int UNIFY_CLASH = -1;
    public static final int UNIFY_TERM = -2;
    public static final int UNIFY_LINEAR = -3;
    public static final int UNIFY_SKIP = -4;

    final static int MASK_VAR_HSTR = 0x00000001;
    final static int MASK_VAR_BODY = 0x00000002;

    final static int[][] cacheUnifyLinear = new int[8][];

    int flags;
    int minarg = -1;
    final SkelVar sort;

    /**
     * <p>Initialize the immutable cache.</p>
     */
    static {
        for (int i = 0; i < 8; i++) {
            int[] array = new int[i];
            for (int j = 0; j < i; j++)
                array[j] = UNIFY_LINEAR;
            cacheUnifyLinear[i] = array;
        }
    }

    /**
     * <p>Creat an optimization variable.</p>
     *
     * @param s The skel variable.
     */
    private Optimization(SkelVar s) {
        sort = s;
    }

    /**
     * <p>Retrieve a immutable zero int array.</p>
     *
     * @param n The length.
     * @return The immutable zero int array.
     */
    private static int[] valueOf(int n) {
        if (n < 8)
            return cacheUnifyLinear[n];
        int[] array = new int[n];
        for (int j = 0; j < n; j++)
            array[j] = UNIFY_LINEAR;
        return array;
    }

    /**
     * <p>Check whether the given variable is extra.</p>
     *
     * @return True if the variable is extra, otherwise false.
     */
    private boolean extraVar() {
        return (flags & MASK_VAR_BODY) == 0 && (flags & MASK_VAR_HSTR) == 0;
    }

    /**
     * <p>Create the helper for the given rule.</p>
     *
     * @param molec The rule skel.
     * @return The helper.
     */
    static Optimization[] createHelper(Object molec) {
        Object var = SupervisorCopy.getVar(molec);
        if (var == null)
            return VAR_VOID;
        Optimization[] helper;
        if (var instanceof SkelVar) {
            helper = new Optimization[1];
            helper[0] = new Optimization((SkelVar) var);
        } else {
            SkelVar[] temp = (SkelVar[]) var;
            helper = new Optimization[temp.length];
            for (int i = 0; i < temp.length; i++) {
                SkelVar mv = temp[i];
                helper[mv.id] = new Optimization(mv);
            }
        }
        return helper;
    }

    /**
     * <p>Set the structure and minarg of the variables in the given term.</p>
     *
     * @param m      The term skel, can be null.
     * @param clause The clause option flags.
     * @param helper The helper.
     */
    static void setHead(Object m, Clause clause,
                        Optimization[] helper) {
        if (!(m instanceof SkelCompound))
            return;
        SkelCompound mc = (SkelCompound) m;
        for (int i = mc.args.length - 1; i >= 0; i--) {
            Object a = mc.args[i];
            if (a instanceof SkelVar) {
                Optimization ov = helper[((SkelVar) a).id];
                ov.minarg = i;
                if ((clause.flags & AbstractDefined.MASK_DEFI_NHED) != 0)
                    ov.flags |= MASK_VAR_HSTR;
            } else if (a instanceof SkelCompound) {
                Object var = ((SkelCompound) a).var;
                if (var == null)
                    continue;
                if (var instanceof SkelVar) {
                    Optimization ov = helper[((SkelVar) var).id];
                    ov.minarg = i;
                    ov.flags |= MASK_VAR_HSTR;
                } else {
                    SkelVar[] temp = (SkelVar[]) var;
                    for (int j = 0; j < temp.length; j++) {
                        Optimization ov = helper[temp[j].id];
                        ov.minarg = i;
                        ov.flags |= MASK_VAR_HSTR;
                    }
                }
            }
        }
    }

    /**
     * <p>Set the min term of the variables in the given term.</p>
     * <p>Set also the min body if we are not in term position.</p>
     *
     * @param m      The term or term, can be null.
     * @param helper The helper.
     */
    static void setBody(Object m, Optimization[] helper) {
        Object var = SupervisorCopy.getVar(m);
        if (var == null)
            return;
        if (var instanceof SkelVar) {
            Optimization ov = helper[((SkelVar) var).id];
            ov.flags |= MASK_VAR_BODY;
        } else {
            SkelVar[] temp = (SkelVar[]) var;
            for (int i = 0; i < temp.length; i++) {
                Optimization ov = helper[temp[i].id];
                ov.flags |= MASK_VAR_BODY;
            }
        }
    }

    /**
     * <p>Sort and displace the variables in the given rule.</p>
     * <p>Sort criteria is the min term.</p>
     *
     * @param helper The helper, length > 1.
     * @return The number of non-extra variables.
     */
    static int sortExtra(Optimization[] helper) {
        int j = helper.length;
        int k = 0;
        while (k < j && helper[j - 1].extraVar())
            j--;
        while (k + 1 < j) {
            if (!helper[k].extraVar()) {
                k++;
            } else {
                j--;
                Optimization help = helper[k];
                helper[k] = helper[j];
                helper[j] = help;
                while (k < j && helper[j - 1].extraVar())
                    j--;
            }
        }
        if (k < j && !helper[k].extraVar())
            k++;
        for (int i = 0; i < helper.length; i++) {
            Optimization var = helper[i];
            var.sort.id = i;
        }
        return k;
    }

    /**
     * <p>Collect the unify arguments.</p>
     *
     * @param m      The term skel, can be null.
     * @param helper The helper.
     * @return The unify arguments.
     */
    static int[] unifyArgs(Object m, Optimization[] helper) {
        if (!(m instanceof SkelCompound))
            return null;
        SkelCompound mc = (SkelCompound) m;
        if (helper.length == 0)
            return valueOf(mc.args.length);
        int i = mc.args.length - 1;
        for (; i >= 0; i--) {
            Object a = mc.args[i];
            if (!(a instanceof SkelVar))
                break;
            Optimization ov = helper[((SkelVar) a).id];
            if ((ov.flags & MASK_VAR_HSTR) == 0) {
                if (ov.minarg != i) {
                    break;
                } else if ((ov.flags & MASK_VAR_BODY) != 0) {
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
                switch (mc.getSubTerm(i)) {
                    case SkelCompoundLineable.SUBTERM_LINEAR:
                        intargs[i] = UNIFY_LINEAR;
                        break;
                    case SkelCompoundLineable.SUBTERM_TERM:
                        intargs[i] = UNIFY_TERM;
                        break;
                    case SkelCompoundLineable.SUBTERM_CLASH:
                        intargs[i] = UNIFY_CLASH;
                        break;
                    default:
                        throw new IllegalArgumentException("illegal subterm");
                }
                continue;
            }
            Optimization ov = helper[((SkelVar) a).id];
            if ((ov.flags & MASK_VAR_HSTR) == 0) {
                if (ov.minarg != i) {
                    intargs[i] = ov.minarg;
                } else if ((ov.flags & MASK_VAR_BODY) != 0) {
                    intargs[i] = UNIFY_LINEAR;
                } else {
                    intargs[i] = UNIFY_SKIP;
                }
            } else if (ov.minarg != i) {
                intargs[i] = UNIFY_CLASH;
            } else {
                intargs[i] = UNIFY_LINEAR;
            }
        }
        return intargs;
    }

}
