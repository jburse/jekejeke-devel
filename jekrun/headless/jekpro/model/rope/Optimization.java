package jekpro.model.rope;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;

/**
 * <p>This class provides optimization infornation.</p>
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
    public final static int MASK_VAR_HSTR = 0x00010000;
    public final static int MASK_VAR_BODY = 0x00020000;
    public final static int MASK_VAR_MARG = 0x0000FFFF;

    public static final int UNIFY_SKIP = -3;
    public static final int UNIFY_TERM = -2;
    public static final int UNIFY_VAR = -1;

    private final static int[][] cacheUnifyTerm = new int[8][];

    int flags;
    SkelVar sort;

    /**
     * <p>Initialize the immutable cache.</p>
     */
    static {
        for (int i = 0; i < 8; i++) {
            int[] array = new int[i];
            for (int j = 0; j < i; j++)
                array[j] = Optimization.UNIFY_TERM;
            Optimization.cacheUnifyTerm[i] = array;
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
     * <p>Create a optimization helper array.</p>
     *
     * @param var The variables.
     * @return The helper.
     */
    static Optimization[] createHelper(Object var) {
        Optimization[] helper;
        if (var instanceof SkelVar) {
            SkelVar mv = (SkelVar) var;
            helper = new Optimization[1];
            Optimization ov = new Optimization();
            ov.sort = mv;
            helper[mv.id] = ov;
        } else {
            SkelVar[] temp = (SkelVar[]) var;
            helper = new Optimization[temp.length];
            for (int i = 0; i < temp.length; i++) {
                SkelVar mv = temp[i];
                Optimization ov = new Optimization();
                ov.sort = mv;
                helper[mv.id] = ov;
            }
        }
        return helper;
    }

    /**
     * <p>Sort and displace the variables in the given rule.</p>
     * <p>Sort criteria is the min term.</p>
     *
     * @param molec  The clause skeleton.
     * @param helper The helper.
     * @return The number of non-extra variables.
     */
    static int sortExtra(Object molec, Optimization[] helper) {
        int j = helper.length;
        int k = 0;
        while (k < j && extraVar(helper[j - 1].flags))
            j--;
        while (k + 1 < j) {
            if (!extraVar(helper[k].flags)) {
                k++;
            } else {
                j--;
                Optimization help = helper[k];
                helper[k] = helper[j];
                helper[j] = help;
                while (k < j && extraVar(helper[j - 1].flags))
                    j--;
            }
        }
        if (k < j && !extraVar(helper[k].flags))
            k++;
        for (int i = 0; i < helper.length; i++) {
            Optimization ov = helper[i];
            ov.sort.id = i;
        }
        return k;
    }

    /**
     * <p>Check whether the given variable is extra.</p>
     *
     * @param flags The flags.
     * @return True if the variable is extra, otherwise false.
     */
    private static boolean extraVar(int flags) {
        return (flags & MASK_VAR_BODY) == 0
                && (flags & MASK_VAR_HSTR) == 0;
    }

    /**
     * <p>Collect the unify arguments.</p>
     *
     * @param molec  The head skeleton.
     * @param helper The helper.
     * @return The unify arguments.
     */
    static int[] unifyArgsExtra(Object molec, Optimization[] helper) {
        if (!(molec instanceof SkelCompound))
            return null;
        SkelCompound mc = (SkelCompound) molec;
        if (SupervisorCopy.getVar(molec) == null)
            return valueOfTerm(mc.args.length);
        int i = mc.args.length - 1;
        for (; i >= 0; i--) {
            Object a = mc.args[i];
            if (!(a instanceof SkelVar))
                break;
            SkelVar mv = (SkelVar) a;
            Optimization ov = helper[mv.id];
            if ((ov.flags & MASK_VAR_MARG) != i) {
                break;
            } else if ((ov.flags & MASK_VAR_HSTR) != 0) {
                break;
            } else if ((ov.flags & MASK_VAR_BODY) != 0) {
                break;
            } else {
                /* */
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
            SkelVar mv = (SkelVar) a;
            Optimization ov = helper[mv.id];
            if ((ov.flags & MASK_VAR_MARG) != i) {
                intargs[i] = ov.flags & MASK_VAR_MARG;
            } else if ((ov.flags & MASK_VAR_HSTR) != 0) {
                intargs[i] = UNIFY_TERM;
            } else if ((ov.flags & MASK_VAR_BODY) != 0) {
                intargs[i] = UNIFY_VAR;
            } else {
                intargs[i] = UNIFY_SKIP;
            }
        }
        return intargs;
    }

    /**
     * <p>Collect the unify arguments.</p>
     *
     * @param molec The head skeleton.
     * @return The unify arguments.
     */
    static int[] unifyArgs(Object molec) {
        if (!(molec instanceof SkelCompound))
            return null;
        SkelCompound mc = (SkelCompound) molec;
        return valueOfTerm(mc.args.length);
    }

}