package jekpro.model.rope;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.tools.term.SkelCompound;
import jekpro.tools.term.SkelVar;
import matula.util.data.ListArray;

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
    public final static int MASK_VAR_HSTR = 0x00000001;
    public final static int MASK_VAR_BODY = 0x00000002;

    public static final int UNIFY_TERM = 0;
    public static final int UNIFY_COMBO = 1;

    int flags;
    short minarg;
    SkelVar sort;

    /**
     * <p>Create a optimization helper array.</p>
     *
     * @param molec The clause.
     * @return The helper.
     */
    static Optimization[] createHelper(Object molec) {
        Object var = SupervisorCopy.getVar(molec);
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
     * @param helper The helper.
     * @return The number of non-extra variables.
     */
    static int sortExtra(Optimization[] helper) {
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
     * @param clause The clause.
     * @param helper The helper.
     * @return The unify arguments.
     */
    static short[] unifyArgs(Clause clause, Optimization[] helper) {
        if (clause.size == clause.sizerule)
            return null;
        SkelCompound mc = (SkelCompound) clause.head;
        ListArray<Short> list = new ListArray<>();
        for (int i = 0; i < mc.args.length; i++) {
            Object a = mc.args[i];
            if (!(a instanceof SkelVar)) {
                list.add(Short.valueOf((short) UNIFY_TERM));
                list.add(Short.valueOf((short) i));
                continue;
            }
            Optimization ov = helper[((SkelVar) a).id];
            if (!extraVar(ov.flags)) {
                list.add(Short.valueOf((short) UNIFY_TERM));
                list.add(Short.valueOf((short) i));
            } else if (ov.minarg != i) {
                list.add(Short.valueOf((short) UNIFY_COMBO));
                list.add(Short.valueOf((short) i));
                list.add(Short.valueOf(ov.minarg));
            }
        }
        short[] intargs = new short[list.size()];
        for (int i = 0; i < list.size(); i++)
            intargs[i] = list.get(i).shortValue();
        return intargs;
    }

}
