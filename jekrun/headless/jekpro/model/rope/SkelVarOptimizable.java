package jekpro.model.rope;

import jekpro.frequent.standard.SupervisorCopy;
import jekpro.tools.term.SkelVar;

/**
 * <p>This class provides variable skeletons with optimization infornation.</p>
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
public class SkelVarOptimizable extends SkelVar {
    public final static SkelVarOptimizable[] VAR_VOID = new SkelVarOptimizable[0];

    public final static int MASK_VAR_HSTR = 0x00000001;
    public final static int MASK_VAR_BODY = 0x00000002;
    public final static int MASK_VAR_GSTR = 0x00000004;

    public int flags;
    public int minarg = -1;
    public SkelVar sort;

    /**
     * <p>Create a skel var optimizable.</p>
     *
     * @param mv The variable.
     */
    public SkelVarOptimizable(SkelVar mv) {
        super(mv.id);
        sort = mv;
    }

    /**
     * <p>Create the helper for the given rule.</p>
     *
     * @param molec The rule skel.
     * @return The helper.
     */
    public static SkelVarOptimizable[] createHelper(Object molec) {
        Object var = SupervisorCopy.getVar(molec);
        if (var == null)
            return VAR_VOID;
        SkelVarOptimizable[] helper;
        if (var instanceof SkelVar) {
            helper = new SkelVarOptimizable[1];
            helper[0] = new SkelVarOptimizable((SkelVar) var);
        } else {
            SkelVar[] temp = (SkelVar[]) var;
            helper = new SkelVarOptimizable[temp.length];
            for (int i = 0; i < temp.length; i++) {
                SkelVar mv = temp[i];
                helper[mv.id] = new SkelVarOptimizable(mv);
            }
        }
        return helper;
    }

    /**
     * <p>Sort and displace the variables in the given rule.</p>
     * <p>Sort criteria is the min term.</p>
     *
     * @param helper The helper, length > 1.
     * @return The number of non-extra variables.
     */
    public static int sortExtra(SkelVarOptimizable[] helper) {
        int j = helper.length;
        int k = 0;
        while (k < j && helper[j - 1].extraVar())
            j--;
        while (k + 1 < j) {
            if (!helper[k].extraVar()) {
                k++;
            } else {
                j--;
                SkelVarOptimizable help = helper[k];
                helper[k] = helper[j];
                helper[j] = help;
                while (k < j && helper[j - 1].extraVar())
                    j--;
            }
        }
        if (k < j && !helper[k].extraVar())
            k++;
        for (int i = 0; i < helper.length; i++) {
            SkelVarOptimizable var = helper[i];
            var.sort.id = i;
        }
        return k;
    }

    /**
     * <p>Check whether the given variable is extra.</p>
     *
     * @return True if the variable is extra, otherwise false.
     */
    public boolean extraVar() {
        return (flags & SkelVarOptimizable.MASK_VAR_GSTR) == 0
                && (flags & SkelVarOptimizable.MASK_VAR_BODY) == 0
                && (flags & SkelVarOptimizable.MASK_VAR_HSTR) == 0;
    }

}