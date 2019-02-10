package jekpro.tools.term;

import jekpro.frequent.standard.EngineCopy;
import matula.util.data.ListArray;

/**
 * <p>This class provides compound skeletons.</p>
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
public final class SkelCompound extends AbstractSkel {
    public final SkelAtom sym;
    public final Object[] args;
    public Object var;

    /**
     * <p>Create a skel compound with given vars.</p>
     *
     * @param f The functor.
     * @param a The arguments.
     * @param v The vars.
     */
    public SkelCompound(SkelAtom f, Object[] a, Object v) {
        if (f == null)
            throw new NullPointerException("functor missing");
        if (a == null)
            throw new NullPointerException("arguments missing");
        if (a.length == 0)
            throw new IllegalArgumentException("zero arguments");
        sym = f;
        args = a;
        var = v;
    }

    /**
     * <p>Create a skel compound and init vars.</p>
     *
     * @param f The functor.
     * @param a The arguments.
     */
    public SkelCompound(SkelAtom f, Object... a) {
        this(f, a, makeExtra(a));
    }

    /**
     * <p>Add new variables from all the spines of the
     * arguments but one in a list.</p>
     *
     * @param a The arguments.
     * @return The list or null.
     */
    public static Object makeExtra(Object[] a) {
        Object var = EngineCopy.getVar(a[a.length - 1]);
        ListArray<SkelVar> vec = null;
        for (int i = a.length - 2; i >= 0; i--) {
            Object newvar = EngineCopy.getVar(a[i]);
            if (newvar != null)
                vec = addExtra(newvar, vec, var);
        }
        if (vec != null)
            var = concatExtra(vec, var);
        return var;
    }


    /*****************************************************************/
    /* Spine Handling                                                */
    /******************************************************************/

    /**
     * <p>Concat the list to the spine.</p>
     *
     * @param vec The list, not null.
     * @param var The spine.
     * @return The new spine.
     */
    private static Object concatExtra(ListArray<SkelVar> vec, Object var) {
        SkelVar[] res;
        int n = vec.size();
        if (var != null) {
            if (var instanceof SkelVar) {
                res = new SkelVar[n + 1];
                res[n] = (SkelVar) var;
            } else {
                SkelVar[] temp = (SkelVar[]) var;
                res = new SkelVar[n + temp.length];
                System.arraycopy(temp, 0, res, n, temp.length);
            }
        } else {
            if (n == 1)
                return vec.get(0);
            res = new SkelVar[n];
        }
        for (int i = 0; i < n; i++)
            res[n - 1 - i] = vec.get(i);
        return res;
    }

    /**
     * <p>Add new variables from a spine in a list,
     * not already appearing in another spine.</p>
     *
     * @param newvar The spine, not null.
     * @param vec    The list or null.
     * @param var    The other spine.
     * @return The new list or null.
     */
    private static ListArray<SkelVar> addExtra(Object newvar,
                                               ListArray<SkelVar> vec,
                                               Object var) {
        if (newvar instanceof SkelVar) {
            SkelVar mv = (SkelVar) newvar;
            if (var != null && indexOf(var, mv) != -1)
                return vec;
            if (vec == null) {
                vec = new ListArray<SkelVar>();
                vec.add(mv);
            } else {
                if (vec.indexOf(mv) == -1)
                    vec.add(mv);
            }
        } else {
            SkelVar[] temp = (SkelVar[]) newvar;
            for (int i = temp.length - 1; i >= 0; i--) {
                SkelVar mv = temp[i];
                if (var != null && indexOf(var, mv) != -1)
                    continue;
                if (vec == null) {
                    vec = new ListArray<SkelVar>();
                    vec.add(mv);
                } else {
                    if (vec.indexOf(mv) == -1)
                        vec.add(mv);
                }
            }
        }
        return vec;
    }

    /**
     * <p>Check whether we have already a spine for the variable.</p>
     *
     * @param var The spine, not null.
     * @param mv  The variable.
     * @return Return index of the variable or -1.
     */
    private static int indexOf(Object var, SkelVar mv) {
        if (var instanceof SkelVar) {
            return (var.equals(mv) ? 0 : -1);
        } else {
            SkelVar[] temp = (SkelVar[]) var;
            for (int i = 0; i < temp.length; i++)
                if (temp[i].equals(mv))
                    return i;
            return -1;
        }
    }

}
