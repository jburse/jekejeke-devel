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
        this(f, a, prepareArray(a));
    }

    /**
     * <p>Prepare the variable or variable array.</p>
     *
     * @param a The arguments.
     * @return The variable or variable array.
     */
    private static Object prepareArray(Object[] a) {
        Object var = EngineCopy.getVar(a[a.length - 1]);
        ListArray<SkelVar> vec = collectVar(var, null);
        vec = prepareVars(a, vec);
        return listToArray(vec, var);
    }

    /**
     * <p>Compute the vars for the arguments except the last.</p>
     *
     * @param a The arguments.
     * @return The vars.
     */
    public static ListArray<SkelVar> prepareVars(Object[] a,
                                                 ListArray<SkelVar> vec) {
        for (int i = a.length - 2; i >= 0; i--) {
            Object var = EngineCopy.getVar(a[i]);
            vec = collectVar(var, vec);
        }
        return vec;
    }

    /**
     * <p>Compute the vars for the given argument.</p>
     * <p>The variables are added in reverse order.</p>
     *
     * @param var The variable or variable array.
     * @param vec The variables or null.
     * @return The variables or null.
     */
    public static ListArray<SkelVar> collectVar(Object var,
                                                ListArray<SkelVar> vec) {
        if (var == null)
            return vec;
        if (var instanceof SkelVar) {
            if (vec == null) {
                vec = new ListArray<SkelVar>();
                vec.add((SkelVar) var);
            } else {
                SkelVar mv = (SkelVar) var;
                if (vec.indexOf(mv) == -1)
                    vec.add(mv);
            }
        } else {
            if (vec == null) {
                vec = new ListArray<SkelVar>();
                SkelVar[] temp = (SkelVar[]) var;
                for (int j = temp.length - 1; j >= 0; j--)
                    vec.add(temp[j]);
            } else {
                SkelVar[] temp = (SkelVar[]) var;
                for (int j = temp.length - 1; j >= 0; j--) {
                    SkelVar mv = temp[j];
                    if (vec.indexOf(mv) == -1)
                        vec.add(mv);
                }
            }
        }
        return vec;
    }


    /**
     * <p>Create the array from the list.</p>
     * <p>The variables are created in reverse order.</p>
     *
     * @param vec The list.
     * @param var The variables for reuse.
     * @return The array.
     */
    public static Object listToArray(ListArray<SkelVar> vec,
                                     Object var) {
        if (vec == null)
            return null;
        int n = vec.size();
        if (n == 1)
            return vec.get(0);
        if (var instanceof SkelVar[] && ((SkelVar[]) var).length == n)
            return var;
        SkelVar[] res = new SkelVar[n];
        for (int i = 0; i < n; i++)
            res[n - 1 - i] = vec.get(i);
        return res;
    }

}
