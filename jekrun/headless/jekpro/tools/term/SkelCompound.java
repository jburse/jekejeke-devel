package jekpro.tools.term;

import jekpro.model.molec.Display;
import jekpro.model.pretty.PrologWriter;
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
    public SkelVar[] vars;

    /**
     * <p>Create a skel compound with given vars.</p>
     *
     * @param f The functor.
     * @param a The arguments.
     * @param v The vars.
     */
    public SkelCompound(SkelAtom f, Object[] a, SkelVar[] v) {
        if (f == null)
            throw new NullPointerException("functor missing");
        if (a == null)
            throw new NullPointerException("arguments missing");
        if (a.length == 0)
            throw new IllegalArgumentException("zero arguments");
        sym = f;
        args = a;
        vars = v;
    }

    /**
     * <p>Create a skel compound and init vars.</p>
     *
     * @param f The functor.
     * @param a The arguments.
     */
    public SkelCompound(SkelAtom f, Object... a) {
        this(f, a, listToArray(prepareList(a, null), null));
    }

    /**
     * <p>Return a string of a skeleton.</p>
     *
     * @return The string.
     */
    public String toString() {
        return PrologWriter.toString(this, Display.DISPLAY_CONST, 0);
    }

    /**
     * <p>Compute the vars for the arguments.</p>
     *
     * @param a The arguments.
     * @return The vars.
     */
    public static ListArray<SkelVar> prepareList(Object[] a, ListArray<SkelVar> vec) {
        for (int i = a.length - 1; i >= 0; i--)
            vec = collectVars(a[i], vec);
        return vec;
    }

    /**
     * <p>Compute the vars for the arguments except the last.</p>
     *
     * @param a The arguments.
     * @return The vars.
     */
    public static ListArray<SkelVar> prepareListButOne(Object[] a, ListArray<SkelVar> vec) {
        for (int i = a.length - 2; i >= 0; i--)
            vec = collectVars(a[i], vec);
        return vec;
    }

    /**
     * <p>Compute the vars for the given argument.</p>
     * <p>The variables are added in reverse order.</p>
     *
     * @param m   The argument.
     * @param vec The variables or null.
     * @return The variables or null.
     */
    public static ListArray<SkelVar> collectVars(Object m, ListArray<SkelVar> vec) {
        if (m instanceof SkelVar) {
            SkelVar mv = (SkelVar) m;
            if (vec == null) {
                vec = new ListArray<SkelVar>();
                vec.add(mv);
            } else {
                if (vec.indexOf(mv) == -1)
                    vec.add(mv);
            }
        } else if (m instanceof SkelCompound) {
            SkelVar[] vars = ((SkelCompound) m).vars;
            if (vars != null) {
                if (vec == null) {
                    vec = new ListArray<SkelVar>();
                    for (int j = vars.length - 1; j >= 0; j--)
                        vec.add(vars[j]);
                } else {
                    for (int j = vars.length - 1; j >= 0; j--) {
                        SkelVar mv = vars[j];
                        if (vec.indexOf(mv) == -1)
                            vec.add(mv);
                    }
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
     * @return The array.
     */
    public static SkelVar[] listToArray(ListArray<SkelVar> vec, SkelVar[] vars) {
        if (vec == null) {
            if (vars == null)
                return vars;
        } else {
            if (vars != null && vars.length == vec.size())
                return vars;
        }
        if (vec == null)
            return null;
        vars = new SkelVar[vec.size()];
        for (int i = 0; i < vec.size(); i++)
            vars[vars.length - 1 - i] = vec.get(i);
        return vars;
    }

}
