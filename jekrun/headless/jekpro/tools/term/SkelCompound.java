package jekpro.tools.term;

import jekpro.frequent.standard.SupervisorCopy;
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
public class SkelCompound extends AbstractSkel
        implements Comparable<SkelCompound> {
    public final SkelAtom sym;
    public final Object[] args;
    public Object var;

    /**
     * <p>Create a skel compound with given vars.</p>
     *
     * @param f The functor.
     * @param a The arguments.
     */
    public SkelCompound(Object[] a, SkelAtom f) {
        if (f == null)
            throw new NullPointerException("functor missing");
        if (a == null)
            throw new NullPointerException("arguments missing");
        if (a.length == 0)
            throw new IllegalArgumentException("zero arguments");
        sym = f;
        args = a;
    }

    /**
     * <p>Create a skel compound and init vars.</p>
     *
     * @param f The functor.
     * @param a The arguments.
     */
    public SkelCompound(SkelAtom f, Object... a) {
        this(a, f);
        makeExtra();
    }

    /**
     * <p>Retrieve the linear flag.</p>
     *
     * @return The linear flag.
     */
    public boolean getLinear() {
        if (var == null) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Populate the variable string.</p>>
     */
    public void makeExtra() {
        Object res = null;
        ListArray<SkelVar> vec = null;
        for (int i = 0; i < args.length; i++) {
            Object newvar = SupervisorCopy.getVar(args[i]);
            if (newvar == null)
                continue;
            if (res == null) {
                res = newvar;
            } else {
                vec = addExtra(newvar, vec, res);
            }
        }
        if (vec != null)
            res = concatExtra(vec, res);
        var = res;
    }

    /**
     * <p>Return the hash code.</p>
     *
     * @return The hash code.
     */
    public int hashCode() {
        return AbstractSkel.hashCodeSkel(this, 0);
    }

    /**
     * <p>Check the identity.</p>
     *
     * @param o The other object.
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof SkelCompound))
            return false;
        return AbstractSkel.equalTermSkel(this, o);
    }

    /**
     * <p>Compare this compound to another compound.</p>
     *
     * @param o The other compound.
     * @return <0 less, 0 equal, >0 greater
     */
    public int compareTo(SkelCompound o) {
        return AbstractSkel.compareTermSkel(this, o);
    }

    /*****************************************************************/
    /* Spine Handling                                                */
    /******************************************************************/

    /**
     * <p>Add new variables from a spine in a list,
     * not already appearing in another spine.</p>
     *
     * @param newvar The spine, non null.
     * @param vec    The list or null.
     * @return The new list or null.
     */
    protected ListArray<SkelVar> addExtra(Object newvar,
                                          ListArray<SkelVar> vec,
                                          Object res) {
        if (newvar instanceof SkelVar) {
            SkelVar mv = (SkelVar) newvar;
            if (res != null && contains(res, mv))
                return vec;
            if (vec == null) {
                vec = new ListArray<>();
                vec.add(mv);
            } else if (vec.indexOf(mv) == -1) {
                vec.add(mv);
            }
        } else {
            SkelVar[] temp = (SkelVar[]) newvar;
            for (int i = 0; i < temp.length; i++) {
                SkelVar mv = temp[i];
                if (res != null && contains(res, mv))
                    continue;
                if (vec == null) {
                    vec = new ListArray<>();
                    vec.add(mv);
                } else if (vec.indexOf(mv) == -1) {
                    vec.add(mv);
                }
            }
        }
        return vec;
    }

    /**
     * <p>Check whether we have already a spine for the variable.</p>
     *
     * @param res The spine, non null.
     * @param mv  The variable.
     * @return Return index of the variable or -1.
     */
    static boolean contains(Object res, SkelVar mv) {
        if (res instanceof SkelVar) {
            return res == mv;
        } else {
            SkelVar[] temp = (SkelVar[]) res;
            for (int i = 0; i < temp.length; i++)
                if (temp[i] == mv)
                    return true;
            return false;
        }
    }

    /**
     * <p>Concat the list to the spine.</p>
     *
     * @param vec The list, non null.
     * @param var The spine.
     * @return The new spine.
     */
    static Object concatExtra(ListArray<SkelVar> vec, Object var) {
        SkelVar[] res;
        int n = vec.size();
        if (var != null) {
            int k;
            if (var instanceof SkelVar) {
                res = new SkelVar[1 + n];
                res[0] = (SkelVar) var;
                k = 1;
            } else {
                SkelVar[] temp = (SkelVar[]) var;
                res = new SkelVar[temp.length + n];
                System.arraycopy(temp, 0, res, 0, temp.length);
                k = temp.length;
            }
            for (int i = 0; i < n; i++)
                res[k + i] = vec.get(i);
        } else {
            if (n == 1)
                return vec.get(0);
            res = new SkelVar[n];
            for (int i = 0; i < n; i++)
                res[i] = vec.get(i);
        }
        return res;
    }

}
