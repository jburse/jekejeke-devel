package jekpro.tools.term;

import jekpro.frequent.standard.SupervisorCopy;
import matula.util.data.ListArray;

/**
 * <p>This class provides lineable compound skeletons.</p>
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
public final class SkelCompoundLineable extends SkelCompound {
    private boolean linear = true;

    /**
     * <p>Create a skel compound with given vars.</p>
     *
     * @param f The functor.
     * @param a The arguments.
     */
    public SkelCompoundLineable(Object[] a, SkelAtom f) {
        super(a, f);
    }

    /**
     * <p>Create a skel compound and init vars.</p>
     *
     * @param f The functor.
     * @param a The arguments.
     */
    public SkelCompoundLineable(SkelAtom f, Object... a) {
        this(a, f);
        makeExtra();
    }

    /**
     * <p>Retrieve the linear flag.</p>
     *
     * @return The linear flag.
     */
    public boolean getLinear() {
        return linear;
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
            if (!SupervisorCopy.getLinear(args[i]))
                linear = false;
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
            if (res != null && contains(res, mv)) {
                linear = false;
                return vec;
            }
            if (vec == null) {
                vec = new ListArray<>();
                vec.add(mv);
            } else if (vec.indexOf(mv) == -1) {
                vec.add(mv);
            } else {
                linear = false;
            }
        } else {
            SkelVar[] temp = (SkelVar[]) newvar;
            for (int i = 0; i < temp.length; i++) {
                SkelVar mv = temp[i];
                if (res != null && contains(res, mv)) {
                    linear = false;
                    continue;
                }
                if (vec == null) {
                    vec = new ListArray<>();
                    vec.add(mv);
                } else if (vec.indexOf(mv) == -1) {
                    vec.add(mv);
                } else {
                    linear = false;
                }
            }
        }
        return vec;
    }

    /**
     * <p>Some testing.</p>
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) {
        SkelVar var = new SkelVar(0);
        SkelVar var2 = new SkelVar(1);
        SkelCompound sc = new SkelCompoundLineable(new SkelAtom("fun"), var, var);
        System.out.println("sc=" + sc + ", linear=" + sc.getLinear());
        sc = new SkelCompoundLineable(new SkelAtom("fun"), var, var2);
        System.out.println("sc=" + sc + ", linear=" + sc.getLinear());
        sc = new SkelCompoundLineable(new SkelAtom("foo"), new SkelCompoundLineable(new SkelAtom("bar"), var, var));
        System.out.println("sc=" + sc + ", linear=" + sc.getLinear());
        sc = new SkelCompoundLineable(new SkelAtom("foo"), new SkelCompoundLineable(new SkelAtom("bar"), var, var2));
        System.out.println("sc=" + sc + ", linear=" + sc.getLinear());
    }
    */

}