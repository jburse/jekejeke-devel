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
    public static final byte SUBTERM_LINEAR = 0; /* YES */
    public static final byte SUBTERM_TERM = 1; /* MAYBE */
    public static final byte SUBTERM_CLASH = 2; /* NO */

    private byte[] subterm;

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
     * <p>Retrieve the linear flag.</p>
     *
     * @return The linear flag.
     */
    public boolean getLinear() {
        if (subterm == null) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * <p>Retrieve a clash flag.</p>
     *
     * @param k The index.
     * @return The clash flag.
     */
    public byte getSubTerm(int k) {
        if (subterm == null) {
            return SUBTERM_LINEAR;
        } else {
            return subterm[k];
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
            if (!SupervisorCopy.getLinear(args[i])) {
                if (subterm == null)
                    subterm = new byte[args.length];
                subterm[i] = SUBTERM_TERM;
            }
            if (res == null) {
                res = newvar;
            } else {
                vec = addExtra(newvar, vec, res, i);
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
     * @param res    The spine, or null.
     * @param k      The argument index.
     * @return The new list or null.
     */
    private ListArray<SkelVar> addExtra(Object newvar,
                                        ListArray<SkelVar> vec,
                                        Object res,
                                        int k) {
        if (newvar instanceof SkelVar) {
            SkelVar mv = (SkelVar) newvar;
            if (res != null && contains(res, mv)) {
                if (subterm == null)
                    subterm = new byte[args.length];
                subterm[k] = SUBTERM_CLASH;
                return vec;
            }
            if (vec == null) {
                vec = new ListArray<>();
                vec.add(mv);
            } else if (vec.indexOf(mv) == -1) {
                vec.add(mv);
            } else {
                if (subterm == null)
                    subterm = new byte[args.length];
                subterm[k] = SUBTERM_CLASH;
            }
        } else {
            SkelVar[] temp = (SkelVar[]) newvar;
            for (int i = 0; i < temp.length; i++) {
                SkelVar mv = temp[i];
                if (res != null && contains(res, mv)) {
                    if (subterm == null)
                        subterm = new byte[args.length];
                    subterm[k] = SUBTERM_CLASH;
                    continue;
                }
                if (vec == null) {
                    vec = new ListArray<>();
                    vec.add(mv);
                } else if (vec.indexOf(mv) == -1) {
                    vec.add(mv);
                } else {
                    if (subterm == null)
                        subterm = new byte[args.length];
                    subterm[k] = SUBTERM_CLASH;
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