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
    public static final byte SUBTERM_MIXED = 1; /* MAYBE */
    public static final byte SUBTERM_TERM = 2; /* NO */

    public final byte[] subterm;

    /**
     * <p>Create a skel compound with given vars.</p>
     *
     * @param a The arguments.
     * @param f The functor.
     * @param s The subterm flags;
     */
    public SkelCompoundLineable(Object[] a, SkelAtom f, byte[] s) {
        super(a, f);
        if (s == null)
            throw new NullPointerException("Subterm missing");
        subterm = s;
    }

    /**
     * <p>Retrieve the linear flag.</p>
     *
     * @return The linear flag.
     */
    public boolean getLinear() {
        return false;
    }

    /**
     * <p>Retrieve a clash flag.</p>
     *
     * @param k The index.
     * @return The clash flag.
     */
    public byte getSubTerm(int k) {
        return subterm[k];
    }

    /**
     * <p>Compute the subterm flags.</p>
     *
     * @param args The arguments.
     * @return The subterm flags.
     */
    public static byte[] makeSubterm(Object[] args) {
        byte[] subterm = null;
        Object res = null;
        ListArray<SkelVar> vec = null;
        for (int i = 0; i < args.length; i++) {
            Object newvar = SupervisorCopy.getVar(args[i]);
            if (newvar == null)
                continue;
            if (!SupervisorCopy.getLinear(args[i])) {
                if (subterm == null)
                    subterm = new byte[args.length];
                subterm[i] = SUBTERM_MIXED;
            }
            if (res == null) {
                res = newvar;
            } else if (newvar instanceof SkelVar) {
                SkelVar mv = (SkelVar) newvar;
                if (canAdd(vec, res, mv)) {
                    if (vec == null)
                        vec = new ListArray<>();
                    vec.add(mv);
                } else {
                    if (subterm == null)
                        subterm = new byte[args.length];
                    subterm[i] = SUBTERM_TERM;
                }
            } else {
                SkelVar[] temp = (SkelVar[]) newvar;
                for (int j = 0; j < temp.length; j++) {
                    SkelVar mv = temp[j];
                    if (canAdd(vec, res, mv)) {
                        if (vec == null)
                            vec = new ListArray<>();
                        vec.add(mv);
                    } else {
                        if (subterm == null)
                            subterm = new byte[args.length];
                        subterm[i] = SUBTERM_TERM;
                    }
                }
            }
        }
        return subterm;
    }

    /**
     * <p>Adorn a component with subterm flags.</p>
     *
     * @param sc The component.
     * @return The possibly adorned component.
     */
    public static SkelCompound adornComponentSkel(SkelCompound sc) {
        byte[] subterm = makeSubterm(sc.args);
        if (subterm == null)
            return sc;
        SkelCompoundLineable sc2 = new SkelCompoundLineable(sc.args, sc.sym, subterm);
        sc2.var = sc.var;
        return sc2;
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