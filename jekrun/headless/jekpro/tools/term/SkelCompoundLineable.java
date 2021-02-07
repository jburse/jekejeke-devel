package jekpro.tools.term;

import jekpro.model.rope.Optimization;

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
     * <p>Compute the subterm flags.</p>
     *
     * @param t The skeleton.
     * @param helper The helper, can be null.
     * @return The skeleton with subterm flags.
     */
    public static Object adornTermSkel(Object t, Optimization[] helper) {
        SkelCompound back = null;
        for (; ; ) {
            if (t instanceof SkelVar) {
                break;
            } else if (t instanceof SkelCompound) {
                SkelCompound sc = (SkelCompound) t;
                if (sc.var != null) {
                    Object[] args = sc.args;
                    int i = 0;
                    for (; i < args.length - 1; i++) {
                        args[i] = adornTermSkel(args[i], helper);
                        sc = adornComponentSkel(sc, i, helper);
                    }
                    t = args[i];
                    args[i] = back;
                    back = sc;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        while (back != null) {
            SkelCompound jack = (SkelCompound) back.args[back.args.length - 1];
            back.args[back.args.length - 1] = t;
            back = adornComponentSkel(back, back.args.length - 1, helper);
            t = back;
            back = jack;
        }
        return t;
    }

    /**
     * <p>Adorn an argument of a compoound.</p>
     *
     * @param sc     The compound.
     * @param k      The argument index.
     * @param helper The helper, can be null.
     * @return The possibly adorned component.
     */
    private static SkelCompound adornComponentSkel(SkelCompound sc, int k,
                                                  Optimization[] helper) {
        byte res = getSubterm(sc.args[k], helper);
        if (res != SUBTERM_LINEAR) {
            byte[] subterm;
            if (sc instanceof SkelCompoundLineable) {
                subterm = ((SkelCompoundLineable) sc).subterm;
            } else {
                subterm = new byte[sc.args.length];
                SkelCompoundLineable sc2 = new SkelCompoundLineable(sc.args, sc.sym, subterm);
                sc2.var = sc.var;
                sc = sc2;
            }
            subterm[k] = res;
        }
        return sc;
    }

    /**
     * <p>Retrieve the linear flag.</p>
     *
     * @param t      The term.
     * @param helper The helper, can be null.
     * @return The linear flag.
     */
    private static byte getSubterm(Object t, Optimization[] helper) {
        if (t instanceof SkelVar) {
            Optimization ov = helper[((SkelVar) t).id];
            if ((ov.flags & Optimization.MASK_VAR_USED) != 0) {
                return SUBTERM_TERM;
            } else {
                ov.flags |= Optimization.MASK_VAR_USED;
                return SUBTERM_LINEAR;
            }
        } else if (t instanceof SkelCompound) {
            if (t instanceof SkelCompoundLineable) {
                byte[] subterm = ((SkelCompoundLineable) t).subterm;
                for (int i = 0; i < subterm.length; i++) {
                    if (subterm[i] != SUBTERM_TERM)
                        return SUBTERM_MIXED;
                }
                return SUBTERM_TERM;
            } else {
                return SUBTERM_LINEAR;
            }
        } else {
            return SUBTERM_LINEAR;
        }
    }

}