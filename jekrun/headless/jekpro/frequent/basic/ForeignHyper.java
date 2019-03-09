package jekpro.frequent.basic;

import jekpro.reference.arithmetic.SpecialCompare;

/**
 * <p>The foreign evaluable functions for the module basic/hyper.</p>
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
public final class ForeignHyper {

    /**
     * <p>Compute the arcus hyperbolic sine.</p>
     *
     * @param x The input.
     * @return The output.
     */
    public static double asinh(double x) {
        return Math.log(x + Math.sqrt(x * x + 1.0));
    }

    /**
     * <p>Compute the arcus hyperbolic cosine.</p>
     *
     * @param x The input.
     * @return The output.
     */
    public static double acosh(double x) {
        return Math.log(x + Math.sqrt(x * x - 1.0));
    }

    /**
     * <p>Compute the arcus hyperbolic tangent.</p>
     *
     * @param x The input.
     * @return The output.
     */
    public static double atanh(double x) {
        return 0.5 * Math.log((x + 1.0) / (x - 1.0));
    }

    /**************************************************************/
    /* CheerpJ Workaround                                         */
    /**************************************************************/

    public static double epsilon() {
        return SpecialCompare.EPSILON;
    }

    public static float epsilon32() {
        return SpecialCompare.EPSILON32;
    }

    public static double pi() {
        return Math.PI;
    }

    public static double e() {
        return Math.E;
    }

}
