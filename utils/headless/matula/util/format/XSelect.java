package matula.util.format;

import matula.util.transform.ValidationError;
import matula.util.transform.XPathCheck;

/**
 * <p>The class represent an abstract xselect.</p>
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
public abstract class XSelect {
    public static final String PATH_CANT_SELE = "path_cant_sele";
    public static final String PATH_MISSING_FOREACH = "path_missing_foreach";
    public static final String PATH_INTEGER_SELE = "path_integer_sele";
    public static final String PATH_PRIMITIV_SELE = "path_timestamp_sele";

    /**
     * <p>Eval an xpath select.</p>
     *
     * @param d The dom element.
     * @return The value.
     */
    public abstract Object evalElement(DomElement d);

    /**
     * <p>Check an xpath select.</p>
     *
     * @param d The schema and simulation.
     * @return The type id.
     * @throws ValidationError Check error.
     */
    public abstract int checkElement(XPathCheck d) throws ValidationError;

    /**
     * <p>Convert this xpath select to a string.</p>
     *
     * @return The string.
     */
    public abstract String toString();

}