package matula.util.format;

import matula.util.transform.ValidationError;
import matula.util.transform.XPathCheck;

/**
 * <p>This class is the base class of xpath expressions.</p>
 * </p>
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
public abstract class XPathExpr {
    public static final String PATH_CANT_PRED = "path_cant_pred";

    /**
     * <p>Eval an xpath expression.</p>
     *
     * @param e The dom element.
     * @return True if the the xpath expression is satisfied, otherwise false.
     */
    public abstract boolean evalElement(DomElement e);

    /**
     * <p>Check an xpath expression.</p>
     *
     * @param e The schema and simulation.
     * @throws ValidationError Check error.
     */
    public abstract void checkElement(XPathCheck e) throws ValidationError;

    /**
     * <p>Convert this xpath expression to a string.</p>
     *
     * @return The string.
     */
    public abstract String toString();

    /**
     * <p>Lift this expression to a combination type.</p>
     *
     * @param c The combination type.
     * @return The lifted expression.
     */
    public XPathExprComb lift(int c) {
        if (this instanceof XPathExprComb &&
                ((XPathExprComb) this).getCombination() == c)
            return (XPathExprComb) this;
        XPathExprComb temp = new XPathExprComb(c);
        temp.whereExpr(Integer.toString(0), this);
        return temp;
    }

    /**
     * <p>Complement this expression.</p>
     */
    public abstract void complement();

}
