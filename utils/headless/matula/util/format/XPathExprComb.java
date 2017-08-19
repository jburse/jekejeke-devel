package matula.util.format;

import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;

/**
 * <p>This class represents an xpath combination expression.</p>
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
public final class XPathExprComb extends XPathExpr {
    public static final int CONBINATION_AND = 0;
    public static final int COMBINATION_OR = 1;

    private MapHashLink<String, XPathExpr> exprs = new MapHashLink<String, XPathExpr>();
    private int combination;

    /**
     * <p>Create a new xpath combination expression.</p>
     *
     * @param c The type of combination.
     */
    public XPathExprComb(int c) {
        combination = c;
    }

    /**
     * <p>Retrieve the type of combination.</p>
     *
     * @return The type of combination.
     */
    public int getCombination() {
        return combination;
    }

    /**
     * <p>Retrieve the expressions.</p>
     *
     * @return The expressions.
     */
    public MapHashLink<String, XPathExpr> getExprs() {
        return exprs;
    }

    /*****************************************************/
    /* XPath Expressions                                 */
    /*****************************************************/

    /**
     * <p>Add an element name predicate.</p>
     *
     * @param n The name.
     */
    public void whereName(String n) {
        whereExpr(n, new XPathExprPrim(n, XPathExprPrim.PRIMITIVE_NAME));
    }

    /**
     * <p>Add an element attribute predicate.</p>
     *
     * @param k The key.
     * @param v The value.
     */
    public void whereAttr(String k, String v) {
        whereExpr(k, new XPathExprPrim(k, v, XPathExprPrim.PRIMITIVE_ATTR));
    }

    /**
     * <p>Add an xpath expression.</p>
     *
     * @param s The slot name.
     * @param e The xath expression.
     */
    public void whereExpr(String s, XPathExpr e) {
        MapEntry<String, XPathExpr> entry = exprs.getEntry(s);
        if (entry != null) {
            entry.value = e;
        } else {
            exprs.put(s, e);
        }
    }

    /**
     * <p>Check whether a dom element satisfies this xpath expression.</p>
     *
     * @param e The dom element.
     * @return True if th dom element satisfies this xpath expression, otherwise false.
     */
    boolean checkElement(DomElement e) {
        switch (combination) {
            case CONBINATION_AND:
                for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                     entry != null; entry = exprs.successor(entry)) {
                    if (!entry.value.checkElement(e))
                        return false;
                }
                return true;
            case COMBINATION_OR:
                for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                     entry != null; entry = exprs.successor(entry)) {
                    if (entry.value.checkElement(e))
                        return true;
                }
                return false;
            default:
                throw new IllegalArgumentException("illegal combination");
        }
    }

    /**
     * <p>Convert this xpath expression to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        switch (combination) {
            case CONBINATION_AND:
                StringBuilder buf = new StringBuilder();
                boolean first = true;
                for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                     entry != null; entry = exprs.successor(entry)) {
                    if (first) {
                        buf.append(entry.value.toString());
                        first = false;
                    } else {
                        buf.append("[");
                        buf.append(entry.value.toString());
                        buf.append("]");
                    }
                }
                return buf.toString();
            case COMBINATION_OR:
                buf = new StringBuilder();
                first = true;
                for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                     entry != null; entry = exprs.successor(entry)) {
                    if (first) {
                        if (!(entry.value instanceof XPathExprPrim))
                            buf.append("(");
                        buf.append(entry.value.toString());
                        if (!(entry.value instanceof XPathExprPrim))
                            buf.append(")");
                        first = false;
                    } else {
                        buf.append(" or ");
                        if (!(entry.value instanceof XPathExprPrim))
                            buf.append("(");
                        buf.append(entry.value.toString());
                        if (!(entry.value instanceof XPathExprPrim))
                            buf.append(")");
                    }
                }
                return buf.toString();
            default:
                throw new IllegalArgumentException("illegal combination");
        }
    }

}
