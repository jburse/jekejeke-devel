package matula.util.format;

import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;
import matula.util.regex.ScannerError;

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
    public static final int EXPR_COMB_PRED = 0;
    public static final int EXPR_COMB_OR = 1;
    public static final int EXPR_COMB_AND = 2;

    private MapHashLink<String, XPathExpr> exprs = new MapHashLink<String, XPathExpr>();
    private int combination;

    /**
     * <p>Retrieve the expressions.</p>
     *
     * @return The expressions.
     */
    public MapHashLink<String, XPathExpr> getExprs() {
        return exprs;
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
     * <p>Create a new xpath combination expression.</p>
     *
     * @param c The type of combination.
     */
    public XPathExprComb(int c) {
        combination = c;
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
        XSelect first = new XSelectPrim(n, XSelectPrim.SELE_PRIM_ATTR);
        whereExpr(n, new XPathExprPrim(first, XPathExprPrim.EXPR_PRIM_NAME));
    }

    /**
     * <p>Add an element attribute predicate.</p>
     *
     * @param k The key.
     * @param v The value.
     */
    public void whereAttrObj(String k, Object v) {
        XSelect first = new XSelectPrim(k, XSelectPrim.SELE_PRIM_ATTR);
        XSelect second = new XSelectPrim(v, XSelectPrim.SELE_PRIM_CONST);
        whereExpr(k, new XPathExprPrim(first, second, XPathExprPrim.EXPR_PRIM_EQ));
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
            exprs.add(s, e);
        }
    }

    /**
     * <p>Check whether a dom element satisfies this xpath expression.</p>
     *
     * @param e The dom element.
     * @return True if th dom element satisfies this xpath expression, otherwise false.
     * @throws ScannerError Shit happens
     */
    public boolean checkElement(DomElement e)
            throws ScannerError {
        switch (combination) {
            case EXPR_COMB_PRED:
                for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                     entry != null; entry = exprs.successor(entry)) {
                    if (!entry.value.checkElement(e))
                        return false;
                }
                return true;
            case EXPR_COMB_OR:
                for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                     entry != null; entry = exprs.successor(entry)) {
                    if (entry.value.checkElement(e))
                        return true;
                }
                return false;
            case EXPR_COMB_AND:
                for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                     entry != null; entry = exprs.successor(entry)) {
                    if (!entry.value.checkElement(e))
                        return false;
                }
                return true;
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
            case EXPR_COMB_PRED:
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
            case EXPR_COMB_OR:
                if (exprs.size() == 0)
                    return "false";
                buf = new StringBuilder();
                first = true;
                for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                     entry != null; entry = exprs.successor(entry)) {
                    if (first) {
                        if (!isTerm(entry.value))
                            buf.append("(");
                        buf.append(entry.value.toString());
                        if (!isTerm(entry.value))
                            buf.append(")");
                        first = false;
                    } else {
                        buf.append(" or ");
                        if (!isTerm(entry.value))
                            buf.append("(");
                        buf.append(entry.value.toString());
                        if (!isTerm(entry.value))
                            buf.append(")");
                    }
                }
                return buf.toString();
            case EXPR_COMB_AND:
                if (exprs.size() == 0)
                    return "true";
                buf = new StringBuilder();
                first = true;
                for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                     entry != null; entry = exprs.successor(entry)) {
                    if (first) {
                        if (!isSimple(entry.value))
                            buf.append("(");
                        buf.append(entry.value.toString());
                        if (!isSimple(entry.value))
                            buf.append(")");
                        first = false;
                    } else {
                        buf.append(" and ");
                        if (!isSimple(entry.value))
                            buf.append("(");
                        buf.append(entry.value.toString());
                        if (!isSimple(entry.value))
                            buf.append(")");
                    }
                }
                return buf.toString();
            default:
                throw new IllegalArgumentException("illegal combination");
        }
    }

    /**
     * <p>Check whether the given expression is a term.</p>
     *
     * @param expr The expression.
     * @return True if the expression is a term, otherwise false.
     */
    private static boolean isTerm(XPathExpr expr) {
        if (isSimple(expr))
            return true;
        if (expr instanceof XPathExprComb)
            return ((XPathExprComb) expr).getCombination() == EXPR_COMB_AND;
        return false;
    }

    /**
     * <p>Check whether the given expression is simple.</p>
     *
     * @param expr The expression.
     * @return True if the expression is simple, otherwise false.
     */
    private static boolean isSimple(XPathExpr expr) {
        if (expr instanceof XPathExprPrim)
            return true;
        if (expr instanceof XPathExprComb)
            return ((XPathExprComb) expr).getExprs().size() == 0;
        return false;
    }

    /**
     * <p>Join with another combined expression.</p>
     * <p>The other expressions is assumed of same combination type.</p>
     *
     * @param beta The second combined expression.
     */
    public void join(XPathExprComb beta) {
        int n = getExprs().size();
        MapHashLink<String, XPathExpr> exprs = beta.getExprs();
        for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
             entry != null; entry = exprs.successor(entry)) {
            whereExpr(Integer.toString(n), entry.value);
            n++;
        }
    }

}
