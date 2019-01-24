package matula.util.format;

import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;
import matula.util.regex.ScannerError;
import matula.util.transform.ValidationError;
import matula.util.transform.XPathCheck;

import java.text.ParseException;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class XPathExprComb extends XPathExpr {
    public static final int EXPR_COMB_PRED = 0;
    public static final int EXPR_COMB_OR = 1;
    public static final int EXPR_COMB_AND = 2;

    public static final String OP_TRUE = "true";
    public static final String OP_FALSE = "false";
    public static final String OP_OR = "or";
    public static final String OP_AND = "and";
    public static final String OP_NOT = "not";

    private MapHashLink<String, XPathExpr> exprs;
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
     * @param n The element name.
     */
    void whereName(String n) {
        XSelect first = new XSelectPrim(n, XSelectPrim.SELE_PRIM_CHILD);
        whereExpr("_name", new XPathExprPrim(first, XPathExprPrim.EXPR_PRIM_NAME));
    }

    /**
     * <p>Add a string attribute value predicate.</p>
     *
     * @param a The attribute name.
     * @param v The String value.
     */
    void whereAttr(String a, String v) {
        whereAttrObj(a, v);
    }

    /**
     * <p>Add a long attribute value predicate.</p>
     *
     * @param a The attribute name.
     * @param v The long value.
     */
    void whereAttrLong(String a, long v) {
        whereAttrObj(a, Long.valueOf(v));
    }

    /**
     * <p>Add an attribute value predicate.</p>
     *
     * @param a The attribute name.
     * @param v The value.
     */
    void whereAttrObj(String a, Object v) {
        XSelect first = new XSelectPrim(a, XSelectPrim.SELE_PRIM_ATTR);
        XSelect second;
        if (v != null) {
            second = new XSelectPrim(v, XSelectPrim.SELE_PRIM_CONST);
        } else {
            second = new XSelectPrim(XSelectPrim.SELE_PRIM_NULL);
        }
        whereExpr(a, new XPathExprPrim(first, second, XPathExprPrim.EXPR_PRIM_EQ));
    }

    /**
     * <p>Add an xpath expression.</p>
     *
     * @param k The key.
     * @param v The xpath expression.
     */
    public void whereExpr(String k, XPathExpr v) {
        if (exprs == null)
            exprs = new MapHashLink<String, XPathExpr>();
        MapEntry<String, XPathExpr> entry = exprs.getEntry(k);
        if (entry != null) {
            entry.value = v;
        } else {
            exprs.add(k, v);
        }
    }

    /**
     * <p>Eval an xpath expression.</p>
     *
     * @param e The dom element.
     * @return True if the the xpath expression is satisfied, otherwise false.
     * @throws ScannerError Syntax error.
     */
    public boolean evalElement(DomElement e) throws ScannerError {
        switch (combination) {
            case EXPR_COMB_PRED:
                for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                     entry != null; entry = exprs.successor(entry)) {
                    if (!entry.value.evalElement(e))
                        return false;
                }
                return true;
            case EXPR_COMB_OR:
                if (exprs != null) {
                    for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                         entry != null; entry = exprs.successor(entry)) {
                        if (entry.value.evalElement(e))
                            return true;
                    }
                }
                return false;
            case EXPR_COMB_AND:
                if (exprs != null) {
                    for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                         entry != null; entry = exprs.successor(entry)) {
                        if (!entry.value.evalElement(e))
                            return false;
                    }
                }
                return true;
            default:
                throw new IllegalArgumentException("illegal combination");
        }
    }

    /**
     * <p>Check an xpath expression.</p>
     *
     * @param e The schema and simulation.
     * @throws ValidationError Check error.
     */
    public void checkElement(XPathCheck e) throws ValidationError {
        switch (getCombination()) {
            case XPathExprComb.EXPR_COMB_OR:
            case XPathExprComb.EXPR_COMB_AND:
                MapHashLink<String, XPathExpr> exprs = getExprs();
                if (exprs != null) {
                    for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                         entry != null; entry = exprs.successor(entry))
                        entry.value.checkElement(e);
                }
                break;
            default:
                throw new ValidationError(XPathExpr.PATH_CANT_PRED, toString());
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
                if (exprs == null)
                    return OP_FALSE;
                buf = new StringBuilder();
                first = true;
                for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                     entry != null; entry = exprs.successor(entry)) {
                    if (first) {
                        if (!isPredicateFactor(entry.value))
                            buf.append('(');
                        buf.append(entry.value.toString());
                        if (!isPredicateFactor(entry.value))
                            buf.append(')');
                        first = false;
                    } else {
                        buf.append(' ');
                        buf.append(OP_OR);
                        buf.append(' ');
                        if (!isPredicateFactor(entry.value))
                            buf.append('(');
                        buf.append(entry.value.toString());
                        if (!isPredicateFactor(entry.value))
                            buf.append(')');
                    }
                }
                return buf.toString();
            case EXPR_COMB_AND:
                if (exprs == null)
                    return OP_TRUE;
                buf = new StringBuilder();
                first = true;
                for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                     entry != null; entry = exprs.successor(entry)) {
                    if (first) {
                        if (!isPredicateSimple(entry.value))
                            buf.append('(');
                        buf.append(entry.value.toString());
                        if (!isPredicateSimple(entry.value))
                            buf.append(')');
                        first = false;
                    } else {
                        buf.append(' ');
                        buf.append(OP_AND);
                        buf.append(' ');
                        if (!isPredicateSimple(entry.value))
                            buf.append('(');
                        buf.append(entry.value.toString());
                        if (!isPredicateSimple(entry.value))
                            buf.append(')');
                    }
                }
                return buf.toString();
            default:
                throw new IllegalArgumentException("illegal combination");
        }
    }

    /**
     * <p>Check whether the given expression is a factor.</p>
     *
     * @param expr The expression.
     * @return True if the expression is a factor, otherwise false.
     */
    private static boolean isPredicateFactor(XPathExpr expr) {
        if (isPredicateSimple(expr))
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
    private static boolean isPredicateSimple(XPathExpr expr) {
        if (expr instanceof XPathExprPrim)
            return true;
        if (expr instanceof XPathExprComb)
            return ((XPathExprComb) expr).getExprs() == null;
        return false;
    }

    /**
     * <p>Join with another combined expression.</p>
     * <p>The other expressions is assumed of same combination type.</p>
     *
     * @param beta The second combined expression.
     */
    public void join(XPathExprComb beta) {
        MapHashLink<String, XPathExpr> ex = beta.getExprs();
        if (ex == null)
            return;
        int n = exprs.size();
        for (MapEntry<String, XPathExpr> entry = ex.getFirstEntry();
             entry != null; entry = ex.successor(entry)) {
            whereExpr(Integer.toString(n), entry.value);
            n++;
        }
    }

    /**
     * <p>Complement this expression.</p>
     */
    public void complement() {
        switch (combination) {
            case EXPR_COMB_OR:
                combination = EXPR_COMB_AND;
                break;
            case EXPR_COMB_AND:
                combination = EXPR_COMB_OR;
                break;
            default:
                throw new IllegalArgumentException("not complementable");
        }
        if (exprs != null) {
            for (MapEntry<String, XPathExpr> entry = exprs.getFirstEntry();
                 entry != null; entry = exprs.successor(entry)) {
                entry.value.complement();
            }
        }
    }

}
