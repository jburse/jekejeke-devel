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

    private MapHashLink<String, XPathExpr> preds = new MapHashLink<String, XPathExpr>();
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
     * <p>Add an element name predicate.</p>
     *
     * @param name The name.
     */
    public void whereName(String name) {
        whereExpr(name, new XPathExprPrim(name, XPathExprPrim.PRIMITIVE_NAME));
    }

    /**
     * <p>Add an element attribute predicate.</p>
     *
     * @param key   The key.
     * @param value The value.
     */
    public void whereAttr(String key, String value) {
        whereExpr(key, new XPathExprPrim(key, value, XPathExprPrim.PRIMITIVE_ATTR));
    }

    /**
     * <p>Add a predicate combintion.</p>
     *
     * @param slot The slot name.
     * @param pred The predicate.
     */
    public void whereExpr(String slot, XPathExpr pred) {
        MapEntry<String, XPathExpr> entry = preds.getEntry(slot);
        if (entry != null) {
            entry.value = pred;
        } else {
            preds.put(slot, pred);
        }
    }

    /**
     * <p>Check whether an element is satisfied by this location.</p>
     *
     * @param e The element.
     * @return True if the element is satisfied by this location, otherwise false.
     */
    boolean checkElement(DomElement e) {
        switch (combination) {
            case CONBINATION_AND:
                for (MapEntry<String, XPathExpr> entry = preds.getFirstEntry();
                     entry != null; entry = preds.successor(entry)) {
                    if (!entry.value.checkElement(e))
                        return false;
                }
                return true;
            case COMBINATION_OR:
                for (MapEntry<String, XPathExpr> entry = preds.getFirstEntry();
                     entry != null; entry = preds.successor(entry)) {
                    if (entry.value.checkElement(e))
                        return true;
                }
                return false;
            default:
                throw new IllegalArgumentException("illegal combination");
        }
    }

    /**
     * <p>Convert this location to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        switch (combination) {
            case CONBINATION_AND:
                StringBuilder buf = new StringBuilder();
                boolean first = true;
                for (MapEntry<String, XPathExpr> entry = preds.getFirstEntry();
                     entry != null; entry = preds.successor(entry)) {
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
                for (MapEntry<String, XPathExpr> entry = preds.getFirstEntry();
                     entry != null; entry = preds.successor(entry)) {
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
