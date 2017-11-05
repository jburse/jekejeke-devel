package matula.util.format;

import matula.util.regex.ScannerError;

/**
 * <p>This predicate implements an xpath primitive expression.</p>
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
public final class XPathExprPrim extends XPathExpr {
    public static final String ERROR_UNKNOWN_ATTRIBUTE = "unknown attribute";

    public static final int PRIMITIVE_NAME = 0;
    public static final int PRIMITIVE_ATTR = 1;

    private XSelect first;
    private XSelect second;
    private int primitive;

    /**
     * <p>>Create a new xpath primitive expression.</p>
     *
     * @param f The first argument.
     * @param p The type of primitive.
     */
    public XPathExprPrim(String f, int p) {
        this(new XSelect(f, XSelect.SELECT_CONST), p);
    }

    /**
     * <p>>Create a new xpath primitive expression.</p>
     *
     * @param f The first argument.
     * @param s The second argument.
     * @param p The type of primitive.
     */
    public XPathExprPrim(String f, String s, int p) {
        this(new XSelect(f, XSelect.SELECT_ATTR),
                new XSelect(s, XSelect.SELECT_CONST), p);
    }

    /**
     * <p>>Create a new xpath primitive expression.</p>
     *
     * @param f The first argument.
     * @param p The type of primitive.
     */
    public XPathExprPrim(XSelect f, int p) {
        if (f == null)
            throw new NullPointerException("first missing");
        if (f.getSelect() != XSelect.SELECT_CONST)
            throw new IllegalArgumentException("not const");
        first = f;
        primitive = p;
    }

    /**
     * <p>>Create a new xpath primitive expression.</p>
     *
     * @param f The first argument.
     * @param s The second argument.
     * @param p The type of primitive.
     */
    public XPathExprPrim(XSelect f, XSelect s, int p) {
        if (f == null)
            throw new NullPointerException("first missing");
        if (s == null)
            throw new NullPointerException("second missing");
        first = f;
        second = s;
        primitive = p;
    }

    /**
     * <p>Retrieve the type of primitive.</p>
     *
     * @return The type of primitive.
     */
    public int getPrimitive() {
        return primitive;
    }

    /**
     * <p>Retrieve the first argument.</p>
     *
     * @return The first argument.
     */
    public XSelect getFirst() {
        return first;
    }

    /**
     * <p>Retrieve the second argument.</p>
     *
     * @return The second argument.
     */
    public XSelect getSecond() {
        return second;
    }

    /**
     * <p>Check whether a dom element satisfies this xpath expression.</p>
     *
     * @param e The dom element.
     * @return True if th dom element satisfies this xpath expression, otherwise false.
     */
    boolean checkElement(DomElement e) throws ScannerError {
        switch (primitive) {
            case PRIMITIVE_NAME:
                String val = first.getAttrOrCnst();
                if (!e.isName(val))
                    return false;
                return true;
            case PRIMITIVE_ATTR:
                val = first.eval(e);
                String val2 = second.eval(e);
                if (!val.equals(val2))
                    return false;
                return true;
            default:
                throw new IllegalArgumentException("illegal primitive");
        }
    }

    /**
     * <p>Convert this xpath expression to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        switch (primitive) {
            case PRIMITIVE_NAME:
                return first.getAttrOrCnst();
            case PRIMITIVE_ATTR:
                StringBuilder buf = new StringBuilder();
                buf.append(first.toString());
                buf.append("=");
                buf.append(second.toString());
                return buf.toString();
            default:
                throw new IllegalArgumentException("illegal primitive");
        }
    }

}
