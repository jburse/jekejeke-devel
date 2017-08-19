package matula.util.format;

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
    public static final int PRIMITIVE_NAME = 0;
    public static final int PRIMITIVE_ATTR = 1;

    private String keyorname;
    private String value;
    private int primitive;

    /**
     * <p>>Create a new xpath primitive expression.</p>
     *
     * @param n The name.
     * @param p The type of primitive.
     */
    public XPathExprPrim(String n, int p) {
        if (n == null)
            throw new NullPointerException("name missing");
        keyorname = n;
        primitive = p;
    }

    /**
     * <p>>Create a new xpath primitive expression.</p>
     *
     * @param k The key.
     * @param v The value.
     * @param p The type of primitive.
     */
    public XPathExprPrim(String k, String v, int p) {
        if (k == null)
            throw new NullPointerException("key missing");
        if (v == null)
            throw new NullPointerException("value missing");
        keyorname = k;
        value = v;
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
     * <p>Retrieve the key or name.</p>
     *
     * @return The key or name.
     */
    public String getKeyOrName() {
        return keyorname;
    }

    /**
     * <p>Check whether a dom element satisfies this xpath expression.</p>
     *
     * @param e The dom element.
     * @return True if th dom element satisfies this xpath expression, otherwise false.
     */
    boolean checkElement(DomElement e) {
        switch (primitive) {
            case PRIMITIVE_NAME:
                if (!e.isName(keyorname))
                    return false;
                return true;
            case PRIMITIVE_ATTR:
                if (!value.equals(e.getAttr(keyorname)))
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
                return keyorname;
            case PRIMITIVE_ATTR:
                StringBuilder buf = new StringBuilder();
                buf.append("@");
                buf.append(keyorname);
                buf.append("=\'");
                buf.append(value);
                buf.append("\'");
                return buf.toString();
            default:
                throw new IllegalArgumentException("illegal primitive");
        }
    }

}
