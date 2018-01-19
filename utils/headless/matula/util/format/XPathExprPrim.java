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
    public static final int EXPR_PRIM_NAME = 0;
    public static final int EXPR_PRIM_NOT_NAME = 1;
    public static final int EXPR_PRIM_ATTR = 2;
    public static final int EXPR_PRIM_NOT_ATTR = 3;
    public static final int EXPR_PRIM_EQ = 4;
    public static final int EXPR_PRIM_NQ = 5;
    public static final int EXPR_PRIM_LS = 6;
    public static final int EXPR_PRIM_GR = 7;
    public static final int EXPR_PRIM_LQ = 8;
    public static final int EXPR_PRIM_GQ = 9;

    private XSelect first;
    private XSelect second;
    private int primitive;

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
     * <p>Retrieve the type of primitive.</p>
     *
     * @return The type of primitive.
     */
    public int getPrimitive() {
        return primitive;
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
        if (!(f instanceof XSelectPrim))
            throw new IllegalArgumentException("not prim");
        if (((XSelectPrim) f).getPrimitive() != XSelectPrim.SELE_PRIM_ATTR)
            throw new IllegalArgumentException("not attr");
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
     * <p>Check whether a dom element satisfies this xpath expression.</p>
     *
     * @param e The dom element.
     * @return True if th dom element satisfies this xpath expression, otherwise false.
     */
    public boolean checkElement(DomElement e) throws ScannerError {
        if (primitive <= EXPR_PRIM_NOT_ATTR) {
            String name = ((XSelectPrim) first).getAttr();
            switch (primitive) {
                case EXPR_PRIM_NAME:
                    return e.isName(name);
                case EXPR_PRIM_NOT_NAME:
                    return !e.isName(name);
                case EXPR_PRIM_ATTR:
                    return e.getAttrObj(name) != null;
                case EXPR_PRIM_NOT_ATTR:
                    return e.getAttrObj(name) == null;
                default:
                    throw new IllegalArgumentException("illegal primitive");
            }
        } else {
            Object val = first.evalElement(e);
            Object val2 = second.evalElement(e);
            switch (primitive) {
                case EXPR_PRIM_EQ:
                    return val.equals(val2);
                case EXPR_PRIM_NQ:
                    return !val.equals(val2);
                case EXPR_PRIM_LS:
                    if (val instanceof String) {
                        return ((String) val).compareTo((String) val2) < 0;
                    } else if (val instanceof Long) {
                        return ((Long) val).longValue() < ((Long) val2).longValue();
                    } else {
                        throw new IllegalArgumentException("unsupported value");
                    }
                case EXPR_PRIM_GR:
                    if (val instanceof String) {
                        return ((String) val).compareTo((String) val2) > 0;
                    } else if (val instanceof Long) {
                        return ((Long) val).longValue() > ((Long) val2).longValue();
                    } else {
                        throw new IllegalArgumentException("unsupported value");
                    }
                case EXPR_PRIM_LQ:
                    if (val instanceof String) {
                        return ((String) val).compareTo((String) val2) <= 0;
                    } else if (val instanceof Long) {
                        return ((Long) val).longValue() <= ((Long) val2).longValue();
                    } else {
                        throw new IllegalArgumentException("unsupported value");
                    }
                case EXPR_PRIM_GQ:
                    if (val instanceof String) {
                        return ((String) val).compareTo((String) val2) >= 0;
                    } else if (val instanceof Long) {
                        return ((Long) val).longValue() >= ((Long) val2).longValue();
                    } else {
                        throw new IllegalArgumentException("unsupported value");
                    }
                default:
                    throw new IllegalArgumentException("illegal primitive");
            }
        }
    }

    /**
     * <p>Convert this xpath expression to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        if (primitive <= EXPR_PRIM_NOT_ATTR) {
            String name = ((XSelectPrim) first).getAttr();
            switch (primitive) {
                case EXPR_PRIM_NAME:
                    return name;
                case EXPR_PRIM_NOT_NAME:
                    StringBuilder buf = new StringBuilder();
                    buf.append("~");
                    buf.append(name);
                    return buf.toString();
                case EXPR_PRIM_ATTR:
                    buf = new StringBuilder();
                    buf.append("@");
                    buf.append(name);
                    return buf.toString();
                case EXPR_PRIM_NOT_ATTR:
                    buf = new StringBuilder();
                    buf.append("~@");
                    buf.append(name);
                    return buf.toString();
                default:
                    throw new IllegalArgumentException("illegal primitive");
            }
        } else {
            StringBuilder buf = new StringBuilder();
            buf.append(first.toString());
            switch (primitive) {
                case EXPR_PRIM_EQ:
                    buf.append("=");
                    break;
                case EXPR_PRIM_NQ:
                    buf.append("<>");
                    break;
                case EXPR_PRIM_LS:
                    buf.append("<");
                    break;
                case EXPR_PRIM_GR:
                    buf.append(">");
                    break;
                case EXPR_PRIM_LQ:
                    buf.append("=<");
                    break;
                case EXPR_PRIM_GQ:
                    buf.append(">=");
                    break;
                default:
                    throw new IllegalArgumentException("illegal primitive");
            }
            buf.append(second.toString());
            return buf.toString();
        }
    }

    /**
     * <p>Completent this expression.</p>
     */
    public void complement() {
        switch (primitive) {
            case EXPR_PRIM_NAME:
                primitive = EXPR_PRIM_NOT_NAME;
                break;
            case EXPR_PRIM_NOT_NAME:
                primitive = EXPR_PRIM_NAME;
                break;
            case EXPR_PRIM_ATTR:
                primitive = EXPR_PRIM_NOT_ATTR;
                break;
            case EXPR_PRIM_NOT_ATTR:
                primitive = EXPR_PRIM_ATTR;
                break;
            case EXPR_PRIM_EQ:
                primitive = EXPR_PRIM_NQ;
                break;
            case EXPR_PRIM_NQ:
                primitive = EXPR_PRIM_EQ;
                break;
            case EXPR_PRIM_LS:
                primitive = EXPR_PRIM_GQ;
                break;
            case EXPR_PRIM_GR:
                primitive = EXPR_PRIM_LQ;
                break;
            case EXPR_PRIM_LQ:
                primitive = EXPR_PRIM_GR;
                break;
            case EXPR_PRIM_GQ:
                primitive = EXPR_PRIM_LS;
                break;
            default:
                throw new IllegalArgumentException("illegal primitive");
        }
    }

}
