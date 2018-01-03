package matula.util.format;

import java.io.IOException;
import java.io.StringWriter;

/**
 * <p>The class represent an xselect prim.</p>
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
public final class XSelectPrim extends XSelect {
    public static final int SELE_PRIM_ATTR = 0;
    public static final int SELE_PRIM_CONST = 1;
    public static final int SELE_PRIM_CHILD = 2;
    public static final int SELE_PRIM_ELEM = 3;

    /* illegal argument errors */
    public static final String PATH_UNKNOWN_ATTRIBUTE = "path_unknown_attribute";
    public static final String PATH_UNKNOWN_CHILD = "path_unknown_child";

    private Object attrorcnst;
    private int primitive;

    /**
     * <p>Retrieve the type of primitive.</p>
     *
     * @return The type of primitive.
     */
    public int getPrimitive() {
        return primitive;
    }

    /**
     * <p>Retrieve the attibute name.</p>
     *
     * @return The attribute name.
     */
    public String getAttr() {
        return (String) attrorcnst;
    }

    /**
     * <p>Retrieve the constant value.</p>
     *
     * @return The constant value.
     */
    public Object getCnst() {
        return attrorcnst;
    }

    /**
     * <p>>Create a new xselect prim.</p>
     *
     * @param a The attribute or variable.
     * @param s The type of primitive.
     */
    public XSelectPrim(Object a, int s) {
        if (a == null)
            throw new NullPointerException("attribute or const missing");
        attrorcnst = a;
        primitive = s;
    }

    /**
     * <p>Eval an xselect.</p>
     *
     * @param d The dom element.
     * @return The value.
     * @throws IllegalArgumentException Shit happens.
     */
    public Object evalElement(DomElement d) throws IllegalArgumentException {
        Object res;
        switch (getPrimitive()) {
            case SELE_PRIM_ATTR:
                res = d.getAttrObj(getAttr());
                if (res == null)
                    throw new IllegalArgumentException(PATH_UNKNOWN_ATTRIBUTE);
                break;
            case SELE_PRIM_CONST:
                res = getCnst();
                break;
            case SELE_PRIM_CHILD:
                res = d.getChild(getAttr());
                if (res == null)
                    throw new IllegalArgumentException(PATH_UNKNOWN_CHILD);
                break;
            case SELE_PRIM_ELEM:
                res = getCnst();
                break;
            default:
                throw new IllegalArgumentException("illegal primitive");
        }
        return res;
    }

    /**
     * <p>Convert this xselect to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        switch (primitive) {
            case SELE_PRIM_ATTR:
                String name = getAttr();
                StringBuilder buf = new StringBuilder();
                buf.append("@");
                buf.append(name);
                return buf.toString();
            case SELE_PRIM_CONST:
                Object val = getCnst();
                buf = new StringBuilder();
                if (val instanceof String) {
                    buf.append("\'");
                    buf.append((String) val);
                    buf.append("\'");
                } else {
                    buf.append(Long.toString(((Long) val).longValue()));
                }
                return buf.toString();
            case SELE_PRIM_CHILD:
                name = getAttr();
                return name;
            case SELE_PRIM_ELEM:
                val = getCnst();
                StringWriter sr = new StringWriter();
                try {
                    ((DomElement) val).store(sr, null, AbstractDom.MASK_TEXT);
                } catch (IOException x) {
                    throw new RuntimeException("internal error", x);
                }
                return sr.toString();
            default:
                throw new IllegalArgumentException("illegal primitive");
        }
    }

}