package matula.util.format;

import matula.util.transform.*;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class XSelectPrim extends XSelect {
    public static final int SELE_PRIM_ATTR = 0;
    public static final int SELE_PRIM_CONST = 1;
    public static final int SELE_PRIM_CHILD = 2;
    public static final int SELE_PRIM_NULL = 3;
    public static final int SELE_PRIM_THIS = 4;

    public static final String OP_NULL = "null";
    public static final String OP_THIS = "this";

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
     * @param s The type of primitive.
     */
    public XSelectPrim(int s) {
        primitive = s;
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
     */
    public Object evalElement(DomElement d) {
        Object res;
        switch (getPrimitive()) {
            case SELE_PRIM_ATTR:
                res = d.getAttrObj(getAttr());
                break;
            case SELE_PRIM_CONST:
                res = getCnst();
                break;
            case SELE_PRIM_CHILD:
                res = d.getChild(getAttr());
                break;
            case SELE_PRIM_NULL:
                res = null;
                break;
            case SELE_PRIM_THIS:
                res = d;
                break;
            default:
                throw new IllegalArgumentException("illegal primitive");
        }
        return res;
    }

    /**
     * <p>Check an xpath select.</p>
     *
     * @param d The schema and simulation.
     * @return The type id.
     * @throws ValidationError Check error.
     */
    public int checkElement(XPathCheck d) throws ValidationError {
        switch (getPrimitive()) {
            case XSelectPrim.SELE_PRIM_ATTR:
                String context = d.getContext();
                if ("".equals(context))
                    throw new ValidationError(XSelect.PATH_MISSING_FOREACH, toString());
                String attr = getAttr();
                XSDSchema schema = d.getSchema();
                XSDDeclAttr decl = schema.getDeclAttr(context, attr);
                return decl.getType();
            case XSelectPrim.SELE_PRIM_CONST:
                Object val = getCnst();
                if (val instanceof String) {
                    return XSDDeclAttr.TYPE_STRING;
                } else if (val instanceof Long) {
                    return XSDDeclAttr.TYPE_INTEGER;
                } else if (val instanceof DomElement) {
                    return XSLSheet.TYPE_ELEMENT;
                } else {
                    throw new ValidationError(XSelect.PATH_CANT_SELE, toString());
                }
            case XSelectPrim.SELE_PRIM_CHILD:
                String name = getAttr();
                schema = d.getSchema();
                XSDDeclElem decl2 = schema.getDeclElem(name);
                context = d.getContext();
                if (!XPathCheck.checkParent(context, decl2))
                    throw new ValidationError(XPathCheck.PATH_ILLEGAL_PARENT,
                            name + " (" + d.getSchema().getName() + ")");
                return XSLSheet.TYPE_ELEMENT;
            case XSelectPrim.SELE_PRIM_NULL:
                return XSDDeclAttr.TYPE_PRIMITIVE;
            default:
                throw new ValidationError(XSelect.PATH_CANT_SELE, toString());
        }
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
                if (val instanceof DomElement) {
                    StringWriter sr = new StringWriter();
                    int mask = AbstractDom.MASK_LIST + AbstractDom.MASK_TEXT;
                    try {
                        sr.write("\"");
                        AbstractWriter.store(sr, (DomElement) val, null, mask);
                        sr.write("\"");
                    } catch (IOException x) {
                        throw new RuntimeException("internal error", x);
                    }
                    return sr.toString();
                } else {
                    if (val instanceof String) {
                        buf = new StringBuilder();
                        buf.append("\'");
                        buf.append((String) val);
                        buf.append("\'");
                        return buf.toString();
                    } else {
                        return Long.toString(((Long) val).longValue());
                    }
                }
            case SELE_PRIM_CHILD:
                name = getAttr();
                return name;
            case SELE_PRIM_NULL:
                return OP_NULL;
            case SELE_PRIM_THIS:
                return OP_THIS;
            default:
                throw new IllegalArgumentException("illegal primitive");
        }
    }

}