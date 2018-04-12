package matula.util.format;

import matula.util.transform.ValidationError;
import matula.util.transform.XPathCheck;
import matula.util.transform.XSDDeclAttr;
import matula.util.transform.XSLSheet;

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
    public static final int EXPR_PRIM_EQ = 1;
    public static final int EXPR_PRIM_NQ = 2;
    public static final int EXPR_PRIM_LS = 3;
    public static final int EXPR_PRIM_GR = 4;
    public static final int EXPR_PRIM_LQ = 5;
    public static final int EXPR_PRIM_GQ = 6;

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
     * <p>Eval an xpath expression.</p>
     *
     * @param e The dom element.
     * @return True if the the xpath expression is satisfied, otherwise false.
     */
    public boolean evalElement(DomElement e)
            throws IllegalArgumentException {
        if (primitive <= EXPR_PRIM_NAME) {
            String name = ((XSelectPrim) first).getAttr();
            switch (primitive) {
                case EXPR_PRIM_NAME:
                    return e.isName(name);
                default:
                    throw new IllegalArgumentException("illegal primitive");
            }
        } else {
            Object val = first.evalElement(e);
            Object val2 = second.evalElement(e);
            switch (primitive) {
                case EXPR_PRIM_EQ:
                    return equals(val, val2);
                case EXPR_PRIM_NQ:
                    return !equals(val, val2);
                case EXPR_PRIM_LS:
                    return compareTo(val, val2) < 0;
                case EXPR_PRIM_GR:
                    return compareTo(val, val2) > 0;
                case EXPR_PRIM_LQ:
                    return compareTo(val, val2) <= 0;
                case EXPR_PRIM_GQ:
                    return compareTo(val, val2) >= 0;
                default:
                    throw new IllegalArgumentException("illegal primitive");
            }
        }
    }

    /**
     * <p>Check an xpath expression.</p>
     *
     * @param e The schema and simulation.
     * @throws ValidationError Check error.
     */
    public void checkElement(XPathCheck e) throws ValidationError {
        switch (getPrimitive()) {
            case XPathExprPrim.EXPR_PRIM_EQ:
            case XPathExprPrim.EXPR_PRIM_NQ:
            case XPathExprPrim.EXPR_PRIM_LS:
            case XPathExprPrim.EXPR_PRIM_GR:
            case XPathExprPrim.EXPR_PRIM_LQ:
            case XPathExprPrim.EXPR_PRIM_GQ:
                int typeid = getFirst().checkElement(e);
                if (typeid == XSLSheet.TYPE_ELEMENT)
                    throw new ValidationError(XSelect.PATH_PRIMITIV_SELE, getFirst().toString());
                typeid = getSecond().checkElement(e);
                if (typeid == XSLSheet.TYPE_ELEMENT)
                    throw new ValidationError(XSelect.PATH_PRIMITIV_SELE, getSecond().toString());
                break;
            default:
                throw new ValidationError(XPathExpr.PATH_CANT_PRED, toString());
        }
    }

    /**
     * <p>Test equality of two primitive values.</p>
     *
     * @param val  The first value.
     * @param val2 The second value.
     * @return True if equal, otherwise false.
     */
    private static boolean equals(Object val, Object val2) {
        int type = typeOf(val);
        int type2 = typeOf(val2);
        if (type != type2)
            return false;
        switch (type) {
            case XSDDeclAttr.TYPE_PRIMITIVE:
                return true;
            case XSDDeclAttr.TYPE_STRING:
                return ((String) val).equals((String) val2);
            case XSDDeclAttr.TYPE_INTEGER:
                return ((Long) val).equals((Long) val2);
            default:
                throw new IllegalArgumentException("illegal type");
        }
    }

    /**
     * <p>Compare of two primitive values.</p>
     *
     * @param val  The first value.
     * @param val2 The second value.
     * @return less < 0, equals = 0, greater > 0
     */
    public static int compareTo(Object val, Object val2) {
        int type = typeOf(val);
        int type2 = typeOf(val2);
        if (type != type2)
            return (type < type2 ? -1 : 1);
        switch (type) {
            case XSDDeclAttr.TYPE_PRIMITIVE:
                return 0;
            case XSDDeclAttr.TYPE_STRING:
                return ((String) val).compareTo((String) val2);
            case XSDDeclAttr.TYPE_INTEGER:
                return ((Long) val).compareTo((Long) val2);
            default:
                throw new IllegalArgumentException("illegal type");
        }
    }

    /**
     * <p>Determine the type of the value.</p>
     *
     * @param val The value.
     * @return The type.
     */
    private static int typeOf(Object val) {
        if (val == null) {
            return XSDDeclAttr.TYPE_PRIMITIVE;
        } else if (val instanceof String) {
            return XSDDeclAttr.TYPE_STRING;
        } else if (val instanceof Long) {
            return XSDDeclAttr.TYPE_INTEGER;
        } else {
            throw new IllegalArgumentException("unsupported value");
        }
    }

    /**
     * <p>Convert this xpath expression to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        if (primitive <= EXPR_PRIM_NAME) {
            String name = ((XSelectPrim) first).getAttr();
            switch (primitive) {
                case EXPR_PRIM_NAME:
                    return name;
                default:
                    throw new IllegalArgumentException("illegal primitive");
            }
        } else {
            StringBuilder buf = new StringBuilder();
            if (!XSelectComb.isSelectTerm(first))
                buf.append('(');
            buf.append(first.toString());
            if (!XSelectComb.isSelectTerm(first))
                buf.append(')');
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
            if (!XSelectComb.isSelectTerm(second))
                buf.append('(');
            buf.append(second.toString());
            if (!XSelectComb.isSelectTerm(second))
                buf.append(')');
            return buf.toString();
        }
    }

    /**
     * <p>Complement this expression.</p>
     */
    public void complement() {
        switch (primitive) {
            case EXPR_PRIM_NAME:
                throw new IllegalArgumentException("complement unsupported");
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

    /**
     * <p>Some test cases.</p>
     *
     * @param args Not used.
     */
    /*
    public static void main(String[] args) {
        String str1="2012-05-29 12:07:15";
        String str2="2013-11-10 00:11:11";
        int res=compareTo(str1,str2);
        System.out.println("str1= "+str1);
        System.out.println("str2= "+str2);
        System.out.println("compareTo(str1,str2)= "+res);

        System.out.println();

        str1="2012-05-29 12:07:15";
        str2="2013-11-10 00:11:11";
        boolean flag=equals(str1,str2);
        System.out.println("str1= "+str1);
        System.out.println("str2= "+str2);
        System.out.println("equals(str1,str2)= "+flag);
    }
    */

}
