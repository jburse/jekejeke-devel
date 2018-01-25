package matula.util.transform;

import idxtab.Temprepo.TemprepoPath;
import matula.util.format.AbstractDom;
import matula.util.format.DomElement;
import matula.util.format.DomWriter;
import matula.util.format.XPathOrder;
import matula.util.regex.ScannerError;
import matula.util.system.AbstractRuntime;

import java.io.IOException;
import java.io.PrintWriter;

/**
 * <p>This class provides an XSL style sheet base.</p>
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
abstract class XSLSheet {
    private static final String SHEET_MISSING_CLASS = "sheet_missing_class";
    private static final String SHEET_MISMATCHED_BEAN = "sheet_mismatched_bean";
    private static final String SHEET_ILLEGAL_ACCESS = "sheet_illegal_access";
    private static final String SHEET_INST_EXCEPTION = "sheet_inst_exception";
    static final String SHEET_ILLEGAL_VALUE = "sheet_illegal_value";

    private static final String OP_ELEMENT = "element";

    private static final String OP_TEXT_PLAIN = "text/plain";
    private static final String OP_TEXT_HTML = "text/html";

    public static final int TYPE_ELEMENT = 3;

    public static final int TEXT_PLAIN = 0;
    public static final int TEXT_HTML = 1;

    /**
     * <p>Resolve the bean.</p>
     *
     * @param bean The bean.
     * @return The interface path.
     * @throws ValidationError Check error.
     */
    public InterfacePath resolveBean(String bean)
            throws ValidationError {
        try {
            ClassLoader loader = getClass().getClassLoader();
            Class<?> _class = AbstractRuntime.stringToClass(bean, loader);
            if (_class == null)
                throw new ValidationError(SHEET_MISSING_CLASS, bean);
            Object obj = _class.newInstance();
            if (!(obj instanceof InterfacePath))
                throw new ValidationError(SHEET_MISMATCHED_BEAN, bean);
            return (InterfacePath) obj;
        } catch (IllegalAccessException x) {
            throw new ValidationError(SHEET_ILLEGAL_ACCESS, bean);
        } catch (InstantiationException x) {
            throw new ValidationError(SHEET_INST_EXCEPTION, bean);
        }
    }

    /**
     * <p>Check a parameter type attribute value.</p>
     *
     * @param de   The dom element.
     * @param type The attribute value.
     * @return The parameter type id.
     * @throws ValidationError Check error.
     */
    static int checkParamType(DomElement de, String type)
            throws ValidationError {
        int typeid = XSDDeclAttr.TYPE_PRIMITIVE;
        if (type == null) {
            /* */
        } else if (XSDDeclAttr.OP_STRING.equalsIgnoreCase(type)) {
            typeid = XSDDeclAttr.TYPE_STRING;
        } else if (XSDDeclAttr.OP_INTEGER.equalsIgnoreCase(type)) {
            typeid = XSDDeclAttr.TYPE_INTEGER;
        } else if (OP_ELEMENT.equalsIgnoreCase(type)) {
            typeid = TYPE_ELEMENT;
        } else {
            String name = de.getName();
            throw new ValidationError(SHEET_ILLEGAL_VALUE, name + ".type");
        }
        return typeid;
    }

    /**
     * <p>Check a mime type attribute value.</p>
     *
     * @param de   The dom element.
     * @param type The attribute value.
     * @return The mime type id.
     * @throws ValidationError Check error.
     */
    static int checkMimeType(DomElement de, String type)
            throws ValidationError {
        if (OP_TEXT_PLAIN.equalsIgnoreCase(type)) {
            return TEXT_PLAIN;
        } else if (OP_TEXT_HTML.equalsIgnoreCase(type)) {
            return TEXT_HTML;
        } else {
            String name = de.getName();
            throw new ValidationError(SHEET_ILLEGAL_VALUE, name + ".mime");
        }
    }

    /**
     * <p>Check an order attribute value.</p>
     *
     * @param de    The dom element.
     * @param order The attribute value.
     * @return The parameter type id.
     * @throws ValidationError Check error.
     */
    static int checkOrder(DomElement de, String order)
            throws ValidationError {
        int orderid = XPathOrder.ORDER_ASC;
        if (order == null) {
            /* */
        } else if (XPathOrder.OP_ASCENDING.equalsIgnoreCase(order)) {
            /* */
        } else if (XPathOrder.OP_DESCENDING.equalsIgnoreCase(order)) {
            orderid = XPathOrder.ORDER_DESC;
        } else {
            String name = de.getName();
            throw new ValidationError(SHEET_ILLEGAL_VALUE, name + ".order");
        }
        return orderid;
    }

    /**
     * <p>Some test cases.</p>
     *
     * @param args Not used.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    public static void main(String[] args)
            throws IOException, ScannerError, ValidationError {
        TemprepoPath tp = new TemprepoPath();
        tp.setDocument("package");
        tp.list();
        boolean f = tp.next();
        tp.close();
        if (!f)
            throw new IllegalArgumentException("template missing");
        DomElement template = tp.getFound();

        TemprepoPath tp2 = new TemprepoPath();
        tp2.setFlags(tp2.getFlags() | TemprepoPath.FLAG_SCHM);
        tp2.setDocument("package");
        tp2.list();
        f = tp2.next();
        tp2.close();
        if (!f)
            throw new IllegalArgumentException("schema missing");
        DomElement de = tp2.getFound();
        XSDSchema schema = new XSDSchema();
        schema.digestElements(de);

        XSLSheetCheck xc = new XSLSheetCheck();
        xc.setMask(AbstractDom.MASK_TEXT);
        xc.setSchema(schema);
        xc.check(template);
        System.out.println("XSL template package ok");

        PrintWriter pw = new PrintWriter(System.out);
        DomWriter dw = new DomWriter();
        dw.setMask(AbstractDom.MASK_TEXT);
        dw.setWriter(pw);

        de = new DomElement();
        DomElement de1 = new DomElement();
        de1.setName("demo");
        de1.setAttr("jack", "abc");
        de1.setAttrLong("jill", 123);
        de.addNode(de1);
        de1 = new DomElement();
        de1.setName("demo");
        de1.setAttr("jack", "ghi");
        de1.setAttrLong("jill", 456);
        de.addNode(de1);
        de1 = new DomElement();
        de1.setName("demo");
        de1.setAttrLong("jill", 789);
        de.addNode(de1);
        de1 = new DomElement();
        de1.setName("demo");
        de1.setAttr("jack", "def");

        de.addNode(de1);

        XSLSheetTransform transform = new XSLSheetTransform();
        transform.setData(de);
        transform.setWriter(dw);

        transform.xslt(template, null);
        pw.flush();
    }

}