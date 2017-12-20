package matula.util.transform;

import matula.util.data.MapHash;
import matula.util.format.*;
import matula.util.regex.ScannerError;
import matula.util.system.ForeignXml;
import matula.util.system.MimeHeader;

import java.io.IOException;

/**
 * <p>This class provides an XSL style sheet transform.</p>
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
public final class XSLSheetTransform extends XSLSheet {
    private static final String PATH_ILLEGAL_VALUE = "path_illegal_value";

    public static final String NAME_STYLESHEET = "stylesheet";
    public static final String NAME_OUTPUT = "output";
    public static final String ATTR_OUTPUT_MIME = "mime";
    public static final String NAME_PARAM = "param";
    public static final String ATTR_PARAM_NAME = "name";
    public static final String ATTR_PARAM_USE = "use";
    public static final String ATTR_PARAM_TYPE = "type";
    public static final String NAME_FOREACH = "for-each";
    public static final String ATTR_FOREACH_SELECT = "select";
    public static final String NAME_VALUEOF = "value-of";
    public static final String ATTR_VALUEOF_SELECT = "select";
    public static final String NAME_WITHDATA = "with-data";
    public static final String ATTR_WITHDATA_BEAN = "bean";
    public static final String ATTR_WITHDATA_SELECT = "select";
    public static final String NAME_IF = "if";
    public static final String ATTR_IF_TEST = "test";
    public static final String NAME_CHOOSE = "choose";
    public static final String NAME_WHEN = "when";
    public static final String ATTR_WHEN_TEST = "test";
    public static final String NAME_OTHERWISE = "otherwise";

    private static final int TYPE_TEXT_PLAIN = 0;
    private static final int TYPE_TEXT_HTML = 1;

    private int type = -1;
    private MapHash<String, Object> variables;
    private DomElement data;
    private DomWriter writer = new DomWriter();

    /**
     * <p>Set the variables.</p>§
     *
     * @param v The variables.
     */
    public void setVariables(MapHash<String, Object> v) {
        variables = v;
    }

    /**
     * <p>Set the data.</p>
     *
     * @param de The data.
     */
    public void setData(DomElement de) {
        data = de;
    }

    /**
     * <p>Retrieve the dom writer.</p>
     *
     * @return The dom writer.
     */
    public DomWriter getWriter() {
        return writer;
    }

    /**
     * <p>Set the dom writer.</p>
     *
     * @param w The dom writer.
     */
    public void setWriter(DomWriter w) {
        writer = w;
    }

    /**
     * <p>Transform a template to a result.</p>
     *
     * @param dn      The template.
     * @param comment The comment.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    public void xslt(DomNode dn, String comment)
            throws IOException, ScannerError, ValidationError {
        if (comment != null && !"".equals(comment))
            writer.writeComment(comment);
        if ((writer.getMask() & DomNode.MASK_LIST) != 0) {
            xsltChildren((DomElement) dn);
        } else {
            xsltNode(dn);
        }
    }

    /**
     * <p>Transform a template to a result.</p>
     *
     * @param dn The template.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    private void xsltNode(DomNode dn)
            throws IOException, ScannerError, ValidationError {
        if (dn instanceof DomText) {
            DomText dt = (DomText) dn;
            copyText(dt.getData());
        } else {
            DomElement de = (DomElement) dn;
            if (de.isName(NAME_FOREACH)) {
                xsltForEach(de);
            } else if (de.isName(NAME_VALUEOF)) {
                xsltValueOf(de);
            } else if (de.isName(NAME_WITHDATA)) {
                xsltWithData(de);
            } else if (de.isName(NAME_OUTPUT)) {
                xsltOutput(de);
            } else if (de.isName(NAME_PARAM)) {
                xsltParam(de);
            } else if (de.isName(NAME_STYLESHEET)) {
                xsltChildren(de);
            } else if (de.isName(NAME_IF)) {
                xsltIf(de);
            } else if (de.isName(NAME_CHOOSE)) {
                xsltChoose(de);
            } else if (de.sizeChildren() != 0) {
                writer.copyStart(de);
                if ((writer.getMask() & DomNode.MASK_TEXT) == 0 &&
                        DomElement.checkAny(writer.getControl(), de.getName())) {
                    int mask = writer.getMask();
                    writer.setMask(writer.getMask() | DomNode.MASK_TEXT);
                    xsltChildren2(de);
                    writer.setMask(mask);
                } else {
                    xsltChildren2(de);
                }
                writer.copyEnd(de);
            } else {
                if (!DomElement.checkEmpty(writer.getControl(), de.getName())) {
                    writer.copyEmpty(de);
                } else {
                    writer.copyStart(de);
                }
            }
        }
    }

    /**
     * <p>Transform the children.</p>
     *
     * @param de The template dom element.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    private void xsltChildren2(DomElement de)
            throws IOException, ScannerError, ValidationError {
        if ((writer.getMask() & DomNode.MASK_TEXT) != 0) {
            xsltChildren(de);
        } else {
            writer.write("\n");
            writer.incIndent();
            xsltChildren(de);
            writer.decIndent();
            writer.writeIndent();
        }
    }

    /**
     * <p>Transform the children.</p>
     *
     * @param de The template dom element.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    private void xsltChildren(DomElement de)
            throws IOException, ScannerError, ValidationError {
        DomNode[] nodes = de.snapshotChildren();
        for (int i = 0; i < nodes.length; i++) {
            DomNode node = nodes[i];
            if ((writer.getMask() & DomNode.MASK_TEXT) != 0) {
                xsltNode(node);
            } else {
                writer.writeIndent();
                xsltNode(node);
                writer.write("\n");
            }
        }
    }

    /**
     * <p>Execute a for each tag.</p>
     *
     * @param de The template dom element.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    private void xsltForEach(DomElement de)
            throws IOException, ScannerError, ValidationError {
        String select = de.getAttr(ATTR_FOREACH_SELECT);
        XPathReadTransform xr = new XPathReadTransform();
        xr.setVariables(variables);
        XPath xpath = xr.createXPath(select);
        DomElement back = data;
        data = xpath.findFirst(0, data);
        while (data != null) {
            xsltChildren(de);
            data = xpath.findNext();
        }
        data = back;
    }

    /**
     * <p>Execute a value of tag.</p>
     *
     * @param de The template dom element.
     * @throws IOException IO error.
     * @throws ScannerError Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltValueOf(DomElement de)
            throws IOException, ScannerError, ValidationError {
        String select = de.getAttr(ATTR_VALUEOF_SELECT);
        String val = attrSelect(select);
        copyText(val);
    }

    /**
     * <p>Execute a with data tag.</p>
     *
     * @param de The template dom element.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltWithData(DomElement de)
            throws IOException, ScannerError, ValidationError {
        String bean = de.getAttr(ATTR_WITHDATA_BEAN);
        InterfacePath pu = resolveBean(bean);
        if ((pu.getFlags() & InterfacePath.FLAG_DIRE) != 0) {
            String select = de.getAttr(ATTR_WITHDATA_SELECT);
            String doc = attrSelect(select);
            pu.setDocument(doc);
        }
        pu.setFlags(pu.getFlags() & ~InterfacePath.FLAG_SCHM);
        pu.list();
        boolean f = pu.next();
        pu.close();
        if (!f)
            throw new IllegalArgumentException("data missing");

        DomElement back = data;
        data = pu.getFound();
        xsltChildren(de);
        data = back;
    }

    /**
     * <p>Execute an output tag.</p>
     *
     * @param de The template dom element.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    private void xsltOutput(DomElement de)
            throws IOException, ScannerError {
        String mime = de.getAttr(XSLSheetTransform.ATTR_OUTPUT_MIME);
        MimeHeader mh = new MimeHeader(mime);
        String typesubtype = mh.getType() + "/" + mh.getSubType();
        if ("text/plain".equals(typesubtype)) {
            type = TYPE_TEXT_PLAIN;
        } else {
            type = TYPE_TEXT_HTML;
        }
    }

    /**
     * <p>Execute a param tag.</p>
     *
     * @param de The template dom element.
     * @throws ScannerError Syntax error.
     */
    private void xsltParam(DomElement de)
            throws ScannerError, ValidationError {
        String name = de.getAttr(ATTR_PARAM_NAME);
        Object val = variables.get(name);
        if (val == null) {
            String use = de.getAttr(ATTR_PARAM_USE);
            boolean opflag = XSDDeclAttr.checkUse(de, use);
            if (!opflag)
                throw new IllegalArgumentException(XPathReadTransform.PATH_UNKNOWN_VARIABLE);
            return;
        }
        String type = de.getAttr(ATTR_PARAM_TYPE);
        int typeid = XSDDeclAttr.checkType(de, type);
        switch (typeid) {
            case XSDDeclAttr.TYPE_OBJECT:
                break;
            case XSDDeclAttr.TYPE_STRING:
                if (!(val instanceof String))
                    throw new ValidationError(PATH_ILLEGAL_VALUE, name);
                break;
            case XSDDeclAttr.TYPE_INTEGER:
                if (!(val instanceof Long))
                    throw new ValidationError(PATH_ILLEGAL_VALUE, name);
                break;
        }
    }

    /**
     * <p>Execute an if tag.</p>
     *
     * @param de The template dom element.
     * @throws ScannerError Syntax error.
     * @throws IOException  IO error.
     */
    private void xsltIf(DomElement de)
            throws IOException, ScannerError, ValidationError {
        String test = de.getAttr(ATTR_IF_TEST);
        boolean val = attrTest(test);
        if (val)
            xsltChildren(de);
    }

    /**
     * <p>Execute an choose tag.</p>
     *
     * @param de The template dom element.
     * @throws ScannerError Syntax error.
     * @throws IOException  IO error.
     */
    private void xsltChoose(DomElement de)
            throws IOException, ScannerError, ValidationError {
        DomNode[] nodes = de.snapshotChildren();
        for (int i = 0; i < nodes.length; i++) {
            DomNode node = nodes[i];
            DomElement de2 = (DomElement) node;
            if (de2.isName(NAME_WHEN)) {
                String test = de2.getAttr(ATTR_WHEN_TEST);
                boolean val = attrTest(test);
                if (val) {
                    xsltChildren(de2);
                    break;
                }
            } else {
                xsltChildren(de2);
                break;
            }
        }
    }

    /****************************************************************/
    /* Select Evaluation                                            */
    /****************************************************************/

    /**
     * <p>Evaluate a xselect.</p>
     *
     * @param select The xselect.
     * @return The evaluation.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     * @throws ValidationError Check error.
     */
    private String attrSelect(String select)
            throws IOException, ScannerError, ValidationError {
        XPathReadTransform xr = new XPathReadTransform();
        xr.setVariables(variables);
        XSelect xs = xr.createXSelect(select);
        return (String) xs.evalElement(data);
    }

    /**
     * <p>Evaluate a xpath expr.</p>
     *
     * @param test The xpath expr.
     * @return The evaluation.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     * @throws ValidationError Check error.
     */
    private boolean attrTest(String test)
            throws IOException, ScannerError, ValidationError {
        XPathReadTransform xr = new XPathReadTransform();
        xr.setVariables(variables);
        XPathExpr xe = xr.createXPathExpr(test);
        return xe.checkElement(data);
    }

    /****************************************************************/
    /* Tag Copying                                                  */
    /****************************************************************/

    /**
     * <p>Copy text.</p>
     *
     * @param data The text.
     */
    private void copyText(String data) throws IOException {
        switch (type) {
            case TYPE_TEXT_PLAIN:
                writer.write(data);
                break;
            case TYPE_TEXT_HTML:
                writer.write(ForeignXml.sysTextEscape(data));
                break;
            default:
                throw new IllegalArgumentException("type missing");
        }
    }

    /**
     * <p>Some test cases.</p
     *
     * @param args Not used.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    /*
    public static void main(String[] args)
            throws IOException, ScannerError {
        PrintWriter pw = new PrintWriter(System.out);

        SimpleDateFormat sdf2 = new SimpleDateFormat(FormatterDateTime.PATTERN_DATETIME);
        String sent = sdf2.format(new java.util.Date());
        MapHash<String, Object> variables = new MapHash<String, Object>();
        variables.add("cust", "user7");
        variables.add("custvers", Long.valueOf(1));
        variables.add("password", "H76JK12AB");
        variables.add("adr", "user2");
        variables.add("adrvers", Long.valueOf(1));
        variables.add("date", sent);

        TemprepoPath tp = new TemprepoPath();
        tp.setDocument("0101register_GERMAN");
        tp.list();
        boolean f = tp.next();
        tp.close();
        if (!f)
            throw new IllegalArgumentException("template missing");
        DomElement template = tp.getFound();

        XSLSheetTransform transform = new XSLSheetTransform();
        transform.setVariables(variables);
        transform.setWriter(pw);
        transform.setMask(DomNode.MASK_TEXT);
        transform.xslt(template, null);
        pw.flush();

        System.out.println();
        System.out.println();
        System.out.println("--------------------------------------------");
        System.out.println();

        tp = new TemprepoPath();
        tp.setDocument("0101register_ENGLISH");
        tp.list();
        f = tp.next();
        tp.close();
        if (!f)
            throw new IllegalArgumentException("template missing");
        template = tp.getFound();

        transform = new XSLSheetTransform();
        transform.setVariables(variables);
        transform.setWriter(pw);
        transform.setMask(DomNode.MASK_TEXT);
        transform.xslt(template, null);
        pw.flush();
        System.out.println();
    }
    */

}