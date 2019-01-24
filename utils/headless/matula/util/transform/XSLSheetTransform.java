package matula.util.transform;

import matula.util.data.ListArray;
import matula.util.data.MapHash;
import matula.util.format.*;
import matula.util.regex.ScannerError;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class XSLSheetTransform extends XSLSheet {
    private static final String PATH_ILLEGAL_VALUE = "path_illegal_value";
    private static final String PATH_UNKNOWN_VARIABLE = "path_unknown_variable";

    private MapHash<String, Object> variables;
    private DomElement data;
    private XmlWriter dw = new XmlWriter();

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
    public XmlWriter getWriter() {
        return dw;
    }

    /**
     * <p>Set the dom writer.</p>
     *
     * @param w The dom writer.
     */
    public void setWriter(XmlWriter w) {
        dw = w;
    }

    /**
     * <p>Transform a template to a result.</p>
     *
     * @param dn      The template.
     * @param comment The comment.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    public void xslt(AbstractDom dn, String comment)
            throws IOException, ScannerError, ValidationError {
        if (comment != null && !"".equals(comment))
            dw.writeComment(comment);
        if ((dw.getMask() & AbstractDom.MASK_LIST) != 0) {
            DomElement elem = (DomElement) dn;
            AbstractDom[] nodes = elem.snapshotNodes();
            xsltNodes(nodes);
        } else {
            xsltNode(dn);
        }
    }

    /**
     * <p>Transform a template to a result.</p>
     *
     * @param dn The template.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltNode(AbstractDom dn)
            throws IOException, ScannerError, ValidationError {
        if (dn instanceof DomText) {
            DomText dt = (DomText) dn;
            dw.copyText(dt.getData());
        } else {
            DomElement de = (DomElement) dn;
            if (de.isName(XSLSheet.NAME_FOREACH)) {
                xsltForEach(de);
            } else if (de.isName(XSLSheet.NAME_SORT)) {
                /* do nothing */
            } else if (de.isName(XSLSheet.NAME_WITHDATA)) {
                xsltWithData(de);
            } else if (de.isName(XSLSheet.NAME_VALUEOF)) {
                xsltValueOf(de);
            } else if (de.isName(XSLSheet.NAME_COPYOF)) {
                xsltCopyOf(de);
            } else if (de.isName(XSLSheet.NAME_IF)) {
                xsltIf(de);
            } else if (de.isName(XSLSheet.NAME_CHOOSE)) {
                xsltChoose(de);
            } else if (de.isName(XSLSheet.NAME_STYLESHEET)) {
                xsltStyleSheet(de);
            } else if (de.isName(XSLSheet.NAME_OUTPUT)) {
                xsltOutput(de);
            } else if (de.isName(XSLSheet.NAME_PARAM)) {
                xsltParam(de);
            } else {
                int backmask = dw.getMask();
                if (((backmask & AbstractDom.MASK_TEXT) == 0 ||
                        (backmask & AbstractDom.MASK_STRP) != 0) &&
                        AbstractDom.getControl(dw.getControl(), de.getName()) == AbstractDom.TYPE_ANY) {
                    int mask = backmask;
                    mask |= AbstractDom.MASK_TEXT;
                    mask &= ~AbstractDom.MASK_STRP;
                    dw.setMask(mask);
                    try {
                        xsltNodes2(de);
                        dw.setMask(backmask);
                    } catch (IOException x) {
                        dw.setMask(backmask);
                        throw x;
                    } catch (ScannerError x) {
                        dw.setMask(backmask);
                        throw x;
                    } catch (ValidationError x) {
                        dw.setMask(backmask);
                        throw x;
                    }
                } else if (((backmask & AbstractDom.MASK_TEXT) == 0 ||
                        (backmask & AbstractDom.MASK_STRP) == 0) &&
                        AbstractDom.getControl(dw.getControl(), de.getName()) == AbstractDom.TYPE_TEXT) {
                    int mask = backmask;
                    mask |= AbstractDom.MASK_TEXT;
                    mask |= AbstractDom.MASK_STRP;
                    dw.setMask(mask);
                    try {
                        xsltNodes2(de);
                        dw.setMask(backmask);
                    } catch (IOException x) {
                        dw.setMask(backmask);
                        throw x;
                    } catch (ScannerError x) {
                        dw.setMask(backmask);
                        throw x;
                    } catch (ValidationError x) {
                        dw.setMask(backmask);
                        throw x;
                    }
                } else {
                    xsltNodes2(de);
                }
            }
        }
    }

    /**
     * <p>Transform the children.</p>
     *
     * @param de The template DOM elememt.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltNodes2(DomElement de)
            throws IOException, ScannerError, ValidationError {
        boolean lastspace = ((dw.getMask() & AbstractDom.MASK_LTSP) != 0);
        AbstractDom[] nodes = de.snapshotNodes();
        if (nodes.length == 0 &&
                (AbstractDom.getControl(dw.getControl(), de.getName()) == AbstractDom.TYPE_EMPTY)) {
            dw.copyStart(de);
        } else if (nodes.length != 0 ||
                (dw.getMask() & AbstractDom.MASK_TEXT) != 0) {
            dw.copyStart(de);
            if ((dw.getMask() & AbstractDom.MASK_TEXT) != 0) {
                xsltNodes(nodes);
            } else {
                dw.write("\n");
                dw.incIndent();
                xsltNodes(nodes);
                dw.decIndent();
                dw.writeIndent();
            }
            dw.copyEnd(de);
        } else {
            dw.copyEmpty(de);
        }
        if (lastspace) {
            dw.setMask(dw.getMask() | AbstractDom.MASK_LTSP);
        } else {
            dw.setMask(dw.getMask() & ~AbstractDom.MASK_LTSP);
        }
    }

    /**
     * <p>Transform the children.</p>
     *
     * @param nodes The template dom nodes.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltNodes(AbstractDom[] nodes)
            throws IOException, ScannerError, ValidationError {
        for (int i = 0; i < nodes.length; i++) {
            AbstractDom node = nodes[i];
            if ((dw.getMask() & AbstractDom.MASK_TEXT) != 0) {
                xsltNode(node);
            } else {
                dw.writeIndent();
                xsltNode(node);
                dw.write("\n");
            }
        }
    }

    /************************************************************/
    /* for-each & with-data                                     */
    /************************************************************/

    /**
     * <p>Execute a for each tag.</p>
     *
     * @param de The template dom element.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltForEach(DomElement de)
            throws IOException, ScannerError, ValidationError {
        String attr = de.getAttr(XSLSheet.ATTR_FOREACH_SELECT);
        XPathReadTransform xr = new XPathReadTransform();
        xr.setMeta(meta);
        xr.setVariables(variables);
        XPath xpath = xr.createXPath(attr);
        AbstractDom[] nodes = de.snapshotNodes();
        for (int i = 0; i < nodes.length; i++) {
            AbstractDom node = nodes[i];
            if (!(node instanceof DomElement))
                continue;
            DomElement elem = (DomElement) node;
            if (!elem.isName(XSLSheet.NAME_SORT))
                continue;
            attr = elem.getAttr(XSLSheet.ATTR_SORT_SELECT);
            XSelect xselect = xr.createXSelect(attr);
            attr = elem.getAttr(XSLSheet.ATTR_SORT_ORDER);
            int orderid = XSLSheet.checkOrder(elem, attr);
            XPathOrder xo = new XPathOrder(xselect, orderid);
            xpath.sortOrder(Integer.toString(i), xo);
        }
        DomElement backdata = data;
        try {
            if (xpath.getOrderBys() != null) {
                ListArray<DomElement> list = xpath.findSort(data);
                for (int j = 0; j < list.size(); j++) {
                    data = list.get(j);
                    xsltNodes(nodes);
                }
            } else {
                data = xpath.findFirst(0, data);
                while (data != null) {
                    xsltNodes(nodes);
                    data = xpath.findNext();
                }
            }
            data = backdata;
        } catch (IOException x) {
            data = backdata;
            throw x;
        } catch (ScannerError x) {
            data = backdata;
            throw x;
        } catch (ValidationError x) {
            data = backdata;
            throw x;
        }
    }

    /**
     * <p>Execute a with data tag.</p>
     *
     * @param de The template dom element.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltWithData(DomElement de)
            throws IOException, ScannerError, ValidationError {
        String bean = de.getAttr(XSLSheet.ATTR_WITHDATA_BEAN);
        Class<?> _class = XSDResolver.findClass(bean);
        InterfacePath pu = XSDResolver.newPath(_class);
        if ((pu.getFlags() & InterfacePath.FLAG_DIRE) != 0) {
            String select = de.getAttr(XSLSheet.ATTR_WITHDATA_SELECT);
            String doc = (String) attrSelect(select);
            pu.setDocument(doc);
        }
        pu.setFlags(pu.getFlags() & ~InterfacePath.FLAG_SCHM);
        pu.list();
        boolean f = pu.next();
        pu.close();
        if (!f)
            throw new IllegalArgumentException("data missing");

        DomElement backdata = data;
        try {
            data = pu.getFound();
            AbstractDom[] nodes = de.snapshotNodes();
            xsltNodes(nodes);
            data = backdata;
        } catch (IOException x) {
            data = backdata;
            throw x;
        } catch (ScannerError x) {
            data = backdata;
            throw x;
        } catch (ValidationError x) {
            data = backdata;
            throw x;
        }
    }

    /************************************************************/
    /* value-of and copy-of                                     */
    /************************************************************/

    /**
     * <p>Execute a value of tag.</p>
     *
     * @param de The template dom element.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    private void xsltValueOf(DomElement de)
            throws IOException, ScannerError {
        String select = de.getAttr(XSLSheet.ATTR_VALUEOF_SELECT);
        int backmask = dw.getMask();
        if ((backmask & AbstractDom.MASK_PLIN) == 0) {
            int mask = backmask;
            mask |= AbstractDom.MASK_PLIN;
            dw.setMask(mask);
            try {
                xsltSendData(select);
                dw.setMask(backmask);
            } catch (IOException x) {
                dw.setMask(backmask);
                throw x;
            } catch (ScannerError x) {
                dw.setMask(backmask);
                throw x;
            }
        } else {
            xsltSendData(select);
        }
    }

    /**
     * <p>Execute a copy of tag.</p>
     *
     * @param de The template dom element.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    private void xsltCopyOf(DomElement de)
            throws IOException, ScannerError {
        String select = de.getAttr(XSLSheet.ATTR_COPYOF_SELECT);
        int backmask = dw.getMask();
        if ((backmask & AbstractDom.MASK_PLIN) != 0) {
            int mask = backmask;
            mask &= ~AbstractDom.MASK_PLIN;
            dw.setMask(mask);
            try {
                xsltSendData(select);
                dw.setMask(backmask);
            } catch (IOException x) {
                dw.setMask(backmask);
                throw x;
            } catch (ScannerError x) {
                dw.setMask(backmask);
                throw x;
            }
        } else {
            xsltSendData(select);
        }
    }

    /**
     * <p>Send the data to the output.</p>
     *
     * @param select The data select.
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     */
    private void xsltSendData(String select)
            throws IOException, ScannerError {
        Object val = attrSelect(select);
        if (val == null)
            return;
        if (val instanceof DomElement) {
            AbstractDom[] nodes = ((DomElement) val).snapshotNodes();
            dw.storeNodes(nodes);
        } else {
            if (val instanceof String) {
                dw.copyText((String) val);
            } else {
                dw.write(Long.toString(((Long) val).longValue()));
            }
        }
    }

    /************************************************************/
    /* if & choose                                              */
    /************************************************************/

    /**
     * <p>Execute an if tag.</p>
     *
     * @param de The template dom element.
     * @throws ScannerError Syntax error.
     * @throws IOException  IO error.
     */
    private void xsltIf(DomElement de)
            throws IOException, ScannerError, ValidationError {
        String test = de.getAttr(XSLSheet.ATTR_IF_TEST);
        boolean val = attrTest(test);
        if (val) {
            AbstractDom[] nodes = de.snapshotNodes();
            xsltNodes(nodes);
        }
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
        AbstractDom[] nodes = de.snapshotNodes();
        for (int i = 0; i < nodes.length; i++) {
            AbstractDom node = nodes[i];
            DomElement de2 = (DomElement) node;
            if (de2.isName(XSLSheet.NAME_WHEN)) {
                String test = de2.getAttr(XSLSheet.ATTR_WHEN_TEST);
                boolean val = attrTest(test);
                if (val) {
                    AbstractDom[] nodes2 = de2.snapshotNodes();
                    xsltNodes(nodes2);
                    break;
                }
            } else {
                AbstractDom[] nodes2 = de2.snapshotNodes();
                xsltNodes(nodes2);
                break;
            }
        }
    }

    /************************************************************/
    /* stylesheet & param                                       */
    /************************************************************/

    /**
     * <p>Execute an output tag.</p>
     *
     * @param de The template dom element.
     * @throws ValidationError Check error.
     * @throws ScannerError    Syntax error.
     * @throws IOException     IO error.
     */
    private void xsltStyleSheet(DomElement de)
            throws ValidationError, ScannerError, IOException {
        AbstractDom[] nodes = de.snapshotNodes();
        xsltNodes(nodes);
    }

    /**
     * <p>Execute an output tag.</p>
     *
     * @param de The template dom element.
     * @throws ValidationError Check error.
     */
    private void xsltOutput(DomElement de)
            throws ValidationError {
        String method = de.getAttr(XSLSheet.ATTR_OUTPUT_METHOD);
        int methodid = XSLSheet.checkMethod(de, method);
        switch (methodid) {
            case XSLSheet.METHOD_TEXT:
                dw.setMask(dw.getMask() | AbstractDom.MASK_PLIN);
                break;
            case XSLSheet.METHOD_HTML:
                dw.setMask(dw.getMask() & ~AbstractDom.MASK_PLIN);
                break;
            default:
                throw new IllegalArgumentException("illegal method");
        }
    }

    /**
     * <p>Execute a param tag.</p>
     *
     * @param de The template dom element.
     * @throws ValidationError Check error.
     */
    private void xsltParam(DomElement de)
            throws ValidationError {
        String name = de.getAttr(XSLSheet.ATTR_PARAM_NAME);
        Object val = variables.get(name);
        if (val == null) {
            String use = de.getAttr(XSLSheet.ATTR_PARAM_USE);
            boolean opflag = XSDDeclAttr.checkUse(de, use);
            if (!opflag)
                throw new IllegalArgumentException(PATH_UNKNOWN_VARIABLE);
            return;
        }
        String type = de.getAttr(XSLSheet.ATTR_PARAM_TYPE);
        int typeid = XSLSheet.checkParamType(de, type);
        switch (typeid) {
            case XSDDeclAttr.TYPE_PRIMITIVE:
                if (!(val instanceof String) && !(val instanceof Long))
                    throw new ValidationError(PATH_ILLEGAL_VALUE, name);
                break;
            case XSDDeclAttr.TYPE_STRING:
                if (!(val instanceof String))
                    throw new ValidationError(PATH_ILLEGAL_VALUE, name);
                break;
            case XSDDeclAttr.TYPE_INTEGER:
                if (!(val instanceof Long))
                    throw new ValidationError(PATH_ILLEGAL_VALUE, name);
                break;
            case XSLSheet.TYPE_ELEMENT:
                if (!(val instanceof DomElement))
                    throw new ValidationError(PATH_ILLEGAL_VALUE, name);
                break;
            default:
                throw new IllegalArgumentException("illegal type");
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
     * @throws ScannerError Syntax error.
     */
    private Object attrSelect(String select)
            throws ScannerError {
        XPathReadTransform xr = new XPathReadTransform();
        xr.setMeta(meta);
        xr.setVariables(variables);
        XSelect xs = xr.createXSelect(select);
        return xs.evalElement(data);
    }

    /**
     * <p>Evaluate a xpath expr.</p>
     *
     * @param test The xpath expr.
     * @return The evaluation.
     * @throws ScannerError Syntax error.
     */
    private boolean attrTest(String test)
            throws ScannerError {
        XPathReadTransform xr = new XPathReadTransform();
        xr.setMeta(meta);
        xr.setVariables(variables);
        XPathExpr xe = xr.createXPathExpr(test);
        return xe.evalElement(data);
    }

    /**
     * <p>Some test cases.</p
     *
     * @param args Not used.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    /*
    public static void main(String[] args)
            throws IOException, ScannerError, ValidationError {
        PrintWriter pw = new PrintWriter(System.out);
        AbstractWriter dw = new AbstractWriter();
        dw.setMask(AbstractDom.MASK_TEXT);
        dw.setWriter(pw);

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
        transform.setWriter(dw);

        transform.xslt(template, null);
        dw.flush();

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
        transform.setWriter(dw);
        transform.xslt(template, null);
        dw.flush();
        System.out.println();
    }
    */

}