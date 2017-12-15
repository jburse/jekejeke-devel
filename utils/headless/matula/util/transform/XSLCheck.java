package matula.util.transform;

import idxtab.Temprepo.TemprepoPath;
import matula.util.data.ListArray;
import matula.util.data.MapHash;
import matula.util.format.*;
import matula.util.regex.ScannerError;
import matula.util.system.ForeignUri;
import matula.util.system.MimeHeader;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

/**
 * <p>This class provides an XSL style sheet checker.</p>
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
public final class XSLCheck {
    public static XSDSchema meta = new XSDSchema();

    private static final String ERROR_SELECT_MISSING = "select missing";
    private static final String ERROR_BEAN_MISSING = "bean missing";
    private static final String ERROR_PATH_EXPECTED = "path expected";
    private static final String ERROR_DATA_EXPECTED = "data expected";
    private static final String ERROR_DUPLICATE_PARA = "duplicate parameter";
    private static final String ERROR_UNSUPPORTED_MIME = "unsupported mime";
    private static final String ERROR_MIME_MISSING = "mime missing";
    private static final String ERROR_SELECT_FORBIDDEN = "select forbidden";
    private static final String ERROR_ILLEGAL_ELEMENT = "illegal element";
    private static final String ERROR_ILLEGAL_TEXT = "illegal text";

    private boolean type;
    private MapHash<String, Integer> parameters = new MapHash<String, Integer>();
    private int mask;
    private XSDSchema schema;
    private ListArray<String> simulation = new ListArray<String>();

    static {
        try {
            InputStream in = XSDSchema.class.getResourceAsStream("template.xsd");

            DomElement schema = new DomElement();
            BufferedReader reader = new BufferedReader(
                    new InputStreamReader(in, ForeignUri.ENCODING_UTF8));
            schema.load(reader, DomNode.MASK_LIST);
            reader.close();

            meta.digestElements(schema);
        } catch (ScannerError x) {
            throw new RuntimeException("meta failed", x);
        } catch (IOException x) {
            throw new RuntimeException("meta failed", x);
        }
    }

    /**
     * <p>Set the mask.</p>
     *
     * @param m The mask.
     */
    public void setMask(int m) {
        mask = m;
    }

    /**
     * <p>Set the XSD schema declarations.</p>
     *
     * @param s The XSD schema declarations.
     */
    public void setSchema(XSDSchema s) {
        schema = s;
    }

    /**
     * <p>Check a template.</p>
     *
     * @param node The template.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    public void check(DomNode node) throws ScannerError, IOException {
        XMLCheck xc = new XMLCheck();
        xc.setMask(DomNode.MASK_TEXT);
        xc.setSchema(meta);
        xc.check(node);
        if ((mask & DomNode.MASK_LIST) != 0) {
            xsltChildren((DomElement) node);
        } else {
            xsltNode(node);
        }
    }

    /**
     * <p>Check a template.</p>
     *
     * @param dn The template.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    private void xsltNode(DomNode dn)
            throws ScannerError, IOException {
        if (dn instanceof DomText) {
            if (!type)
                throw new ScannerError(ERROR_MIME_MISSING);
        } else {
            DomElement de = (DomElement) dn;
            if (de.isName(XSLTransform.NAME_FOREACH)) {
                xsltForEach(de);
            } else if (de.isName(XSLTransform.NAME_VALUEOF)) {
                xsltValueOf(de);
            } else if (de.isName(XSLTransform.NAME_WITHDATA)) {
                xsltWithData(de);
            } else if (de.isName(XSLTransform.NAME_OUTPUT)) {
                xsltOutput(de);
            } else if (de.isName(XSLTransform.NAME_PARAM)) {
                xsltParam(de);
            } else if (de.isName(XSLTransform.NAME_STYLESHEET)) {
                xsltChildren(de);
            } else if (de.isName(XSLTransform.NAME_IF)) {
                xsltIf(de);
            } else if (de.isName(XSLTransform.NAME_CHOOSE)) {
                xsltChoose(de);
            } else {
                xsltChildren(de);
            }
        }
    }

    /**
     * <p>Check the children.</p>
     *
     * @param de The template dom element.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    private void xsltChildren(DomElement de)
            throws ScannerError, IOException {
        DomNode[] nodes = de.snapshotChildren();
        for (int i = 0; i < nodes.length; i++) {
            DomNode node = nodes[i];
            xsltNode(node);
        }
    }

    /**
     * <p>Check a for each tag.</p>
     *
     * @param de The template dom element.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    private void xsltForEach(DomElement de)
            throws ScannerError, IOException {
        String select = de.getAttr(XSLTransform.ATTR_FOREACH_SELECT);
        XPathReadCheck xr = new XPathReadCheck();
        xr.setParameters(parameters);
        XPath xpath = xr.createXPath(select);
        ListArray<String> back = simulation;
        XPathCheck xc = new XPathCheck();
        xc.setSchema(schema);
        xc.setSimulation((ListArray<String>) simulation.clone());
        xc.xpath(xpath);
        simulation = xc.getSimulation();
        xsltChildren(de);
        simulation = back;
    }

    /**
     * <p>Check a value of tag.</p>
     *
     * @param de The template dom element.
     * @throws ScannerError Shit happens.
     */
    private void xsltValueOf(DomElement de)
            throws ScannerError, IOException {
        String select = de.getAttr(XSLTransform.ATTR_VALUEOF_SELECT);
        attrSelect(select);
        if (!type)
            throw new ScannerError(ERROR_MIME_MISSING);
    }

    /**
     * <p>Check a with data tag.</p>
     *
     * @param de The template dom element.
     * @throws ScannerError Shit happens.
     * @throws IOException  Shit happens.
     */
    private void xsltWithData(DomElement de)
            throws IOException, ScannerError {
        try {
            String bean = de.getAttr(XSLTransform.ATTR_WITHDATA_BEAN);
            if (bean == null)
                throw new ScannerError(ERROR_BEAN_MISSING);
            Class<?> _class = Class.forName(bean);
            Object obj = _class.newInstance();
            if (!(obj instanceof InterfacePath))
                throw new ScannerError(ERROR_PATH_EXPECTED);
            InterfacePath pu = (InterfacePath) obj;
            if ((pu.getFlags() & InterfacePath.FLAG_STYL) != 0)
                throw new ScannerError(ERROR_DATA_EXPECTED);
            if ((pu.getFlags() & InterfacePath.FLAG_DIRE) != 0) {
                String select = de.getAttr(XSLTransform.ATTR_WITHDATA_SELECT);
                if (select == null)
                    throw new ScannerError(ERROR_SELECT_MISSING);
                attrSelect(select);
            } else {
                String select = de.getAttr(XSLTransform.ATTR_WITHDATA_SELECT);
                if (select != null)
                    throw new ScannerError(ERROR_SELECT_FORBIDDEN);
            }
            pu.setFlags(pu.getFlags() | InterfacePath.FLAG_SCHM);
            pu.list();
            boolean f = pu.next();
            pu.close();
            if (!f)
                throw new IllegalArgumentException("schema missing");

            XSDSchema xdef = new XSDSchema();
            xdef.digestElements(pu.getFound());

            ListArray<String> back = simulation;
            XSDSchema back2 = schema;
            simulation = new ListArray<String>();
            schema = xdef;
            xsltChildren(de);
            schema = back2;
            simulation = back;
        } catch (IllegalAccessException x) {
            throw new ScannerError(XSLTransform.ERROR_ILLEGAL_ACCESS);
        } catch (InstantiationException x) {
            throw new ScannerError(XSLTransform.ERROR_INSTANTIATION_EXCEPTION);
        } catch (ClassNotFoundException x) {
            throw new ScannerError(XSLTransform.ERROR_CLASS_MISSING);
        }
    }

    /**
     * <p>Check an output tag.</p>
     *
     * @param de The template dom element.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    private void xsltOutput(DomElement de)
            throws IOException, ScannerError {
        String mime = de.getAttr(XSLTransform.ATTR_OUTPUT_MIME);
        MimeHeader mh = new MimeHeader(mime);
        String typesubtype = mh.getType() + "/" + mh.getSubType();
        if ("text/plain".equals(typesubtype)) {
            type = true;
        } else if ("text/html".equals(typesubtype)) {
            type = true;
        } else {
            throw new ScannerError(ERROR_UNSUPPORTED_MIME);
        }
    }

    /**
     * <p>Check a param tag.</p>
     *
     * @param de The template dom element.
     * @throws ScannerError Shit happens.
     */
    private void xsltParam(DomElement de) throws ScannerError {
        String name = de.getAttr(XSLTransform.ATTR_PARAM_NAME);
        if (parameters.get(name) != null)
            throw new ScannerError(ERROR_DUPLICATE_PARA);
        String type = de.getAttr(XSLTransform.ATTR_PARAM_TYPE);
        int typeid = XSDDeclAttr.checkType(type);
        parameters.add(name, Integer.valueOf(typeid));
    }

    /**
     * <p>Check a if tag.</p>
     *
     * @param de The template dom element.
     * @throws ScannerError Shit happens.
     * @throws IOException  Shit happens.
     */
    private void xsltIf(DomElement de)
            throws ScannerError, IOException {
        String test = de.getAttr(XSLTransform.ATTR_IF_TEST);
        attrTest(test);
        xsltChildren(de);
    }

    /**
     * <p>Check a choose tag.</p>
     *
     * @param de The template dom element.
     * @throws ScannerError Shit happens.
     * @throws IOException  Shit happens.
     */
    private void xsltChoose(DomElement de)
            throws ScannerError, IOException {
        DomNode[] nodes = de.snapshotChildren();
        for (int i = 0; i < nodes.length; i++) {
            DomNode node = nodes[i];
            if (node instanceof DomText)
                throw new ScannerError(ERROR_ILLEGAL_TEXT);
            DomElement de2 = (DomElement) node;
            if (de2.isName(XSLTransform.NAME_WHEN)) {
                String test = de2.getAttr(XSLTransform.ATTR_WHEN_TEST);
                attrTest(test);
                xsltChildren(de2);
            } else if (de2.isName(XSLTransform.NAME_OTHERWISE)) {
                xsltChildren(de2);
            } else {
                throw new ScannerError(ERROR_ILLEGAL_ELEMENT);
            }
        }
    }

    /****************************************************************/
    /* Some Checks                                                  */
    /****************************************************************/

    /**
     * <p>Check a xselect.</p>
     *
     * @param select The xselect.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    private void attrSelect(String select)
            throws IOException, ScannerError {
        XPathReadCheck xr = new XPathReadCheck();
        xr.setParameters(parameters);
        XSelect xs = xr.createXSelect(select);
        XPathCheck xc = new XPathCheck();
        xc.setSchema(schema);
        xc.setSimulation(simulation);
        xc.select(xs);
    }

    /**
     * <p>Check a xpath expr.</p>
     *
     * @param test The xpath expr.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    private void attrTest(String test)
            throws IOException, ScannerError {
        XPathReadCheck xr = new XPathReadCheck();
        xr.setParameters(parameters);
        XPathExpr xe = xr.createXPathExpr(test);
        XPathCheck xc = new XPathCheck();
        xc.setSchema(schema);
        xc.setSimulation(simulation);
        xc.predicate(xe);
    }

    /**
     * <p>Some test cases.</p
     *
     * @param args Not used.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    public static void main(String[] args) throws IOException, ScannerError, ClassNotFoundException {
        TemprepoPath tp = new TemprepoPath();
        tp.setDocument("0101register_GERMAN");
        tp.list();
        boolean f = tp.next();
        tp.close();
        if (!f)
            throw new IllegalArgumentException("template missing");
        DomElement template = tp.getFound();

        XSLCheck xc = new XSLCheck();
        xc.setMask(DomNode.MASK_TEXT);
        xc.check(template);
        System.out.println("XSL template GERMAN ok");

        tp = new TemprepoPath();
        tp.setDocument("0101register_ENGLISH");
        tp.list();
        f = tp.next();
        tp.close();
        if (!f)
            throw new IllegalArgumentException("template missing");
        template = tp.getFound();

        xc = new XSLCheck();
        xc.setMask(DomNode.MASK_TEXT);
        xc.check(template);
        System.out.println("XSL template ENGLISH ok");
    }

}