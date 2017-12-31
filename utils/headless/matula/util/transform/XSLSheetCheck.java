package matula.util.transform;

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
public final class XSLSheetCheck extends XSLSheet {
    private static final String PATH_DUPLICATE_VAR = "path_duplicate_var";

    private static final String SHEET_ILLEGAL_VALUE = "sheet_illegal_value";
    private static final String SHEET_FORBIDDEN_TEXT = "sheet_forbidden_text";
    private static final String SHEET_FORBIDDEN_ELEM = "sheet_forbidden_elem";
    private static final String SHEET_FORBIDDEN_ATTR = "sheet_forbidden_attr";
    private static final String SHEET_MISSING_ATTR = "sheet_missing_attr";
    private static final String SHEET_MISSING_OUTPUT = "sheet_missing_output";
    private static final String SHEET_MISMATCHED_PATH = "sheet_mismatched_path";

    private static XSDSchema meta = new XSDSchema();

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
            schema.load(reader, AbstractDom.MASK_LIST);
            reader.close();

            meta.digestElements(schema);
        } catch (ScannerError x) {
            throw new RuntimeException("meta failed", x);
        } catch (ValidationError x) {
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
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    public void check(AbstractDom node)
            throws IOException, ScannerError, ValidationError {
        XMLCheck xc = new XMLCheck();
        xc.setMask(AbstractDom.MASK_TEXT);
        xc.setSchema(meta);
        xc.check(node);
        if ((mask & AbstractDom.MASK_LIST) != 0) {
            xsltChildren((DomElement) node);
        } else {
            xsltNode(node);
        }
    }

    /**
     * <p>Check a template.</p>
     *
     * @param dn The template.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltNode(AbstractDom dn)
            throws IOException, ScannerError, ValidationError {
        if (dn instanceof DomText) {
            if (!type)
                throw new ValidationError(SHEET_MISSING_OUTPUT, "#text");
        } else {
            DomElement de = (DomElement) dn;
            if (de.isName(XSLSheetTransform.NAME_FOREACH)) {
                xsltForEach(de);
            } else if (de.isName(XSLSheetTransform.NAME_VALUEOF)) {
                xsltValueOf(de);
            } else if (de.isName(XSLSheetTransform.NAME_WITHDATA)) {
                xsltWithData(de);
            } else if (de.isName(XSLSheetTransform.NAME_OUTPUT)) {
                xsltOutput(de);
            } else if (de.isName(XSLSheetTransform.NAME_PARAM)) {
                xsltParam(de);
            } else if (de.isName(XSLSheetTransform.NAME_STYLESHEET)) {
                xsltChildren(de);
            } else if (de.isName(XSLSheetTransform.NAME_IF)) {
                xsltIf(de);
            } else if (de.isName(XSLSheetTransform.NAME_CHOOSE)) {
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
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltChildren(DomElement de)
            throws IOException, ScannerError, ValidationError {
        AbstractDom[] nodes = de.snapshotChildren();
        for (int i = 0; i < nodes.length; i++) {
            AbstractDom node = nodes[i];
            xsltNode(node);
        }
    }

    /**
     * <p>Check a for each tag.</p>
     *
     * @param de The template dom element.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltForEach(DomElement de)
            throws IOException, ScannerError, ValidationError {
        String select = de.getAttr(XSLSheetTransform.ATTR_FOREACH_SELECT);
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
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltValueOf(DomElement de)
            throws IOException, ScannerError, ValidationError {
        String select = de.getAttr(XSLSheetTransform.ATTR_VALUEOF_SELECT);
        attrSelect(select);
        if (!type) {
            String name = de.getName();
            throw new ValidationError(SHEET_MISSING_OUTPUT, name);
        }
    }

    /**
     * <p>Check a with data tag.</p>
     *
     * @param de The template dom element.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltWithData(DomElement de)
            throws IOException, ScannerError, ValidationError {
        String bean = de.getAttr(XSLSheetTransform.ATTR_WITHDATA_BEAN);
        InterfacePath pu = resolveBean(bean);
        if ((pu.getFlags() & InterfacePath.FLAG_STYL) != 0)
            throw new ValidationError(SHEET_MISMATCHED_PATH, bean);
        if ((pu.getFlags() & InterfacePath.FLAG_DIRE) != 0) {
            String select = de.getAttr(XSLSheetTransform.ATTR_WITHDATA_SELECT);
            if (select == null) {
                String name = de.getName();
                throw new ValidationError(SHEET_MISSING_ATTR, name + ".select");
            }
            attrSelect(select);
        } else {
            String select = de.getAttr(XSLSheetTransform.ATTR_WITHDATA_SELECT);
            if (select != null) {
                String name = de.getName();
                throw new ValidationError(SHEET_FORBIDDEN_ATTR, name + ".select");
            }
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

    }

    /**
     * <p>Check an output tag.</p>
     *
     * @param de The template dom element.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltOutput(DomElement de)
            throws IOException, ScannerError, ValidationError {
        String mime = de.getAttr(XSLSheetTransform.ATTR_OUTPUT_MIME);
        MimeHeader mh = new MimeHeader(mime);
        String typesubtype = mh.getType() + "/" + mh.getSubType();
        if ("text/plain".equals(typesubtype)) {
            type = true;
        } else if ("text/html".equals(typesubtype)) {
            type = true;
        } else {
            String name = de.getName();
            throw new ValidationError(SHEET_ILLEGAL_VALUE, name + ".mime");
        }
    }

    /**
     * <p>Check a param tag.</p>
     *
     * @param de The template dom element.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltParam(DomElement de)
            throws ScannerError, ValidationError {
        String name = de.getAttr(XSLSheetTransform.ATTR_PARAM_NAME);
        if (parameters.get(name) != null)
            throw new ValidationError(PATH_DUPLICATE_VAR, name);
        String type = de.getAttr(XSLSheetTransform.ATTR_PARAM_TYPE);
        int typeid = XSDDeclAttr.checkType(de, type);
        parameters.add(name, Integer.valueOf(typeid));
    }

    /**
     * <p>Check a if tag.</p>
     *
     * @param de The template dom element.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltIf(DomElement de)
            throws IOException, ScannerError, ValidationError {
        String test = de.getAttr(XSLSheetTransform.ATTR_IF_TEST);
        attrTest(test);
        xsltChildren(de);
    }

    /**
     * <p>Check a choose tag.</p>
     *
     * @param de The template dom element.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltChoose(DomElement de)
            throws IOException, ScannerError, ValidationError {
        AbstractDom[] nodes = de.snapshotChildren();
        for (int i = 0; i < nodes.length; i++) {
            AbstractDom node = nodes[i];
            if (node instanceof DomText)
                throw new ValidationError(SHEET_FORBIDDEN_TEXT, "#text");
            DomElement de2 = (DomElement) node;
            if (de2.isName(XSLSheetTransform.NAME_WHEN)) {
                String test = de2.getAttr(XSLSheetTransform.ATTR_WHEN_TEST);
                attrTest(test);
                xsltChildren(de2);
            } else if (de2.isName(XSLSheetTransform.NAME_OTHERWISE)) {
                xsltChildren(de2);
            } else {
                String name = de2.getName();
                throw new ValidationError(SHEET_FORBIDDEN_ELEM, name);
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
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void attrSelect(String select)
            throws IOException, ScannerError, ValidationError {
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
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void attrTest(String test)
            throws IOException, ScannerError, ValidationError {
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
     * @throws IOException  IO error.
     * @throws ScannerError Syntax error.
     * @throws ValidationError Check error.
     */
    /*
    public static void main(String[] args)
            throws IOException, ScannerError, ValidationError {
        TemprepoPath tp = new TemprepoPath();
        tp.setDocument("0101register_GERMAN");
        tp.list();
        boolean f = tp.next();
        tp.close();
        if (!f)
            throw new IllegalArgumentException("template missing");
        DomElement template = tp.getFound();

        XSLSheetCheck xc = new XSLSheetCheck();
        xc.setMask(AbstractDom.MASK_TEXT);
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

        xc = new XSLSheetCheck();
        xc.setMask(AbstractDom.MASK_TEXT);
        xc.check(template);
        System.out.println("XSL template ENGLISH ok");
    }
    */

}