package matula.util.transform;

import matula.util.data.ListArray;
import matula.util.data.MapHash;
import matula.util.format.*;
import matula.util.regex.ScannerError;

import java.io.IOException;

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
 * Restrictions
 * Only to be distributed with programs that add significant and primary
 * functionality to the library. Not to be distributed with additional
 * software intended to replace any components of the library.
 * <p/>
 * Trademarks
 * Jekejeke is a registered trademark of XLOG Technologies GmbH.
 */
public final class XSLSheetCheck extends XSLSheet {

    private static final String SHEET_FORBIDDEN_TEXT = "sheet_forbidden_text";
    private static final String SHEET_FORBIDDEN_ELEM = "sheet_forbidden_elem";
    private static final String SHEET_FORBIDDEN_ATTR = "sheet_forbidden_attr";
    private static final String SHEET_MISSING_ATTR = "sheet_missing_attr";
    private static final String SHEET_MISMATCHED_PATH = "sheet_mismatched_path";

    private MapHash<String, Integer> parameters = new MapHash<String, Integer>();
    private int mask;
    private XSDSchema schema;
    private ListArray<String> simulation = new ListArray<String>();

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
            AbstractDom[] nodes = ((DomElement) node).snapshotNodes();
            xsltNodes(nodes);
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
            /* */
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
                AbstractDom[] nodes = de.snapshotNodes();
                xsltNodes(nodes);
            }
        }
    }

    /**
     * <p>Check the children.</p>
     *
     * @param nodes The dom elements.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltNodes(AbstractDom[] nodes)
            throws IOException, ScannerError, ValidationError {
        for (int i = 0; i < nodes.length; i++) {
            AbstractDom node = nodes[i];
            xsltNode(node);
        }
    }

    /************************************************************/
    /* for-each & with-data                                     */
    /************************************************************/

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
        String attr = de.getAttr(XSLSheet.ATTR_FOREACH_SELECT);
        XPathReadCheck xr = new XPathReadCheck();
        xr.setMeta(meta);
        xr.setParameters(parameters);
        XPath xpath = xr.createXPath(attr);
        XPathCheck xc = new XPathCheck();
        xc.setSchema(schema);
        xc.setSimulation((ListArray<String>) simulation.clone());
        xc.xpath(xpath);
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
            xselect.checkElement(xc);
            attr = elem.getAttr(XSLSheet.ATTR_SORT_ORDER);
            XSLSheet.checkOrder(elem, attr);
        }
        ListArray<String> backsimulation = simulation;
        try {
            simulation = xc.getSimulation();
            xsltNodes(nodes);
            simulation = backsimulation;
        } catch (IOException x) {
            simulation = backsimulation;
            throw x;
        } catch (ScannerError x) {
            simulation = backsimulation;
            throw x;
        } catch (ValidationError x) {
            simulation = backsimulation;
            throw x;
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
        String bean = de.getAttr(XSLSheet.ATTR_WITHDATA_BEAN);

        XSDResolver resolver = schema.getResolver();
        Class<?> _class = XSDResolver.findClass(bean);
        XSDSchema xdef = resolver.resolveSchema(_class);

        if ((xdef.getFlags() & InterfacePath.FLAG_STYL) != 0)
            throw new ValidationError(SHEET_MISMATCHED_PATH, bean);
        if ((xdef.getFlags() & InterfacePath.FLAG_DIRE) != 0) {
            String select = de.getAttr(XSLSheet.ATTR_WITHDATA_SELECT);
            if (select == null) {
                String name = de.getName();
                throw new ValidationError(SHEET_MISSING_ATTR, name + ".select");
            }
            int typeid = attrSelect(select);
            if (typeid != XSDDeclAttr.TYPE_STRING)
                throw new ValidationError(XPathCheck.PATH_STRING_SELE, select);
        } else {
            String select = de.getAttr(XSLSheet.ATTR_WITHDATA_SELECT);
            if (select != null) {
                String name = de.getName();
                throw new ValidationError(SHEET_FORBIDDEN_ATTR, name + ".select");
            }
        }

        ListArray<String> backsimulation = simulation;
        XSDSchema backschema = schema;
        try {
            simulation = new ListArray<String>();
            schema = xdef;
            AbstractDom[] nodes = de.snapshotNodes();
            xsltNodes(nodes);
            schema = backschema;
            simulation = backsimulation;
        } catch (IOException x) {
            schema = backschema;
            simulation = backsimulation;
            throw x;
        } catch (ScannerError x) {
            schema = backschema;
            simulation = backsimulation;
            throw x;
        } catch (ValidationError x) {
            schema = backschema;
            simulation = backsimulation;
            throw x;
        }
    }

    /************************************************************/
    /* value-of and copy-of                                     */
    /************************************************************/

    /**
     * <p>Check a value of tag.</p>
     *
     * @param de The template dom element.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltValueOf(DomElement de)
            throws ScannerError, ValidationError {
        String select = de.getAttr(XSLSheet.ATTR_VALUEOF_SELECT);
        attrSelect(select);
    }

    /**
     * <p>Check a copy of tag.</p>
     *
     * @param de The template dom element.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltCopyOf(DomElement de)
            throws ScannerError, ValidationError {
        String select = de.getAttr(XSLSheet.ATTR_COPYOF_SELECT);
        attrSelect(select);
    }

    /************************************************************/
    /* if & choose                                              */
    /************************************************************/

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
        String test = de.getAttr(XSLSheet.ATTR_IF_TEST);
        attrTest(test);
        AbstractDom[] nodes = de.snapshotNodes();
        xsltNodes(nodes);
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
        AbstractDom[] nodes = de.snapshotNodes();
        for (int i = 0; i < nodes.length; i++) {
            AbstractDom node = nodes[i];
            if (node instanceof DomText)
                throw new ValidationError(SHEET_FORBIDDEN_TEXT, "#text");
            DomElement de2 = (DomElement) node;
            if (de2.isName(XSLSheet.NAME_WHEN)) {
                String test = de2.getAttr(XSLSheet.ATTR_WHEN_TEST);
                attrTest(test);
                AbstractDom[] nodes2 = de2.snapshotNodes();
                xsltNodes(nodes2);
            } else if (de2.isName(XSLSheet.NAME_OTHERWISE)) {
                AbstractDom[] nodes2 = de2.snapshotNodes();
                xsltNodes(nodes2);
            } else {
                String name = de2.getName();
                throw new ValidationError(SHEET_FORBIDDEN_ELEM, name);
            }
        }
    }

    /************************************************************/
    /* stylesheet & param                                       */
    /************************************************************/

    /**
     * <p>Check an output tag.</p>
     *
     * @param de The template dom element.
     * @throws IOException     IO error.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void xsltStyleSheet(DomElement de)
            throws ValidationError, ScannerError, IOException {
        AbstractDom[] nodes = de.snapshotNodes();
        xsltNodes(nodes);
    }

    /**
     * <p>Check an output tag.</p>
     *
     * @param de The template dom element.
     * @throws ValidationError Check error.
     */
    private void xsltOutput(DomElement de)
            throws ValidationError {
        String method = de.getAttr(XSLSheet.ATTR_OUTPUT_METHOD);
        XSLSheet.checkMethod(de, method);
    }

    /**
     * <p>Check a param tag.</p>
     *
     * @param de The template dom element.
     * @throws ValidationError Check error.
     */
    private void xsltParam(DomElement de)
            throws ValidationError {
        String name = de.getAttr(XSLSheet.ATTR_PARAM_NAME);
        if (parameters.get(name) != null)
            throw new ValidationError(XPathCheck.PATH_DUPLICATE_VAR, name);
        String type = de.getAttr(XSLSheet.ATTR_PARAM_TYPE);
        int typeid = XSLSheet.checkParamType(de, type);
        parameters.add(name, Integer.valueOf(typeid));
    }

    /****************************************************************/
    /* Some Checks                                                  */
    /****************************************************************/

    /**
     * <p>Check a xselect.</p>
     *
     * @param select The xselect.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private int attrSelect(String select)
            throws ScannerError, ValidationError {
        XPathReadCheck xr = new XPathReadCheck();
        xr.setMeta(meta);
        xr.setParameters(parameters);
        XSelect xs = xr.createXSelect(select);
        XPathCheck xc = new XPathCheck();
        xc.setSchema(schema);
        xc.setSimulation(simulation);
        return xs.checkElement(xc);
    }

    /**
     * <p>Check a xpath expr.</p>
     *
     * @param test The xpath expr.
     * @throws ScannerError    Syntax error.
     * @throws ValidationError Check error.
     */
    private void attrTest(String test)
            throws ScannerError, ValidationError {
        XPathReadCheck xr = new XPathReadCheck();
        xr.setMeta(meta);
        xr.setParameters(parameters);
        XPathExpr xe = xr.createXPathExpr(test);
        XPathCheck xc = new XPathCheck();
        xc.setSchema(schema);
        xc.setSimulation(simulation);
        xe.checkElement(xc);
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