package matula.util.transform;

import matula.util.data.ListArray;
import matula.util.format.DomElement;
import matula.util.format.DomNode;
import matula.util.format.DomText;
import matula.util.regex.ScannerError;

import java.io.IOException;

/**
 * <p>This class provides an xml schema checker.</p>
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
public final class XMLCheck {
    public static final String ERROR_UNDECLARED_NAME = "undeclared name";
    public static final String ERROR_UNDECLARED_ATTRIBUTE = "undeclared attribute";
    public static final String ERROR_TEXT_UNEXPECTED = "text unexpected";
    public static final String ERROR_MISSING_ATTRIBUTE = "missing attribute";
    public static final String ERROR_PARENT_MISMATCH = "parent mismatch";
    public static final String ERROR_TYPE_MISMATCH = "type mismatch";

    private String context = "";
    private int mask;
    private XSDSchema schema;

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
     * <p>Check XML data.</p>
     *
     * @param node The data dom node.
     * @throws ScannerError Shit happen.
     */
    public void check(DomNode node) throws ScannerError {
        if ((mask & DomNode.MASK_LIST) != 0) {
            checkChildren((DomElement) node);
        } else {
            checkNode(node);
        }
    }

    /**
     * <p>Check XML data.</p>
     *
     * @param de The data dom element.
     * @throws ScannerError Shit happen.
     */
    private void checkChildren(DomElement de)
            throws ScannerError {
        String back = context;
        context = de.getName();
        DomNode[] nodes = de.snapshotChildren();
        for (int i = 0; i < nodes.length; i++) {
            DomNode node = nodes[i];
            checkNode(node);
        }
        context = back;
    }

    /**
     * <p>Check XML data.</p>
     *
     * @param node The data dom node.
     * @throws ScannerError Shit happen.
     */
    private void checkNode(DomNode node)
            throws ScannerError {
        if (node instanceof DomText) {
            if ((mask & DomNode.MASK_TEXT) == 0)
                throw new ScannerError(ERROR_TEXT_UNEXPECTED);
        } else if (node instanceof DomElement) {
            DomElement de = (DomElement) node;
            checkElement(de);
            checkChildren(de);
        } else {
            throw new IllegalArgumentException("illegal node");
        }
    }

    /**
     * <p>Check XML data.</p>
     *
     * @param de data dom element.
     * @throws ScannerError Shit happen.
     */
    public void checkElement(DomElement de) throws ScannerError {
        String name = de.getName();
        XSDDecl decl = schema.getDecl(name);
        if (decl == null || !(decl instanceof XSDDeclElem))
            throw new ScannerError(ERROR_UNDECLARED_NAME);
        XSDDeclElem xe = (XSDDeclElem) decl;
        String[] attrs = de.snapshotAttrs();
        for (int i = 0; i < attrs.length; i++) {
            String attr = attrs[i];
            decl = schema.getDecl(name + "." + attr);
            if (decl == null || !(decl instanceof XSDDeclAttr))
                throw new ScannerError(ERROR_UNDECLARED_ATTRIBUTE);
            XSDDeclAttr xa = (XSDDeclAttr) decl;
            checkType(de, attr, xa);
        }
        checkMandatory(de, xe);
        checkParent(xe);
    }

    /**
     * <p>Check mandatory attributes constraint.</p>
     *
     * @param de The data dom element.
     * @param xe The XSD schema element declaration.
     * @throws ScannerError Shit happens.
     */
    private static void checkMandatory(DomElement de, XSDDeclElem xe)
            throws ScannerError {
        ListArray<String> mandatory = xe.getMandatory();
        for (int i = 0; i < mandatory.size(); i++) {
            String attr = mandatory.get(i);
            if (de.getAttrObj(attr) == null)
                throw new ScannerError(ERROR_MISSING_ATTRIBUTE);
        }
    }

    /**
     * <p>Check parent element constraint.</p>
     *
     * @param xe The XSD schema element declaration.
     * @throws ScannerError Shit happens.
     */
    private void checkParent(XSDDeclElem xe)
            throws ScannerError {
        String parent = xe.getParent();
        if (parent == null)
            return;
        if (!context.equalsIgnoreCase(parent))
            throw new ScannerError(ERROR_PARENT_MISMATCH);
    }

    /**
     * <p>Check attribute value type constraint.</p>
     *
     * @param de   The data dom element.
     * @param attr The attribute name.
     * @param xa   The XSD schema attribute declaration.
     * @throws ScannerError Shit happens.
     */
    private static void checkType(DomElement de, String attr, XSDDeclAttr xa)
            throws ScannerError {
        switch (xa.getType()) {
            case XSDDeclAttr.TYPE_OBJECT:
                break;
            case XSDDeclAttr.TYPE_STRING:
                Object val = de.getAttrObj(attr);
                if (val == null)
                    break;
                if (!(val instanceof String))
                    throw new ScannerError(ERROR_TYPE_MISMATCH);
                break;
            case XSDDeclAttr.TYPE_INTEGER:
                val = de.getAttrObj(attr);
                if (val == null)
                    break;
                if (!(val instanceof Long))
                    throw new ScannerError(ERROR_TYPE_MISMATCH);
                break;
            default:
                throw new IllegalArgumentException("illegal type");
        }
    }

    /**
     * <p>Some test cases.</p
     *
     * @param args Not used.
     * @throws IOException  Shit happens.
     * @throws ScannerError Shit happens.
     */
    /*
    public static void main(String[] args) throws IOException, ScannerError {
        DomElement schema = new DomElement();
        BufferedReader reader = new BufferedReader(
                new InputStreamReader(new FileInputStream("D:\\Tablespace\\Config2\\htatab\\sheet\\cust\\package.xsd"),
                        ForeignUri.ENCODING_UTF8));
        schema.load(reader, DomNode.MASK_LIST);
        reader.close();

        XSDSchema xdef = new XSDSchema();
        xdef.digestElements(schema);

        CustPath cp = new CustPath();
        cp.setDocument("user2");
        cp.list();
        boolean f = cp.next();
        cp.close();
        if (!f)
            throw new IllegalArgumentException("data missing");
        DomElement data = cp.getFound();

        XMLCheck xc = new XMLCheck();
        xc.setMask(DomNode.MASK_LIST);
        xc.setSchema(xdef);
        xc.check(data);
        System.out.println("XML data ok");

        cp = new CustPath();
        cp.setDocument("user7");
        cp.list();
        f = cp.next();
        cp.close();
        if (!f)
            throw new IllegalArgumentException("data missing");
        data = cp.getFound();

        xc = new XMLCheck();
        xc.setMask(DomNode.MASK_LIST);
        xc.setSchema(xdef);
        xc.check(data);
        System.out.println("XML data ok");
    }
    */

}