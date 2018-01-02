package matula.util.transform;

import matula.util.data.AssocArray;
import matula.util.data.ListArray;
import matula.util.format.AbstractDom;
import matula.util.format.DomElement;
import matula.util.format.DomText;

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
    public static final String DATA_UNEXPECTED_TEXT = "data_unexpected_text";
    public static final String DATA_MISSING_ATTR = "data_missing_attr";
    public static final String DATA_MISSING_ELEM = "data_missing_elem";
    public static final String DATA_FORBIDDEN_ELEM = "data_forbidden_elem";
    public static final String DATA_ILLEGAL_PARENT = "data_illegal_parent";
    public static final String DATA_ILLEGAL_VALUE = "data_illegal_value";

    private String context = "";
    private int mask;
    private XSDSchema schema;

    /**
     * <p>Set the context.</p>
     *
     * @param c The context.
     */
    public void setContext(String c) {
        context = c;
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
     * <p>Check XML data.</p>
     *
     * @param node The data dom node.
     * @throws ValidationError Check error.
     */
    public void check(AbstractDom node) throws ValidationError {
        if ((mask & AbstractDom.MASK_LIST) != 0) {
            DomElement de = (DomElement) node;
            XSDDeclElem xe = schema.getDeclElem(de.getName());
            checkNodes(de, xe);
        } else {
            checkNode(node);
        }
    }

    /**
     * <p>Check XML data.</p>
     *
     * @param node The data dom node.
     * @throws ValidationError Check error.
     */
    private void checkNode(AbstractDom node)
            throws ValidationError {
        if (node instanceof DomText) {
            if ((mask & AbstractDom.MASK_TEXT) == 0)
                throw new ValidationError(DATA_UNEXPECTED_TEXT, "#text");
        } else if (node instanceof DomElement) {
            DomElement de = (DomElement) node;
            XSDDeclElem xe = schema.getDeclElem(de.getName());
            checkAttributes(de, xe);
            if ((mask & AbstractDom.MASK_TEXT) == 0 &&
                    xe.getComplex() == XSDDeclElem.COMPLEX_ANY) {
                int backmask = mask;
                try {
                    mask |= AbstractDom.MASK_TEXT;
                    checkNodes(de, xe);
                    mask = backmask;
                } catch (ValidationError x) {
                    mask = backmask;
                    throw x;
                }
            } else {
                checkNodes(de, xe);
            }
        } else {
            throw new IllegalArgumentException("illegal node");
        }
    }


    /**********************************************************/
    /* Check Attributes                                       */
    /**********************************************************/

    /**
     * <p>Check XML data.</p>
     *
     * @param de data DOM element.
     * @param xe The XSD schema element declaration.
     * @throws ValidationError Check error.
     */
    private void checkAttributes(DomElement de, XSDDeclElem xe)
            throws ValidationError {
        String[] attrs = de.snapshotAttrs();
        for (int i = 0; i < attrs.length; i++) {
            String attr = attrs[i];
            XSDDeclAttr xa = schema.getDeclAttr(de.getName(), attr);
            checkType(de, attr, xa);
        }
        checkMandatory(de, xe);
    }

    /**
     * <p>Check attribute value type.</p>
     *
     * @param de   The data DOM element.
     * @param attr The attribute name.
     * @param xa   The XSD schema attribute declaration.
     * @throws ValidationError Check error.
     */
    private static void checkType(DomElement de, String attr, XSDDeclAttr xa)
            throws ValidationError {
        switch (xa.getType()) {
            case XSDDeclAttr.TYPE_OBJECT:
                break;
            case XSDDeclAttr.TYPE_STRING:
                Object val = de.getAttrObj(attr);
                if (val == null)
                    break;
                if (!(val instanceof String)) {
                    String name = de.getName();
                    throw new ValidationError(DATA_ILLEGAL_VALUE, name + "." + attr);
                }
                break;
            case XSDDeclAttr.TYPE_INTEGER:
                val = de.getAttrObj(attr);
                if (val == null)
                    break;
                if (!(val instanceof Long)) {
                    String name = de.getName();
                    throw new ValidationError(DATA_ILLEGAL_VALUE, name + "." + attr);
                }
                break;
            default:
                throw new IllegalArgumentException("illegal type");
        }
    }

    /**
     * <p>Check mandatory attributes.</p>
     *
     * @param de The data DOM element.
     * @param xe The XSD schema element declaration.
     * @throws ValidationError Check error.
     */
    private static void checkMandatory(DomElement de, XSDDeclElem xe)
            throws ValidationError {
        ListArray<String> mandatory = xe.getMandatory();
        if (mandatory == null)
            return;
        for (int i = 0; i < mandatory.size(); i++) {
            String attr = mandatory.get(i);
            if (de.getAttrObj(attr) == null) {
                String name = de.getName();
                throw new ValidationError(DATA_MISSING_ATTR, name + "." + attr);
            }
        }
    }

    /**********************************************************/
    /* Check Elements                                         */
    /**********************************************************/

    /**
     * <p>Check XML data.</p>
     *
     * @param de The data DOM element.
     * @param xe The XSD schema element declaration.
     * @throws ValidationError Check error.
     */
    private void checkNodes(DomElement de, XSDDeclElem xe)
            throws ValidationError {
        String name = de.getName();
        checkParent(context, name, xe);
        AbstractDom[] nodes = de.snapshotNodes();
        checkConstraint(nodes, xe);
        String backcontext = context;
        try {
            context = de.getName();
            for (int i = 0; i < nodes.length; i++) {
                AbstractDom node = nodes[i];
                checkNode(node);
            }
            context = backcontext;
        } catch (ValidationError x) {
            context = backcontext;
            throw x;
        }
    }

    /**
     * <p>Check parent element.</p>
     *
     * @param context The context.
     * @param name    The element name.
     * @param xe      The XSD schema element declaration.
     * @throws ValidationError Check error.
     */
    static void checkParent(String context, String name, XSDDeclElem xe)
            throws ValidationError {
        AssocArray<String, Integer> parent = xe.getParent();
        if (parent == null)
            return;
        for (int i = 0; i < parent.size(); i++) {
            String par = parent.getKey(i);
            if (context.equalsIgnoreCase(par))
                return;
        }
        throw new ValidationError(DATA_ILLEGAL_PARENT, name);
    }

    /**
     * <p>Check occurs constraint.</p>
     *
     * @param xe    The XSD schema element declaration.
     * @param nodes The nodes.
     * @throws ValidationError Check error.
     */
    private static void checkConstraint(AbstractDom[] nodes, XSDDeclElem xe)
            throws ValidationError {
        AssocArray<String, Integer> constraint = xe.getConstraint();
        if (constraint == null)
            return;
        for (int i = 0; i < constraint.size(); i++) {
            String child = constraint.getKey(i);
            int occurs = constraint.getValue(i).intValue();
            int count = countChild(nodes, child, 2);
            switch (occurs) {
                case XSDDeclElem.OCCURS_MULTI:
                    if (count < 1)
                        throw new ValidationError(DATA_MISSING_ELEM, child);
                    break;
                case XSDDeclElem.OCCURS_DET:
                    if (count < 1)
                        throw new ValidationError(DATA_MISSING_ELEM, child);
                    if (count > 1)
                        throw new ValidationError(DATA_FORBIDDEN_ELEM, child);
                    break;
                case XSDDeclElem.OCCURS_SEMIDET:
                    if (count > 1)
                        throw new ValidationError(DATA_FORBIDDEN_ELEM, child);
                    break;
                default:
                    throw new IllegalArgumentException("illegal occurs");
            }
        }
    }

    /**
     * <p>Returns the count.</p>
     *
     * @param nodes The DOM nodes.
     * @param key   The child name.
     * @param limit The limit of interest.
     * @return The count.
     */
    private static int countChild(AbstractDom[] nodes, String key, int limit) {
        int k = 0;
        for (int i = 0; i < nodes.length && k < limit; i++) {
            AbstractDom node = nodes[i];
            if (node instanceof DomElement &&
                    ((DomElement) node).isName(key))
                k++;
        }
        return k;
    }

    /**
     * <p>Some test cases.</p
     *
     * @param args Not used.
     * @throws IOException  Shit happens.
     * @throws ValidationError Check error.
     */
    /*
    public static void main(String[] args) throws IOException, ValidationError {
        DomElement schema = new DomElement();
        BufferedReader reader = new BufferedReader(
                new InputStreamReader(new FileInputStream("D:\\Tablespace\\Config2\\htatab\\sheet\\cust\\package.xsd"),
                        ForeignUri.ENCODING_UTF8));
        schema.load(reader, AbstractDom.MASK_LIST);
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
        xc.setMask(AbstractDom.MASK_LIST);
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
        xc.setMask(AbstractDom.MASK_LIST);
        xc.setSchema(xdef);
        xc.check(data);
        System.out.println("XML data ok");
    }
    */

}