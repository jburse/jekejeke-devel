package matula.util.transform;

import matula.util.data.ListArray;
import matula.util.format.DomElement;
import matula.util.regex.ScannerError;

/**
 * <p>This class provides an xml schema element declaration.</p>
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
public final class XSDDeclElem extends XSDDecl {
    static final String NAME_ELEMENT = "element";
    static final String ATTR_ELEMENT_NAME = "name";
    static final String ATTR_ELEMENT_PARENT = "parent";
    static final String ATTR_ELEMENT_COMPLEX = "complex";

    static final String SCHEMA_ILLEGAL_COMPLEX = "schema_illegal_complex";

    static final String OP_ANY = "any";
    static final String OP_EMPTY = "empty";

    private ListArray<String> mandatory;
    private String parent;
    private int complex;

    public static final int COMPLEX_NONE = 0;
    public static final int COMPLEX_EMPTY = 1; /* disable text and list */
    public static final int COMPLEX_ANY = 2; /* enable text and list */

    /**
     * <p>Retrieve the mandatory attribute names.</p>
     *
     * @return The mandatory attribute names,
     */
    ListArray<String> getMandatory() {
        return mandatory;
    }

    /**
     * <p>Set the mandatory attribute names.</p>
     *
     * @param m The mandatory attribute names.
     */
    void setMandatory(ListArray<String> m) {
        mandatory = m;
    }

    /**
     * <p>Retrieve the parent.</p>
     *
     * @return The parent.
     */
    public String getParent() {
        return parent;
    }

    /**
     * <p>Set the parent.</p>
     *
     * @param p The parent.
     */
    void setParent(String p) {
        parent = p;
    }

    /**
     * <p>Retrieve the complex type.</p>
     *
     * @return The complex type.
     */
    public int getComplex() {
        return complex;
    }

    /**
     * <p>Set the complex type.</p>
     *
     * @param c The complex type.
     */
    void setComplex(int c) {
        complex = c;
    }

    /**
     * <p>Digest an element of the XSD schema.</p>
     *
     * @param de The schema dom element.
     * @return The digested element.
     */
    static XSDDeclElem traverseElement(DomElement de) throws ScannerError {
        XSDDeclElem xe = new XSDDeclElem();
        String val = de.getAttr(ATTR_ELEMENT_PARENT);
        xe.setParent(val);
        val = de.getAttr(ATTR_ELEMENT_COMPLEX);
        xe.setComplex(checkComplex(val));
        return xe;
    }

    /**
     * <p>Check a complex attribute value.</p>
     *
     * @param complex The complex value.
     * @return The complex id.
     * @throws ScannerError Shit happens.
     */
    static int checkComplex(String complex) throws ScannerError {
        int typeid = COMPLEX_NONE;
        if (complex == null) {
            /* */
        } else if (OP_ANY.equalsIgnoreCase(complex)) {
            typeid = COMPLEX_ANY;
        } else if (OP_EMPTY.equalsIgnoreCase(complex)) {
            typeid = COMPLEX_EMPTY;
        } else {
            throw new ScannerError(SCHEMA_ILLEGAL_COMPLEX, -1);
        }
        return typeid;
    }

}