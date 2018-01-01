package matula.util.transform;

import matula.util.data.AssocArray;
import matula.util.data.ListArray;
import matula.util.format.DomElement;

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
    static final String ATTR_ELEMENT_COMPLEX = "complex";

    static final String OP_ANY = "any";
    static final String OP_EMPTY = "empty";

    static final String OP_MULTI = "multi";
    static final String OP_DET = "det";
    static final String OP_SEMIDET = "semidet";

    private ListArray<String> mandatory;
    private ListArray<String> parent;
    private int complex;
    private AssocArray<String, Integer> constraint;

    public static final int COMPLEX_NONE = 0;
    public static final int COMPLEX_EMPTY = 1; /* disable text and list */
    public static final int COMPLEX_ANY = 2; /* enable text and list */

    public static final int OCCURS_NONDET = 0;  /* mc */
    public static final int OCCURS_MULTI = 1;   /* m */
    public static final int OCCURS_DET = 2;     /* 1 */
    public static final int OCCURS_SEMIDET = 3;  /* c */

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
    public ListArray<String> getParent() {
        return parent;
    }

    /**
     * <p>Set the parent.</p>
     *
     * @param p The parent.
     */
    void setParent(ListArray<String> p) {
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
     * <p>Retrieve the constraint constraints.</p>
     *
     * @return The constraint constraints.
     */
    AssocArray<String, Integer> getConstraint() {
        return constraint;
    }

    /**
     * <p>Set the constraint constraints.</p>
     *
     * @param o The constraint constraints.
     */
    void setConstraint(AssocArray<String, Integer> o) {
        constraint = o;
    }

    /**
     * <p>Digest an element of the XSD schema.</p>
     *
     * @param de The schema dom element.
     * @return The digested element.
     * @throws ValidationError Check error.
     */
    static XSDDeclElem traverseElement(DomElement de)
            throws ValidationError {
        XSDDeclElem xe = new XSDDeclElem();
        String val = de.getAttr(ATTR_ELEMENT_COMPLEX);
        xe.setComplex(checkComplex(de,val));
        return xe;
    }

    /**
     * <p>Check a complex attribute value.</p>
     *
     * @param de The dom element.
     * @param str The complex value.
     * @return The complex id.
     * @throws ValidationError Check error.
     */
    static int checkComplex(DomElement de, String str)
            throws ValidationError {
        int complex = COMPLEX_NONE;
        if (str == null) {
            /* */
        } else if (OP_ANY.equalsIgnoreCase(str)) {
            complex = COMPLEX_ANY;
        } else if (OP_EMPTY.equalsIgnoreCase(str)) {
            complex = COMPLEX_EMPTY;
        } else {
            String name=de.getName();
            throw new ValidationError(SCHEMA_ILLEGAL_VALUE, name+".complex");
        }
        return complex;
    }

    /**
     * <p>Check a constraint attribute value.</p>
     *
     * @param de The dom element.
     * @param str The constraint value.
     * @return The constraint id.
     * @throws ValidationError Check error.
     */
    static int checkOccurs(DomElement de, String str)
            throws ValidationError {
        int occurs = OCCURS_NONDET;
        if (str == null) {
            /* */
        } else if (OP_MULTI.equalsIgnoreCase(str)) {
            occurs = OCCURS_MULTI;
        } else if (OP_DET.equalsIgnoreCase(str)) {
            occurs = OCCURS_DET;
        } else if (OP_SEMIDET.equalsIgnoreCase(str)) {
            occurs = OCCURS_SEMIDET;
        } else {
            String name=de.getName();
            throw new ValidationError(SCHEMA_ILLEGAL_VALUE, name+".occurs");
        }
        return occurs;
    }

}