package matula.util.transform;

import matula.util.format.DomElement;
import matula.util.regex.ScannerError;

/**
 * <p>This class provides an xml schema attribute declaration.</p>
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
public final class XSDDeclAttr extends XSDDecl {
    static final String NAME_ATTRIBUTE = "attribute";
    static final String ATTR_ATTRIBUTE_NAME = "name";
    static final String ATTR_ATTRIBUTE_USE = "use";
    static final String ATTR_ATTRIBUTE_TYPE = "type";

    static final String ERROR_ILLEGAL_USE = "illegal use";
    static final String ERROR_ILLEGAL_TYPE = "illegal type";

    static final String OP_OPTIONAL = "optional";
    static final String OP_STRING = "string";
    static final String OP_INTEGER = "integer";

    public static final int TYPE_OBJECT = 0;
    public static final int TYPE_STRING = 1;
    public static final int TYPE_INTEGER = 2;

    private boolean optional;
    private int type;

    /**
     * <p>Retrieve the optional flag.</p>
     *
     * @return The optional flag.
     */
    boolean getOptional() {
        return optional;
    }

    /**
     * <p>Set the optinal flag.</p>
     *
     * @param o The optional flag.
     */
    void setOptional(boolean o) {
        optional = o;
    }

    /**
     * <p>Retrieve the type.</p>
     *
     * @return The type.
     */
    public int getType() {
        return type;
    }

    /**
     * <p>Set the type.</p>
     *
     * @param t The type.
     */
    public void setType(int t) {
        type = t;
    }

    /**
     * <p>Digest an attribute of the XSD schema.</p>
     *
     * @param de The schema dom element.
     * @return The digested attribute.
     */
    static XSDDeclAttr traverseAttribute(DomElement de) throws ScannerError {
        XSDDeclAttr xa = new XSDDeclAttr();
        String val = de.getAttr(ATTR_ATTRIBUTE_USE);
        xa.setOptional(checkUse(val));
        val = de.getAttr(ATTR_ATTRIBUTE_TYPE);
        xa.setType(checkType(val));
        return xa;
    }

    /**
     * <p>Check a use attribute value.</p>
     *
     * @param use The attribute value.
     * @return The optional flag.
     * @throws ScannerError Shit happens.
     */
    public static boolean checkUse(String use) throws ScannerError {
        boolean opflag = false;
        if (use == null) {
            /* */
        } else if (OP_OPTIONAL.equalsIgnoreCase(use)) {
            opflag = true;
        } else {
            throw new ScannerError(ERROR_ILLEGAL_USE);
        }
        return opflag;
    }

    /**
     * <p>Check a type attribute value.</p>
     *
     * @param type The attribute value.
     * @return The type id.
     * @throws ScannerError Shit happens.
     */
    public static int checkType(String type) throws ScannerError {
        int typeid = TYPE_OBJECT;
        if (type == null) {
            /* */
        } else if (OP_STRING.equalsIgnoreCase(type)) {
            typeid = TYPE_STRING;
        } else if (OP_INTEGER.equalsIgnoreCase(type)) {
            typeid = TYPE_INTEGER;
        } else {
            throw new ScannerError(ERROR_ILLEGAL_TYPE);
        }
        return typeid;
    }

}