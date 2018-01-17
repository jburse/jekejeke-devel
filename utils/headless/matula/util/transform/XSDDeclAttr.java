package matula.util.transform;

import matula.util.format.DomElement;

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

    static final String OP_OPTIONAL = "optional";
    static final String OP_STRING = "string";
    static final String OP_INTEGER = "integer";
    static final String OP_FLOAT = "float";
    static final String OP_TIMESTAMP = "timestamp";

    public static final int TYPE_OBJECT = 0;
    public static final int TYPE_STRING = 1;
    public static final int TYPE_INTEGER = 2;
    public static final int TYPE_FLOAT = 3;
    public static final int TYPE_TIMESTAMP = 4;

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
    private void setOptional(boolean o) {
        optional = o;
    }

    /**
     * <p>Retrieve the type.</p>
     *
     * @return The type.
     */
    int getType() {
        return type;
    }

    /**
     * <p>Set the type.</p>
     *
     * @param t The type.
     */
    private void setType(int t) {
        type = t;
    }

    /**
     * <p>Digest an attribute of the XSD schema.</p>
     *
     * @param de The schema dom element.
     * @return The digested attribute.
     * @throws ValidationError Check error.
     */
    static XSDDeclAttr traverseAttribute(DomElement de)
            throws ValidationError {
        XSDDeclAttr xa = new XSDDeclAttr();
        String val = de.getAttr(ATTR_ATTRIBUTE_USE);
        xa.setOptional(checkUse(de, val));
        val = de.getAttr(ATTR_ATTRIBUTE_TYPE);
        xa.setType(checkAttrType(de, val));
        return xa;
    }

    /**
     * <p>Check a use attribute value.</p>
     *
     * @param de  The dom element.
     * @param use The attribute value.
     * @return The optional flag.
     * @throws ValidationError Check error.
     */
    public static boolean checkUse(DomElement de, String use)
            throws ValidationError {
        boolean opflag = false;
        if (use == null) {
            /* */
        } else if (OP_OPTIONAL.equalsIgnoreCase(use)) {
            opflag = true;
        } else {
            String name = de.getName();
            throw new ValidationError(SCHEMA_ILLEGAL_VALUE, name + ".use");
        }
        return opflag;
    }

    /**
     * <p>Check a type attribute value.</p>
     *
     * @param de   The dom element.
     * @param type The attribute value.
     * @return The type id.
     * @throws ValidationError Check error.
     */
    public static int checkAttrType(DomElement de, String type)
            throws ValidationError {
        int typeid = TYPE_OBJECT;
        if (type == null) {
            /* */
        } else if (OP_STRING.equalsIgnoreCase(type)) {
            typeid = TYPE_STRING;
        } else if (OP_INTEGER.equalsIgnoreCase(type)) {
            typeid = TYPE_INTEGER;
        } else if (OP_FLOAT.equalsIgnoreCase(type)) {
            typeid = TYPE_FLOAT;
        } else if (OP_INTEGER.equalsIgnoreCase(type)) {
            typeid = TYPE_TIMESTAMP;
        } else {
            String name = de.getName();
            throw new ValidationError(SCHEMA_ILLEGAL_VALUE, name + ".type");
        }
        return typeid;
    }

}