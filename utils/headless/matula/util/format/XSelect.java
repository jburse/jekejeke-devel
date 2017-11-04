package matula.util.format;

import matula.util.regex.ScannerError;

/**
 * <p>The class represent an xpath select.</p>
 * <p/>
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
public final class XSelect {
    public static final int SELECT_ATTR = 0;
    public static final int SELECT_CONST = 1;

    private String attrorcnst;
    private int select;

    /**
     * <p>>Create a new xpath select.</p>
     *
     * @param a The attribute or variable.
     * @param s The type of select.
     */
    public XSelect(String a, int s) {
        if (a == null)
            throw new NullPointerException("attribute or const missing");
        attrorcnst = a;
        select = s;
    }

    /**
     * <p>Retrieve the type of select.</p>
     *
     * @return The type of select.
     */
    public int getSelect() {
        return select;
    }

    /**
     * <p>Retrieve the attribute or variable.</p>
     *
     * @return The attribute or variable.
     */
    public String getAttrOrCnst() {
        return attrorcnst;
    }

    /**
     * <p>Eval an xpath select.</p>
     *
     * @param d The dom element.
     * @return The value.
     * @throws ScannerError Shit happens.
     */
    public String eval(DomElement d) throws ScannerError {
        String res;
        switch (getSelect()) {
            case XSelect.SELECT_ATTR:
                res = d.getAttr(getAttrOrCnst());
                if (res == null)
                    throw new ScannerError(XPathExprPrim.ERROR_UNKNOWN_ATTRIBUTE);
                break;
            case XSelect.SELECT_CONST:
                res = getAttrOrCnst();
                break;
            default:
                throw new IllegalArgumentException("illegal select");
        }
        return res;
    }

    /**
     * <p>Convert this xpath select to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        switch (select) {
            case SELECT_ATTR:
                StringBuilder buf = new StringBuilder();
                buf.append("@");
                buf.append(attrorcnst);
                return buf.toString();
            case SELECT_CONST:
                buf = new StringBuilder();
                buf.append("\'");
                buf.append(attrorcnst);
                buf.append("\'");
                return buf.toString();
            default:
                throw new IllegalArgumentException("illegal select");
        }
    }

}