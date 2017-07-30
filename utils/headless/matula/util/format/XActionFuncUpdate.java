package matula.util.format;

/**
 * <p>This class represents an xquery update function.</p>
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
public final class XActionFuncUpdate extends XActionFunc {
    public static final int UPDATE_NAME = 0;
    public static final int UPDATE_ATTR = 1;

    private String keyorname;
    private String value;
    private int update;

    /**
     * <p>>Create a new xquery update.</p>
     *
     * @param n The name.
     * @param u The type of update.
     */
    public XActionFuncUpdate(String n, int u) {
        if (n == null)
            throw new NullPointerException("name missing");
        keyorname = n;
        update = u;
    }

    /**
     * <p>>Create a new xquery update.</p>
     *
     * @param k The key.
     * @param v The value.
     * @param u The type of update.
     */
    public XActionFuncUpdate(String k, String v, int u) {
        if (k == null)
            throw new NullPointerException("key missing");
        if (v == null)
            throw new NullPointerException("value missing");
        keyorname = k;
        value = v;
        update = u;
    }

    /**
     * <p>Perform this xquery function on a dom element.</p>
     *
     * @param e The dom element.
     */
    void updateElement(DomElement e) {
        switch (update) {
            case UPDATE_NAME:
                e.setName(keyorname);
                break;
            case UPDATE_ATTR:
                e.setAttr(keyorname, value);
                break;
            default:
                throw new IllegalArgumentException("illegal update");
        }
    }

    /**
     * <p>Convert this xquery function to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        switch (update) {
            case UPDATE_NAME:
                return keyorname;
            case UPDATE_ATTR:
                StringBuilder buf = new StringBuilder();
                buf.append("@");
                buf.append(keyorname);
                buf.append("=\"");
                buf.append(value);
                buf.append("\"");
                return buf.toString();
            default:
                throw new IllegalArgumentException("illegal update");
        }
    }

}
