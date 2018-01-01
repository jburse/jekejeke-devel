package matula.util.format;

import matula.util.regex.ScannerError;

/**
 * <p>This class represents an xaction update function.</p>
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
    public static final int UPDATE_CHILD = 2;

    private String keyorname;
    private XSelect value;
    private int update;

    /**
     * <p>>Create a new xaction update.</p>
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
     * <p>>Create a new xaction update.</p>
     *
     * @param k The key.
     * @param v The value.
     * @param u The type of update.
     */
    public XActionFuncUpdate(String k, XSelect v, int u) {
        if (k == null)
            throw new NullPointerException("key missing");
        if (v == null)
            throw new NullPointerException("value missing");
        keyorname = k;
        value = v;
        update = u;
    }

    /**
     * <p>Perform this xaction function on a dom element.</p>
     *
     * @param r The target dom element.
     * @param e The source dom element.
     * @throws ScannerError         Shit happens.
     * @throws InterruptedException Transaction was interrupted.
     */
    public void updateElement(DomElement r, DomElement e)
            throws ScannerError, InterruptedException {
        switch (update) {
            case UPDATE_NAME:
                r.setName(keyorname);
                break;
            case UPDATE_ATTR:
                Object val = value.evalElement(e);
                r.setAttrObj(keyorname, val);
                break;
            case UPDATE_CHILD:
                val = value.evalElement(e);
                val = ((DomElement) val).clone();
                r.setChild(keyorname, (DomElement) val);
                break;
            default:
                throw new IllegalArgumentException("illegal update");
        }
    }

    /**
     * <p>Convert this xaction function to a string.</p>
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
                buf.append("=");
                buf.append(value.toString());
                return buf.toString();
            case UPDATE_CHILD:
                buf = new StringBuilder();
                buf.append(keyorname);
                buf.append("=");
                buf.append(value.toString());
                return buf.toString();
            default:
                throw new IllegalArgumentException("illegal update");
        }
    }

}
