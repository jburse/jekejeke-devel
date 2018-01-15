package matula.util.format;

import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;
import matula.util.regex.ScannerError;

/**
 * <p>This class represents an xaction aggregate function.</p>
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
public final class XActionFuncAggr extends XActionFunc {
    public static final int ACTION_DELETE = 0;
    public static final int ACTION_UPDATE = 1;
    public static final int ACTION_INSERT_INDEX = 2;

    private MapHashLink<String, XActionFunc> funcs = new MapHashLink<String, XActionFunc>();
    private int action;
    private int pos;

    /**
     * <p>Create a new xaction aggregate function.</p>
     *
     * @param a The type of aggegate.
     */
    public XActionFuncAggr(int a) {
        action = a;
    }

    /**
     * <p>Retrieve the action.</p>
     *
     * @return The action.
     */
    int getAction() {
        return action;
    }

    /**
     * <p>Set the index.</p>
     *
     * @param i The index.
     */
    void setPos(int i) {
        pos = i;
    }

    /**
     * <p>Retrieve the index.</p>
     *
     * @return The index.
     */
    int getPos() {
        return pos;
    }

    /*****************************************************/
    /* XAction Updates                                   */
    /*****************************************************/

    /**
     * <p>Add an element name action.</p>
     *
     * @param n The name.
     */
    public void calcName(String n) {
        calcFunc("_name", new XActionFuncUpdate(n, XActionFuncUpdate.UPDATE_NAME));
    }

    /**
     * <p>Add an element attribute action.</p>
     *
     * @param a The attribute name.
     * @param v The String value.
     */
    public void calcAttr(String a, String v) {
        calcAttrObj(a, v);
    }

    /**
     * <p>Add an element attribute action.</p>
     *
     * @param a The attribute name.
     * @param v The long value.
     */
    public void calcAttrLong(String a, long v) {
        calcAttrObj(a, Long.valueOf(v));
    }

    /**
     * <p>Add an element attribute action.</p>
     *
     * @param a The attribute name.
     * @param v The value.
     */
    public void calcAttrObj(String a, Object v) {
        XSelect xs = new XSelectPrim(v, XSelectPrim.SELE_PRIM_CONST);
        calcFunc(a, new XActionFuncUpdate(a, xs, XActionFuncUpdate.UPDATE_SET_ATTR));
    }

    /**
     * <p>Add an element children action.</p>
     *
     * @param n The child name.
     * @param v The elem.
     */
    public void calcChild(String n, DomElement v) {
        XSelect xs = new XSelectPrim(v, XSelectPrim.SELE_PRIM_CONST);
        calcFunc(n, new XActionFuncUpdate(n, xs, XActionFuncUpdate.UPDATE_SET_CHILD));
    }

    /**
     * <p>Delete an element attribute action.</p>
     *
     * @param a The attribute name.
     */
    public void calcDelAttr(String a) {
        calcFunc(a, new XActionFuncUpdate(a, XActionFuncUpdate.UPDATE_RESET_ATTR));
    }

    /**
     * <p>Delete an element child action.</p>
     *
     * @param n The child name.
     */
    public void calcDelChild(String n) {
        calcFunc(n, new XActionFuncUpdate(n, XActionFuncUpdate.UPDATE_RESET_CHILD));
    }

    /**
     * <p>Add an xaction function.</p>
     *
     * @param k The key.
     * @param v The xaction function.
     */
    public void calcFunc(String k, XActionFunc v) {
        MapEntry<String, XActionFunc> entry = funcs.getEntry(k);
        if (entry != null) {
            entry.value = v;
        } else {
            funcs.add(k, v);
        }
    }

    /**
     * <p>Perform this xaction function on a dom element.</p>
     *
     * @param r The target dom element.
     * @param e The source dom element.
     * @throws ScannerError         Shit happens.
     */
    public void updateElement(DomElement r, DomElement e)
            throws ScannerError {
        for (MapEntry<String, XActionFunc> entry = funcs.getFirstEntry();
             entry != null; entry = funcs.successor(entry)) {
            entry.value.updateElement(r, e);
        }
    }

    /**
     * <p>Convert this xaction function to a string.</p>
     *
     * @return The string.
     */
    public String toString() {
        StringBuilder buf = new StringBuilder();
        boolean first = true;
        for (MapEntry<String, XActionFunc> entry = funcs.getFirstEntry();
             entry != null; entry = funcs.successor(entry)) {
            if (first) {
                buf.append(entry.value.toString());
                first = false;
            } else {
                buf.append("[");
                buf.append(entry.value.toString());
                buf.append("]");
            }
        }
        return buf.toString();
    }

}
