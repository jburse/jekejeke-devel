package matula.util.format;

import matula.util.data.MapEntry;
import matula.util.data.MapHashLink;

/**
 * <p>This class represents an xquery aggregate function.</p>
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
    public static final int ACTION_INSERT = 0;
    public static final int ACTION_DELETE = 1;
    public static final int ACTION_UPDATE = 2;
    public static final int ACTION_INSERT_INDEX = 3;

    private MapHashLink<String, XActionFunc> funcs = new MapHashLink<String, XActionFunc>();
    private int action;
    private int pos;

    /**
     * <p>Create a new xquery aggregate function.</p>
     *
     * @param a The type of aggegate.
     */
    XActionFuncAggr(int a) {
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
    void calcName(String n) {
        calcFunc(n, new XActionFuncUpdate(n, XActionFuncUpdate.UPDATE_NAME));
    }

    /**
     * <p>Add an element attribute action.</p>
     *
     * @param k The key.
     * @param v The value.
     */
    void calcAttr(String k, String v) {
        calcFunc(k, new XActionFuncUpdate(k, v, XActionFuncUpdate.UPDATE_ATTR));
    }

    /**
     * <p>Add an xquery function.</p>
     *
     * @param s The slot name.
     * @param f The xquery update.
     */
    void calcFunc(String s, XActionFunc f) {
        MapEntry<String, XActionFunc> entry = funcs.getEntry(s);
        if (entry != null) {
            entry.value = f;
        } else {
            funcs.put(s, f);
        }
    }

    /**
     * <p>Perform this xquery function on a dom element.</p>
     *
     * @param e The dom element.
     */
    void updateElement(DomElement e) {
        for (MapEntry<String, XActionFunc> entry = funcs.getFirstEntry();
             entry != null; entry = funcs.successor(entry)) {
            entry.value.updateElement(e);
        }
    }

    /**
     * <p>Convert this xquery function to a string.</p>
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
